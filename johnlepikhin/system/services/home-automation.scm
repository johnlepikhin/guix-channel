;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Evgenii Lepikhin <johnlepikhin@gmail.com>
;;;
;;; This file is not part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

;;; Home automation: Home Assistant and Zigbee2MQTT in containers, talking
;;; through a local Mosquitto broker.
;;;
;;; The containers are extensions of `oci-service-type', which the host must
;;; instantiate (with podman, also `rootless-podman-service-type').  They run
;;; as root on the host network: Zigbee2MQTT needs the serial device, and on
;;; a host that also runs Docker a bridged podman network would be caught by
;;; Docker's FORWARD policy.  State lives in host directories, never in named
;;; volumes.

(define-module (johnlepikhin system services home-automation)
  #:use-module (gnu services)
  #:use-module (gnu services containers)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (mosquitto-local-configuration-file

            zigbee2mqtt-configuration
            zigbee2mqtt-configuration?
            zigbee2mqtt-service-type

            home-assistant-configuration
            home-assistant-configuration?
            home-assistant-service-type))


;;;
;;; Mosquitto.
;;;

(define* (mosquitto-local-configuration-file #:key (port 1883))
  "Return a Mosquitto configuration file for a broker that only clients on
this host can reach: it listens on the loopback interface and needs no
credentials.  Retained messages are not persisted; Zigbee2MQTT publishes its
discovery messages again whenever Home Assistant comes online."
  (plain-file "mosquitto.conf"
              (string-append "listener " (number->string port) " 127.0.0.1\n"
                             "allow_anonymous true\n"
                             "persistence false\n"
                             "log_dest syslog\n")))


;;;
;;; YAML serialization, for the seed configuration of Zigbee2MQTT.
;;;

(define (yaml-scalar value)
  (match value
    ((? string?) (string-append "'" (string-join (string-split value #\')
                                                 "''")
                                "'"))
    ((? boolean?) (if value "true" "false"))
    ((? number?) (number->string value))))

(define (yaml-mapping? value)
  (and (pair? value) (every pair? value)))

(define* (yaml-mapping alist #:optional (indent ""))
  "Serialize ALIST, whose values are scalars or nested alists, as a YAML
block mapping."
  (string-concatenate
   (map (match-lambda
          ((key . (? yaml-mapping? nested))
           (string-append indent key ":\n"
                          (yaml-mapping nested (string-append indent "  "))))
          ((key . value)
           (string-append indent key ": " (yaml-scalar value) "\n")))
        alist)))


;;;
;;; Zigbee2MQTT.
;;;

(define-record-type* <zigbee2mqtt-configuration>
  zigbee2mqtt-configuration
  make-zigbee2mqtt-configuration
  zigbee2mqtt-configuration?
  (image           zigbee2mqtt-configuration-image)        ;string, with tag
  ;; Stable path of the coordinator, e.g. under /dev/serial/by-id/.
  (serial-port     zigbee2mqtt-configuration-serial-port)  ;string
  (adapter         zigbee2mqtt-configuration-adapter
                   (default "ember"))
  (baudrate        zigbee2mqtt-configuration-baudrate
                   (default 115200))
  (rtscts?         zigbee2mqtt-configuration-rtscts?
                   (default #f))
  (mqtt-server     zigbee2mqtt-configuration-mqtt-server
                   (default "mqtt://127.0.0.1:1883"))
  (frontend-port   zigbee2mqtt-configuration-frontend-port
                   (default 8080))
  (timezone        zigbee2mqtt-configuration-timezone)     ;string
  (data-directory  zigbee2mqtt-configuration-data-directory
                   (default "/var/lib/zigbee2mqtt"))
  ;; Root-only file with secrets for the container environment.  The
  ;; activation creates it with a random frontend token if it is missing.
  (environment-file zigbee2mqtt-configuration-environment-file
                    (default "/etc/zigbee2mqtt/environment")))

;; Inside the container the coordinator always appears under this name.
(define %zigbee2mqtt-container-serial-port "/dev/ttyZigbee")

(define (zigbee2mqtt-seed-configuration config)
  "The configuration.yaml Zigbee2MQTT starts from.  It is only a seed:
Zigbee2MQTT writes the generated network key and anything changed in its
frontend back to the file, which therefore must never be replaced."
  (plain-file
   "zigbee2mqtt-configuration.yaml"
   (yaml-mapping
    `(("version" . 5)
      ("homeassistant" ("enabled" . #t))
      ("frontend"
       ("enabled" . #t)
       ("port" . ,(zigbee2mqtt-configuration-frontend-port config)))
      ("mqtt"
       ("base_topic" . "zigbee2mqtt")
       ("server" . ,(zigbee2mqtt-configuration-mqtt-server config)))
      ("serial"
       ("port" . ,%zigbee2mqtt-container-serial-port)
       ("adapter" . ,(zigbee2mqtt-configuration-adapter config))
       ("baudrate" . ,(zigbee2mqtt-configuration-baudrate config))
       ("rtscts" . ,(zigbee2mqtt-configuration-rtscts? config)))
      ("advanced"
       ("network_key" . "GENERATE")
       ("pan_id" . "GENERATE")
       ("ext_pan_id" . "GENERATE"))))))

(define (zigbee2mqtt-activation config)
  (let ((data (zigbee2mqtt-configuration-data-directory config))
        (environment (zigbee2mqtt-configuration-environment-file config)))
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 binary-ports)
                     (rnrs bytevectors))
        (define (random-hex bytes)
          (call-with-input-file "/dev/urandom"
            (lambda (port)
              (string-concatenate
               (map (lambda (byte)
                      (string-pad (number->string byte 16) 2 #\0))
                    (bytevector->u8-list (get-bytevector-n port bytes)))))))
        (mkdir-p #$data)
        (chmod #$data #o700)
        (let ((file (string-append #$data "/configuration.yaml")))
          (unless (file-exists? file)
            (copy-file #$(zigbee2mqtt-seed-configuration config) file)
            (chmod file #o600)))
        (mkdir-p (dirname #$environment))
        ;; An empty file is what a failed earlier attempt leaves behind.
        (unless (and (file-exists? #$environment)
                     (> (stat:size (stat #$environment)) 0))
          (call-with-output-file #$environment
            (lambda (port)
              (chmod port #o600)
              (format port "ZIGBEE2MQTT_CONFIG_FRONTEND_AUTH_TOKEN=~a~%"
                      (random-hex 24))))))))

(define (zigbee2mqtt-oci-extension config)
  (oci-extension
   (containers
    (list (oci-container-configuration
           (provision "zigbee2mqtt")
           (user "root")
           (group "root")
           (image (zigbee2mqtt-configuration-image config))
           (network "host")
           (respawn? #t)
           (requirement '(mosquitto))
           (log-file "/var/log/zigbee2mqtt.log")
           (environment
            `(("TZ" . ,(zigbee2mqtt-configuration-timezone config))))
           (volumes
            `((,(zigbee2mqtt-configuration-data-directory config)
               . "/app/data")
              ("/run/udev" . "/run/udev:ro")))
           (extra-arguments
            (list "--device"
                  (string-append
                   (zigbee2mqtt-configuration-serial-port config)
                   ":" %zigbee2mqtt-container-serial-port)
                  "--env-file"
                  (zigbee2mqtt-configuration-environment-file config))))))))

(define zigbee2mqtt-service-type
  (service-type
   (name 'zigbee2mqtt)
   (extensions
    (list (service-extension oci-service-type zigbee2mqtt-oci-extension)
          (service-extension activation-service-type
                             zigbee2mqtt-activation)))
   (description "Run Zigbee2MQTT in a container, bridging a Zigbee
coordinator to a local MQTT broker.  The frontend requires the token from the
environment file.")))


;;;
;;; Home Assistant.
;;;

(define-record-type* <home-assistant-configuration>
  home-assistant-configuration
  make-home-assistant-configuration
  home-assistant-configuration?
  (image             home-assistant-configuration-image)   ;string, with tag
  (timezone          home-assistant-configuration-timezone) ;string
  (config-directory  home-assistant-configuration-config-directory
                     (default "/var/lib/home-assistant"))
  ;; Reach the host's BlueZ over the system D-Bus and let Home Assistant
  ;; manage the adapter.  Needs `bluetooth-service-type' on the host.
  (bluetooth?        home-assistant-configuration-bluetooth?
                     (default #f)))

(define (home-assistant-activation config)
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p #$(home-assistant-configuration-config-directory config))
      (chmod #$(home-assistant-configuration-config-directory config)
             #o700)))

(define (home-assistant-oci-extension config)
  (oci-extension
   (containers
    (list (oci-container-configuration
           (provision "home-assistant")
           (user "root")
           (group "root")
           (image (home-assistant-configuration-image config))
           (network "host")
           (respawn? #t)
           (log-file "/var/log/home-assistant.log")
           (environment
            `(("TZ" . ,(home-assistant-configuration-timezone config))))
           (volumes
            `((,(home-assistant-configuration-config-directory config)
               . "/config")
              ,@(if (home-assistant-configuration-bluetooth? config)
                    '(("/run/dbus" . "/run/dbus:ro"))
                    '())))
           (extra-arguments
            (if (home-assistant-configuration-bluetooth? config)
                '("--cap-add" "NET_ADMIN" "--cap-add" "NET_RAW")
                '())))))))

(define home-assistant-service-type
  (service-type
   (name 'home-assistant)
   (extensions
    (list (service-extension oci-service-type home-assistant-oci-extension)
          (service-extension activation-service-type
                             home-assistant-activation)))
   (description "Run Home Assistant in a container on the host network.
Its HTTP settings, including reverse-proxy trust, are made in its web
interface.")))
