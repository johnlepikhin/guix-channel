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

(define-module (johnlepikhin system services usb-port-disable)
  #:use-module (gnu services)
  #:use-module (gnu services base)                 ; udev-service-type, file->udev-rule
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:export (usb-port
            usb-port?
            usb-port-controller
            usb-port-hub
            usb-port-number
            usb-port-disable-service-type))

;; Power off selected USB root hub ports at boot, e.g. to silence a broken
;; built-in device that keeps waking the CPU.  Writing 1 to
;; /sys/.../usbN-portP/disable disconnects whatever sits on the port and cuts
;; its power, so the device cannot enumerate again.  Deauthorising the device
;; (`authorized' = 0) or a `usbcore.quirks' entry leaves the port powered.
;;
;; A port is identified by the PCI address of its xHCI controller, the root
;; hub (USB 2.0 or USB 3.x bus of that controller) and the port number.  The
;; bus number N is deliberately not part of the identity: it follows the probe
;; order of the host controllers and is not stable on machines with more than
;; one of them.
;;
;; Mechanism: ports have no subsystem and emit no uevents, so the rule matches
;; the `add' event of the root hub itself.  eudev does not expand
;; substitutions in the attribute name of an ATTR{...} assignment, which rules
;; out a plain ATTR write to a path containing N; instead RUN+= calls a small
;; program with the root hub's syspath and %n (= N).
;;
;; The rule fires only when the root hub appears -- at boot or when the
;; controller is re-bound.  The kernel enumerates the port before udev runs,
;; so a broken device still logs a few seconds of errors early in boot.  It is
;; not re-applied after suspend/resume; on an Intel xHCI controller the port
;; was observed to stay powered off across suspend/resume without it.
;;
;; Finding the values for a device reported as `usb N-P' in dmesg:
;;   readlink -f /sys/bus/usb/devices/usbN   → .../<controller>/usbN
;;   cat /sys/bus/usb/devices/usbN/speed     → 480 for `usb2', 5000+ for `usb3'
;; A device behind a USB 3.x connector has a port on both root hubs; list both.

(define (assert-pci-address value)
  (unless (and (string? value)
               (string-match "^[0-9a-f]{4}:[0-9a-f]{2}:[0-9a-f]{2}\\.[0-7]$"
                             value))
    (error "usb-port: controller must be a lower-case PCI address such as \
\"0000:00:14.0\":" value))
  value)

(define (assert-hub value)
  (unless (memq value '(usb2 usb3))
    (error "usb-port: hub must be 'usb2 or 'usb3:" value))
  value)

(define (assert-port-number value)
  (unless (and (exact-integer? value) (positive? value))
    (error "usb-port: number must be a positive integer:" value))
  value)

(define-record-type* <usb-port>
  usb-port make-usb-port
  usb-port?
  ;; PCI address of the host controller, as in /sys/bus/pci/devices.
  (controller usb-port-controller
              (sanitize assert-pci-address))
  ;; Root hub of that controller: `usb2' (High-Speed bus) or `usb3'
  ;; (SuperSpeed bus).  Built-in devices are usually on `usb2'.
  (hub        usb-port-hub
              (default 'usb2)
              (sanitize assert-hub))
  ;; Port number P on that root hub, as in `usbN-portP'.
  (number     usb-port-number
              (sanitize assert-port-number)))

(define (hub-speed-pattern hub)
  "Return the udev match for the `speed' attribute (Mb/s) of HUB's root hub."
  (case hub
    ((usb2) "480")
    ((usb3) "5000|10000|20000")))

(define %usb-port-disable-program
  (program-file
   "usb-port-disable"
   #~(begin
       (use-modules (ice-9 match))
       (match (command-line)
         ((_ root-hub bus port)
          (let ((file (string-append root-hub "/" bus "-0:1.0/usb" bus
                                     "-port" port "/disable")))
            (catch 'system-error
              (lambda ()
                (call-with-output-file file
                  (lambda (out)
                    (display "1" out))))
              (lambda args
                (format (current-error-port) "usb-port-disable: ~a: ~a~%"
                        file (strerror (system-error-errno args)))
                (exit 1)))))
         ((program . _)
          (format (current-error-port)
                  "usage: ~a ROOT-HUB-SYSPATH BUS-NUMBER PORT~%" program)
          (exit 2))))))

(define (usb-port->udev-rule port)
  "Return the pieces of the udev rule line that powers off PORT."
  (list "ACTION==\"add\", SUBSYSTEM==\"usb\", ENV{DEVTYPE}==\"usb_device\", "
        "KERNEL==\"usb[0-9]*\", "
        "ATTR{speed}==\"" (hub-speed-pattern (usb-port-hub port)) "\", "
        ;; Parent keys must all match the same ancestor: the controller.
        "SUBSYSTEMS==\"pci\", KERNELS==\"" (usb-port-controller port) "\", "
        "RUN+=\"" %usb-port-disable-program " %S%p %n "
        (number->string (usb-port-number port)) "\"\n"))

(define (usb-port-disable-udev-rules ports)
  (if (null? ports)
      '()
      (list (file->udev-rule
             "90-usb-port-disable.rules"
             (apply mixed-text-file "90-usb-port-disable.rules"
                    (append-map usb-port->udev-rule ports))))))

(define usb-port-disable-service-type
  (service-type
   (name 'usb-port-disable)
   (extensions
    (list (service-extension udev-service-type
                             usb-port-disable-udev-rules)))
   ;; Several instances and extensions add up to one list of ports.
   (compose concatenate)
   (extend append)
   (default-value '())
   (description "Power off the given USB root hub ports at boot through a udev
rule, so that a broken device on such a port can no longer enumerate or wake
the system.  The value is a list of @code{usb-port} records.")))
