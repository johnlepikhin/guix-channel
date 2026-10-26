;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023-2026 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

;;; Building blocks shared by the desktop and the server system services.

(define-module (johnlepikhin system services common)
  #:use-module (gnu packages linux)                ; util-linux
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (nonguix-signing-key
            bordeaux-signing-key
            make-guix-gc-job
            guix-gc-job
            fstrim-job
            prefer-ipv4-service))

;; Also signs the substitutes served by cache-cdn.guix.moe.
(define nonguix-signing-key
  (plain-file "non-guix.pub"
   "(public-key
     (ecc
       (curve Ed25519)
       (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)
     ))"))

(define bordeaux-signing-key
  (plain-file "bordeaux.guix.gnu.org.pub"
   "(public-key
     (ecc
       (curve Ed25519)
       (q #7D602902D3A2DBB83F8A0FB98602A754C5493B0B778C8D1DD4E0F41DE14DE34F#)
     ))"))

(define* (make-guix-gc-job #:key
                           (schedule "30 4   * * *")
                           (free-space "30G")
                           (delete-generations #f))
  "Return an mcron job that collects garbage until FREE-SPACE is available
and, when DELETE-GENERATIONS is a pattern such as \"1m\", first deletes
profile generations matching it."
  #~(job #$schedule
         #$(string-append "guix gc -F " free-space
                          (if delete-generations
                              (string-append " -d " delete-generations)
                              ""))))

(define guix-gc-job (make-guix-gc-job))

;; trim free blocks on SSD.  mcron's PATH has no sbin directory.
(define fstrim-job
  #~(job "40 */2   * * *"
         (string-append #$(file-append util-linux "/sbin/fstrim") " -v /")))

;; IPv6 to Cloudflare (gitlab.com and others) times out on some networks,
;; while libgit2 tries addresses in getaddrinfo order without Happy Eyeballs,
;; so `guix pull' fails on nonguix.  Prefer IPv4.
(define prefer-ipv4-service
  (simple-service 'prefer-ipv4 etc-service-type
                  (list `("gai.conf"
                          ,(plain-file "gai.conf"
                                       "precedence ::ffff:0:0/96 100\n")))))
