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

(define-module (johnlepikhin packages pipewire)
  #:use-module ((gnu packages linux) #:prefix gnu:)
  #:use-module (guix gexp)
  #:use-module (guix packages))

;; Upstream PipeWire leaves the HFP (SCO) transport in the error state
;; after every HFP session: releasing a running SCO sink is reported as
;; a failure, and nothing clears it before the next acquire.  With
;; WirePlumber's A2DP<->HFP autoswitch, every second call then gets a
;; capture stream that dies after ~0.6 s.  The code path is unchanged in
;; 1.6.11 and in master as of 2026-11; see the patch header for the
;; mechanism.  Drop these variants once upstream clears the state.
;;
;; Both packages are hidden: they keep the upstream names and versions,
;; so a by-name lookup would be ambiguous.  Use them as package objects.
(define-public pipewire/sco-fix
  (hidden-package
   (package/inherit gnu:pipewire
     (source
      (origin
        (inherit (package-source gnu:pipewire))
        (patches
         (cons (local-file "patches/pipewire-bluez5-sco-stale-error.patch")
               (origin-patches (package-source gnu:pipewire)))))))))

;; The Bluetooth monitor runs inside WirePlumber, which loads
;; libspa-bluez5 from the SPA plugin directory of the libpipewire it is
;; linked against, so it has to be rebuilt against the patched build.
(define-public wireplumber/sco-fix
  (hidden-package
   (package/inherit gnu:wireplumber
     (inputs
      (modify-inputs (package-inputs gnu:wireplumber)
        (replace "pipewire" pipewire/sco-fix))))))
