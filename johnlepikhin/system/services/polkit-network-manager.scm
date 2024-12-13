;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Evgenii Lepikhin <johnlepikhin@gmail.com>
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
(define-module (johnlepikhin system services polkit-network-manager)
  #:use-module (gnu services dbus)
  #:use-module (guix gexp)
  #:use-module (gnu services))

(define polkit-network-manager
  (file-union
   "polkit-wheel"
   `(("share/polkit-1/rules.d/org.freedesktop.NetworkManager.rules"
      ,(plain-file
        "org.freedesktop.NetworkManager.rules"
        "polkit.addRule(function(action, subject) {
  if (action.id == \"org.freedesktop.NetworkManager.settings.modify.system\" &&
	subject.local && subject.active && subject.isInGroup (\"netdev\")) {
    return polkit.Result.YES;
  }
});")))))

(define polkit-network-manager-service
  (simple-service 'polkit-network-manager polkit-service-type (list polkit-network-manager)))
