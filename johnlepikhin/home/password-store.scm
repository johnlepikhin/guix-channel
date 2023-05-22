;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin home password-store)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages password-utils)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:export (home-password-store-service-type))

(define (add-packages config)
  (list
   password-store
   browserpass-native))

(define (add-files config)
  `((".config/chromium/NativeMessagingHosts/com.github.browserpass.native.json"
     ,(mixed-text-file
       "com.github.browserpass.native.json"
       #~(string-append
          "\
{
    \"name\": \"com.github.browserpass.native\",
    \"description\": \"Browserpass native component for the Chromium extension\",
    \"path\": \"" #$(file-append browserpass-native "/bin/browserpass") "\",
    \"type\": \"stdio\",
    \"allowed_origins\": [
        \"chrome-extension://naepdomgkenhinolocfifgehidddafch/\",
        \"chrome-extension://pjmbgaakjkbhpopmakjoedenlfdmcdgm/\",
        \"chrome-extension://klfoddkbhleoaabpmiigbmpbjfljimgb/\"
    ]
}")))))

(define home-password-store-service-type
  (service-type
   (name 'home-password-store)
   (extensions
    (list
     (service-extension home-profile-service-type
                        add-packages)
     (service-extension home-files-service-type
                        add-files)))
   (compose concatenate)
   (description "Setup password-store / browserpass")))
