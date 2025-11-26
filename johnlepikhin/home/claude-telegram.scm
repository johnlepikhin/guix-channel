;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin home claude-telegram)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages web)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (home-claude-telegram-configuration
            home-claude-telegram-service-type))

(define-record-type* <home-claude-telegram-configuration>
  home-claude-telegram-configuration make-home-claude-telegram-configuration
  home-claude-telegram-configuration?
  (bot-token home-claude-telegram-configuration-bot-token)
  (chat-id home-claude-telegram-configuration-chat-id))

(define (generate-env-file config)
  (plain-file
   "telegram-claude.env"
   (string-append
    "TELEGRAM_BOT_TOKEN=\"" (home-claude-telegram-configuration-bot-token config) "\"\n"
    "TELEGRAM_CHAT_ID=\"" (home-claude-telegram-configuration-chat-id config) "\"\n")))

(define (add-files config)
  `((".config/telegram-claude.env"
     ,(generate-env-file config))
    (".local/bin/claude-telegram-notify"
     ,(local-file "files/claude-telegram-notify" #:recursive? #t))))

(define (add-packages config)
  (list curl jq))

(define home-claude-telegram-service-type
  (service-type
   (name 'home-claude-telegram)
   (extensions
    (list
     (service-extension home-profile-service-type add-packages)
     (service-extension home-files-service-type add-files)))
   (description "Setup Claude Code Telegram notifications with curl and jq dependencies.")))
