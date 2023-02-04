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

(define-module (johnlepikhin home xmobar)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages wm)
  #:use-module (gnu home services)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-xmobar-configuration
            home-xmobar-service-type))

(define-record-type* <home-xmobar-configuration>
  home-xmobar-configuration make-home-xmobar-configuration
  home-xmobar-configuration?
  (package home-xmobar-configuration-package (default xmobar))
  (weather-id home-xmobar-configuration-weather-id (default "UCFM"))
  (screen-width home-xmobar-configuration-screen-width (default 1920))
  (height home-xmobar-configuration-height (default 32)))

(define (add-xmobar-config config)
  `((".xmobarrc"
     ,(mixed-text-file
       "xmobarrc"
       #~(string-append
          "Config {
         font = \"xft:Monospace-Bold:size=9\"
       , borderColor = \"#303030\"
       , border = BottomBM 3
       , borderWidth = 1
       , bgColor = \"black\"
       , fgColor = \"grey\"
       , position = Static { xpos = 0, ypos = 0, width = "
          #$(number->string (home-xmobar-configuration-screen-width config))
          ", height = "
          #$(number->string (home-xmobar-configuration-height config))
          " }
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = \".\"
       , allDesktops = True
       , overrideRedirect = False
       , commands = [
          Run Weather \"" #$(home-xmobar-configuration-weather-id config) "\" [\"-t\", \"<tempC>C\"
                             , \"-L\", \"18\", \"-H\", \"30\"
                             , \"--normal\", \"green\"
                             , \"--high\", \"red\"
                             , \"--low\", \"lightblue\" ] 36000
         , Run Memory [\"-t\", \"Mem: <usedratio>%\"] 50
         , Run Date \"%_d/%m %H:%M:%S\" \"date\" 10
         , Run StdinReader

, Run BatteryP [\"BAT0\"]
  [
    \"-t\", \"<acstatus>\"
  , \"-L\", \"20\"
  , \"-l\", \"red\"
  , \"--\", \"-O\", \"Charging\", \"-o\", \"<fc=white,red>Bat: <left>%</fc>\"
  ] 10

]
, sepChar = \"%\"
, alignSep = \"}{\"
, template = \"%StdinReader% }{ %memory% | %battery% | %UCFM% | %date% \"\n")))))

(define (add-xmobar-package config)
  (list (home-xmobar-configuration-package config)))

(define home-xmobar-service-type
  (service-type
   (name 'home-xmobar)
   (extensions
    (list
     (service-extension
      home-files-service-type add-xmobar-config)
     (service-extension
      home-profile-service-type add-xmobar-package)))
   (compose concatenate)
   (description "Install xmobar and create @file{~/.xmobarrc}")))
