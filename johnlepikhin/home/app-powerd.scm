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

(define-module (johnlepikhin home app-powerd)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (johnlepikhin home xsession)
  #:use-module (johnlepikhin packages app-powerd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 format)
  #:export (app-powerd-guards-configuration
            app-powerd-guards-configuration?
            app-powerd-maintenance-configuration
            app-powerd-maintenance-configuration?
            app-powerd-timing-configuration
            app-powerd-timing-configuration?
            app-powerd-mode-configuration
            app-powerd-mode-configuration?
            app-powerd-defaults-configuration
            app-powerd-defaults-configuration?
            app-powerd-profile-configuration
            app-powerd-profile-configuration?
            app-powerd-match-configuration
            app-powerd-match-configuration?
            app-powerd-policy-configuration
            app-powerd-policy-configuration?
            app-powerd-rule-configuration
            app-powerd-rule-configuration?
            home-app-powerd-configuration
            home-app-powerd-configuration?
            home-app-powerd-service-type))

;;;
;;; Validators
;;;

(define (sanitize-guard-value field-name)
  (lambda (value)
    (unless (memq value '(check ignore))
      (error (format #f "app-powerd: ~a must be 'check or 'ignore, got: ~a"
                     field-name value)))
    value))

(define (sanitize-action value)
  (unless (memq value '(freeze throttle ignore))
    (error (format #f "app-powerd: action must be 'freeze, 'throttle, or 'ignore, got: ~a"
                   value)))
  value)

(define (sanitize-mode-value field-name)
  (lambda (value)
    (unless (memq value '(enable disable))
      (error (format #f "app-powerd: ~a must be 'enable or 'disable, got: ~a"
                     field-name value)))
    value))

(define (sanitize-optional-action value)
  (when (and value (not (memq value '(freeze throttle ignore))))
    (error (format #f "app-powerd: action must be 'freeze, 'throttle, 'ignore, or #f, got: ~a"
                   value)))
  value)

;;;
;;; Record types
;;;

(define-record-type* <app-powerd-guards-configuration>
  app-powerd-guards-configuration make-app-powerd-guards-configuration
  app-powerd-guards-configuration?
  (audio-active  app-powerd-guards-configuration-audio-active
                 (default 'check)
                 (sanitize (sanitize-guard-value 'audio-active)))
  (mic-active    app-powerd-guards-configuration-mic-active
                 (default 'check)
                 (sanitize (sanitize-guard-value 'mic-active)))
  (camera-active app-powerd-guards-configuration-camera-active
                 (default 'check)
                 (sanitize (sanitize-guard-value 'camera-active)))
  (fullscreen    app-powerd-guards-configuration-fullscreen
                 (default 'check)
                 (sanitize (sanitize-guard-value 'fullscreen)))
  (input-idle    app-powerd-guards-configuration-input-idle    (default #f)))

(define-record-type* <app-powerd-maintenance-configuration>
  app-powerd-maintenance-configuration make-app-powerd-maintenance-configuration
  app-powerd-maintenance-configuration?
  (enabled  app-powerd-maintenance-configuration-enabled  (default #f))
  (interval app-powerd-maintenance-configuration-interval (default "30s"))
  (duration app-powerd-maintenance-configuration-duration (default "1s")))

(define-record-type* <app-powerd-timing-configuration>
  app-powerd-timing-configuration make-app-powerd-timing-configuration
  app-powerd-timing-configuration?
  (suspend-delay app-powerd-timing-configuration-suspend-delay (default "30s"))
  (resume-grace  app-powerd-timing-configuration-resume-grace  (default "3s"))
  (min-suspend   app-powerd-timing-configuration-min-suspend   (default "5s")))

(define-record-type* <app-powerd-mode-configuration>
  app-powerd-mode-configuration make-app-powerd-mode-configuration
  app-powerd-mode-configuration?
  (ac      app-powerd-mode-configuration-ac
           (default 'disable)
           (sanitize (sanitize-mode-value 'ac)))
  (battery app-powerd-mode-configuration-battery
           (default 'enable)
           (sanitize (sanitize-mode-value 'battery))))

(define-record-type* <app-powerd-defaults-configuration>
  app-powerd-defaults-configuration make-app-powerd-defaults-configuration
  app-powerd-defaults-configuration?
  (enabled            app-powerd-defaults-configuration-enabled
                      (default #t))
  (mode               app-powerd-defaults-configuration-mode
                      (default (app-powerd-mode-configuration)))
  (timing             app-powerd-defaults-configuration-timing
                      (default (app-powerd-timing-configuration)))
  (maintenance-resume app-powerd-defaults-configuration-maintenance-resume
                      (default (app-powerd-maintenance-configuration)))
  (guards             app-powerd-defaults-configuration-guards
                      (default (app-powerd-guards-configuration))))

(define-record-type* <app-powerd-profile-configuration>
  app-powerd-profile-configuration make-app-powerd-profile-configuration
  app-powerd-profile-configuration?
  (name               app-powerd-profile-configuration-name)
  (action             app-powerd-profile-configuration-action
                      (sanitize sanitize-action))
  (suspend-delay      app-powerd-profile-configuration-suspend-delay
                      (default #f))
  (nice               app-powerd-profile-configuration-nice
                      (default #f))
  (cpu-weight         app-powerd-profile-configuration-cpu-weight
                      (default #f))
  (cpu-quota          app-powerd-profile-configuration-cpu-quota
                      (default #f))
  (maintenance-resume app-powerd-profile-configuration-maintenance-resume
                      (default #f))
  (guards             app-powerd-profile-configuration-guards
                      (default #f)))

(define-record-type* <app-powerd-match-configuration>
  app-powerd-match-configuration make-app-powerd-match-configuration
  app-powerd-match-configuration?
  (executable         app-powerd-match-configuration-executable
                      (default '()))
  (wm-class           app-powerd-match-configuration-wm-class
                      (default '()))
  (app-id             app-powerd-match-configuration-app-id
                      (default '()))
  (desktop-file       app-powerd-match-configuration-desktop-file
                      (default '()))
  (cmdline-regex      app-powerd-match-configuration-cmdline-regex
                      (default #f))
  (window-title-regex app-powerd-match-configuration-window-title-regex
                      (default #f)))

(define-record-type* <app-powerd-policy-configuration>
  app-powerd-policy-configuration make-app-powerd-policy-configuration
  app-powerd-policy-configuration?
  (use-profile        app-powerd-policy-configuration-use-profile
                      (default #f))
  (action             app-powerd-policy-configuration-action
                      (default #f)
                      (sanitize sanitize-optional-action))
  (suspend-delay      app-powerd-policy-configuration-suspend-delay
                      (default #f))
  (nice               app-powerd-policy-configuration-nice
                      (default #f))
  (cpu-weight         app-powerd-policy-configuration-cpu-weight
                      (default #f))
  (cpu-quota          app-powerd-policy-configuration-cpu-quota
                      (default #f))
  (maintenance-resume app-powerd-policy-configuration-maintenance-resume
                      (default #f))
  (guards             app-powerd-policy-configuration-guards
                      (default #f)))

(define-record-type* <app-powerd-rule-configuration>
  app-powerd-rule-configuration make-app-powerd-rule-configuration
  app-powerd-rule-configuration?
  (id     app-powerd-rule-configuration-id)
  (match  app-powerd-rule-configuration-match)
  (policy app-powerd-rule-configuration-policy))

(define-record-type* <home-app-powerd-configuration>
  home-app-powerd-configuration make-home-app-powerd-configuration
  home-app-powerd-configuration?
  (package  home-app-powerd-configuration-package
            (default app-powerd))
  (defaults home-app-powerd-configuration-defaults
            (default (app-powerd-defaults-configuration)))
  (profiles home-app-powerd-configuration-profiles
            (default %default-profiles))
  (rules    home-app-powerd-configuration-rules
            (default %default-rules)))

;;;
;;; YAML serialization helpers
;;;

(define (indent level str)
  (string-append (make-string (* 2 level) #\space) str))

(define (yaml-field level key value)
  (indent level (string-append key ": " value "\n")))

(define (yaml-string-list items)
  (string-append "[" (string-join items ", ") "]"))

(define (serialize-guards guards level)
  (string-append
   (indent level "guards:\n")
   (yaml-field (+ level 1) "audio_active"
               (symbol->string (app-powerd-guards-configuration-audio-active guards)))
   (yaml-field (+ level 1) "mic_active"
               (symbol->string (app-powerd-guards-configuration-mic-active guards)))
   (yaml-field (+ level 1) "camera_active"
               (symbol->string (app-powerd-guards-configuration-camera-active guards)))
   (yaml-field (+ level 1) "fullscreen"
               (symbol->string (app-powerd-guards-configuration-fullscreen guards)))
   (let ((idle (app-powerd-guards-configuration-input-idle guards)))
     (if idle
         (yaml-field (+ level 1) "input_idle"
                     (string-append "\"" idle "\""))
         ""))))

(define (serialize-maintenance maint level)
  (string-append
   (indent level "maintenance_resume:\n")
   (yaml-field (+ level 1) "enabled"
               (if (app-powerd-maintenance-configuration-enabled maint)
                   "true" "false"))
   (yaml-field (+ level 1) "interval"
               (string-append "\"" (app-powerd-maintenance-configuration-interval maint) "\""))
   (yaml-field (+ level 1) "duration"
               (string-append "\"" (app-powerd-maintenance-configuration-duration maint) "\""))))

(define (serialize-timing timing level)
  (string-append
   (indent level "timing:\n")
   (yaml-field (+ level 1) "suspend_delay"
               (string-append "\"" (app-powerd-timing-configuration-suspend-delay timing) "\""))
   (yaml-field (+ level 1) "resume_grace"
               (string-append "\"" (app-powerd-timing-configuration-resume-grace timing) "\""))
   (yaml-field (+ level 1) "min_suspend"
               (string-append "\"" (app-powerd-timing-configuration-min-suspend timing) "\""))))

(define (serialize-mode mode level)
  (string-append
   (indent level "mode:\n")
   (yaml-field (+ level 1) "ac"
               (symbol->string (app-powerd-mode-configuration-ac mode)))
   (yaml-field (+ level 1) "battery"
               (symbol->string (app-powerd-mode-configuration-battery mode)))))

(define (serialize-defaults defaults)
  (string-append
   "defaults:\n"
   (yaml-field 1 "enabled"
               (if (app-powerd-defaults-configuration-enabled defaults)
                   "true" "false"))
   "\n"
   (serialize-mode (app-powerd-defaults-configuration-mode defaults) 1)
   "\n"
   (serialize-timing (app-powerd-defaults-configuration-timing defaults) 1)
   "\n"
   (serialize-maintenance
    (app-powerd-defaults-configuration-maintenance-resume defaults) 1)
   "\n"
   (serialize-guards (app-powerd-defaults-configuration-guards defaults) 1)))

(define (serialize-profile profile)
  (let ((name (app-powerd-profile-configuration-name profile))
        (action (app-powerd-profile-configuration-action profile))
        (suspend-delay (app-powerd-profile-configuration-suspend-delay profile))
        (nice (app-powerd-profile-configuration-nice profile))
        (cpu-weight (app-powerd-profile-configuration-cpu-weight profile))
        (cpu-quota (app-powerd-profile-configuration-cpu-quota profile))
        (maint (app-powerd-profile-configuration-maintenance-resume profile))
        (guards (app-powerd-profile-configuration-guards profile)))
    (string-append
     (indent 1 (string-append name ":\n"))
     (yaml-field 2 "action" (symbol->string action))
     (if suspend-delay
         (yaml-field 2 "suspend_delay" (string-append "\"" suspend-delay "\""))
         "")
     (if nice
         (yaml-field 2 "nice" (number->string nice))
         "")
     (if cpu-weight
         (yaml-field 2 "cpu_weight" (number->string cpu-weight))
         "")
     (if cpu-quota
         (yaml-field 2 "cpu_quota" (string-append "\"" cpu-quota "\""))
         "")
     (if maint
         (serialize-maintenance maint 2)
         "")
     (if guards
         (serialize-guards guards 2)
         ""))))

(define (serialize-profiles profiles)
  (string-append
   "profiles:\n"
   (string-join (map serialize-profile profiles) "\n")))

(define (serialize-match-config match level)
  (let ((executable (app-powerd-match-configuration-executable match))
        (wm-class (app-powerd-match-configuration-wm-class match))
        (app-id (app-powerd-match-configuration-app-id match))
        (desktop-file (app-powerd-match-configuration-desktop-file match))
        (cmdline-regex (app-powerd-match-configuration-cmdline-regex match))
        (window-title-regex (app-powerd-match-configuration-window-title-regex match)))
    (string-append
     (indent level "match:\n")
     (if (null? executable) ""
         (yaml-field (+ level 1) "executable" (yaml-string-list executable)))
     (if (null? wm-class) ""
         (yaml-field (+ level 1) "wm_class" (yaml-string-list wm-class)))
     (if (null? app-id) ""
         (yaml-field (+ level 1) "app_id" (yaml-string-list app-id)))
     (if (null? desktop-file) ""
         (yaml-field (+ level 1) "desktop_file" (yaml-string-list desktop-file)))
     (if cmdline-regex
         (yaml-field (+ level 1) "cmdline_regex"
                     (string-append "\"" cmdline-regex "\""))
         "")
     (if window-title-regex
         (yaml-field (+ level 1) "window_title_regex"
                     (string-append "\"" window-title-regex "\""))
         ""))))

(define (serialize-policy policy level)
  (let ((use-profile (app-powerd-policy-configuration-use-profile policy))
        (action (app-powerd-policy-configuration-action policy))
        (suspend-delay (app-powerd-policy-configuration-suspend-delay policy))
        (nice (app-powerd-policy-configuration-nice policy))
        (cpu-weight (app-powerd-policy-configuration-cpu-weight policy))
        (cpu-quota (app-powerd-policy-configuration-cpu-quota policy))
        (maint (app-powerd-policy-configuration-maintenance-resume policy))
        (guards (app-powerd-policy-configuration-guards policy)))
    (string-append
     (indent level "policy:\n")
     (if use-profile
         (yaml-field (+ level 1) "use_profile" use-profile)
         "")
     (if action
         (yaml-field (+ level 1) "action" (symbol->string action))
         "")
     (if suspend-delay
         (yaml-field (+ level 1) "suspend_delay"
                     (string-append "\"" suspend-delay "\""))
         "")
     (if nice
         (yaml-field (+ level 1) "nice" (number->string nice))
         "")
     (if cpu-weight
         (yaml-field (+ level 1) "cpu_weight" (number->string cpu-weight))
         "")
     (if cpu-quota
         (yaml-field (+ level 1) "cpu_quota" (string-append "\"" cpu-quota "\""))
         "")
     (if maint
         (serialize-maintenance maint (+ level 1))
         "")
     (if guards
         (serialize-guards guards (+ level 1))
         ""))))

(define (serialize-rule rule)
  (let ((id (app-powerd-rule-configuration-id rule))
        (match (app-powerd-rule-configuration-match rule))
        (policy (app-powerd-rule-configuration-policy rule)))
    (string-append
     (indent 1 (string-append "- id: " id "\n"))
     (serialize-match-config match 2)
     (serialize-policy policy 2))))

(define (serialize-rules rules)
  (string-append
   "rules:\n"
   (string-join (map serialize-rule rules) "\n")))

(define (serialize-configuration config)
  (let ((defaults (home-app-powerd-configuration-defaults config))
        (profiles (home-app-powerd-configuration-profiles config))
        (rules (home-app-powerd-configuration-rules config)))
    (string-append
     "version: 1\n\n"
     (serialize-defaults defaults)
     "\n"
     (serialize-profiles profiles)
     "\n"
     (serialize-rules rules))))

;;;
;;; Default profiles
;;;

(define %default-profiles
  (list
   (app-powerd-profile-configuration
    (name "ignore")
    (action 'ignore))

   (app-powerd-profile-configuration
    (name "freeze")
    (action 'freeze)
    (suspend-delay "60s"))

   (app-powerd-profile-configuration
    (name "freeze-fast")
    (action 'freeze)
    (suspend-delay "20s")
    (guards (app-powerd-guards-configuration
             (audio-active 'ignore)
             (mic-active 'ignore)
             (camera-active 'ignore)
             (fullscreen 'check))))

   (app-powerd-profile-configuration
    (name "throttle")
    (action 'throttle)
    (suspend-delay "30s")
    (nice 5)
    (cpu-weight 20)
    (cpu-quota "40%"))

   (app-powerd-profile-configuration
    (name "editor")
    (action 'throttle)
    (suspend-delay "45s")
    (nice 5)
    (cpu-weight 50)
    (cpu-quota "50%"))

   (app-powerd-profile-configuration
    (name "browser")
    (action 'throttle)
    (suspend-delay "30s")
    (nice 5)
    (cpu-weight 20)
    (cpu-quota "40%"))

   (app-powerd-profile-configuration
    (name "messenger")
    (action 'freeze)
    (suspend-delay "1m")
    (maintenance-resume (app-powerd-maintenance-configuration
                         (enabled #t)
                         (interval "30s")
                         (duration "3s")))
    (guards (app-powerd-guards-configuration
             (audio-active 'ignore))))

   (app-powerd-profile-configuration
    (name "email")
    (action 'freeze)
    (suspend-delay "3m")
    (maintenance-resume (app-powerd-maintenance-configuration
                         (enabled #t)
                         (interval "5m")
                         (duration "5s"))))

   (app-powerd-profile-configuration
    (name "throttle-aggressive")
    (action 'throttle)
    (suspend-delay "30s")
    (nice 19)
    (cpu-weight 1)
    (cpu-quota "5%"))

   (app-powerd-profile-configuration
    (name "background-worker")
    (action 'throttle)
    (suspend-delay "60s")
    (nice 10)
    (cpu-weight 10)
    (cpu-quota "25%"))))

;;;
;;; Default rules
;;;

(define %default-rules
  (list
   ;; System utilities: ignore
   (app-powerd-rule-configuration
    (id "i3-components")
    (match (app-powerd-match-configuration
            (executable '("i3bar" "i3status" "i3lock" "i3-nagbar" "polybar"
                          "waybar" "sway" "swaybg" "swayidle" "swaylock"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   (app-powerd-rule-configuration
    (id "system-tray")
    (match (app-powerd-match-configuration
            (executable '("nm-applet" "blueman-applet" "blueman-tray"
                          "pasystray" "udiskie" "cbatticon" "batsignal"
                          "trayer" "stalonetray"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   (app-powerd-rule-configuration
    (id "clipboard-managers")
    (match (app-powerd-match-configuration
            (executable '("clipman" "clipmenu" "clipcat" "parcellite" "copyq"
                          "gpaste-client" "xclip" "xsel" "wl-copy" "wl-paste"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   (app-powerd-rule-configuration
    (id "notification-daemons")
    (match (app-powerd-match-configuration
            (executable '("dunst" "mako" "swaync" "fnott"
                          "deadd-notification-center" "xfce4-notifyd"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   (app-powerd-rule-configuration
    (id "screenshot-tools")
    (match (app-powerd-match-configuration
            (executable '("flameshot" "scrot" "maim" "grim" "slurp"
                          "spectacle" "gnome-screenshot" "shutter" "ksnip"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   (app-powerd-rule-configuration
    (id "launchers")
    (match (app-powerd-match-configuration
            (executable '("rofi" "dmenu" "wofi" "fuzzel" "bemenu"
                          "albert" "ulauncher" "synapse"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   (app-powerd-rule-configuration
    (id "compositors")
    (match (app-powerd-match-configuration
            (executable '("picom" "compton" "xcompmgr"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   ;; Terminals: ignore
   (app-powerd-rule-configuration
    (id "alacritty")
    (match (app-powerd-match-configuration
            (wm-class '("Alacritty"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   (app-powerd-rule-configuration
    (id "terminals")
    (match (app-powerd-match-configuration
            (executable '("kitty" "foot" "wezterm-gui" "st" "urxvt" "urxvtd"
                          "xterm" "gnome-terminal-server" "konsole"
                          "xfce4-terminal" "tilix" "terminator" "sakura"
                          "lxterminal" "mate-terminal" "guake" "yakuake"
                          "tilda"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   ;; Editors: throttle
   (app-powerd-rule-configuration
    (id "emacs")
    (match (app-powerd-match-configuration
            (wm-class '("Emacs"))))
    (policy (app-powerd-policy-configuration
             (use-profile "editor"))))

   (app-powerd-rule-configuration
    (id "neovim-gui")
    (match (app-powerd-match-configuration
            (executable '("neovide" "gvim"))))
    (policy (app-powerd-policy-configuration
             (use-profile "editor"))))

   ;; IDEs
   (app-powerd-rule-configuration
    (id "vscode")
    (match (app-powerd-match-configuration
            (executable '("code" "code-oss" "codium"))))
    (policy (app-powerd-policy-configuration
             (use-profile "editor")
             (cpu-quota "60%"))))

   (app-powerd-rule-configuration
    (id "jetbrains")
    (match (app-powerd-match-configuration
            (cmdline-regex "jetbrains|intellij|idea|pycharm|webstorm|clion|goland|rider|rustrover|datagrip|dataspell|phpstorm|fleet")))
    (policy (app-powerd-policy-configuration
             (use-profile "editor")
             (cpu-quota "50%"))))

   (app-powerd-rule-configuration
    (id "zed")
    (match (app-powerd-match-configuration
            (executable '("zed"))))
    (policy (app-powerd-policy-configuration
             (use-profile "editor"))))

   ;; Browsers
   (app-powerd-rule-configuration
    (id "chrome")
    (match (app-powerd-match-configuration
            (executable '("google-chrome" "google-chrome-stable"
                          "google-chrome-beta" "google-chrome-unstable"
                          "chromium" "chromium-browser"))))
    (policy (app-powerd-policy-configuration
             (use-profile "browser"))))

   (app-powerd-rule-configuration
    (id "firefox")
    (match (app-powerd-match-configuration
            (executable '("firefox" "firefox-esr" "firefox-developer-edition"
                          "librewolf" "waterfox" "floorp"))))
    (policy (app-powerd-policy-configuration
             (use-profile "browser"))))

   (app-powerd-rule-configuration
    (id "brave")
    (match (app-powerd-match-configuration
            (executable '("brave" "brave-browser"))))
    (policy (app-powerd-policy-configuration
             (use-profile "browser"))))

   (app-powerd-rule-configuration
    (id "epiphany")
    (match (app-powerd-match-configuration
            (executable '("epiphany" "epiphany-browser"))))
    (policy (app-powerd-policy-configuration
             (use-profile "browser"))))

   (app-powerd-rule-configuration
    (id "vivaldi")
    (match (app-powerd-match-configuration
            (executable '("vivaldi" "vivaldi-stable"))))
    (policy (app-powerd-policy-configuration
             (use-profile "browser"))))

   (app-powerd-rule-configuration
    (id "opera")
    (match (app-powerd-match-configuration
            (executable '("opera"))))
    (policy (app-powerd-policy-configuration
             (use-profile "browser"))))

   ;; Messengers
   (app-powerd-rule-configuration
    (id "telegram")
    (match (app-powerd-match-configuration
            (wm-class '("TelegramDesktop"))))
    (policy (app-powerd-policy-configuration
             (use-profile "messenger"))))

   (app-powerd-rule-configuration
    (id "vkteams")
    (match (app-powerd-match-configuration
            (wm-class '(".vkteams-real"))))
    (policy (app-powerd-policy-configuration
             (use-profile "messenger"))))

   (app-powerd-rule-configuration
    (id "slack")
    (match (app-powerd-match-configuration
            (executable '("slack"))))
    (policy (app-powerd-policy-configuration
             (use-profile "messenger"))))

   (app-powerd-rule-configuration
    (id "discord")
    (match (app-powerd-match-configuration
            (executable '("discord" "Discord"))))
    (policy (app-powerd-policy-configuration
             (use-profile "messenger"))))

   (app-powerd-rule-configuration
    (id "signal")
    (match (app-powerd-match-configuration
            (executable '("signal-desktop"))))
    (policy (app-powerd-policy-configuration
             (use-profile "messenger"))))

   (app-powerd-rule-configuration
    (id "element")
    (match (app-powerd-match-configuration
            (executable '("element-desktop"))))
    (policy (app-powerd-policy-configuration
             (use-profile "messenger"))))

   (app-powerd-rule-configuration
    (id "teams")
    (match (app-powerd-match-configuration
            (executable '("teams" "teams-for-linux"))))
    (policy (app-powerd-policy-configuration
             (use-profile "messenger"))))

   (app-powerd-rule-configuration
    (id "zoom")
    (match (app-powerd-match-configuration
            (executable '("zoom" "ZoomLauncher"))))
    (policy (app-powerd-policy-configuration
             (use-profile "messenger"))))

   ;; Email
   (app-powerd-rule-configuration
    (id "thunderbird")
    (match (app-powerd-match-configuration
            (executable '("thunderbird" "thunderbird-esr"))))
    (policy (app-powerd-policy-configuration
             (use-profile "email"))))

   (app-powerd-rule-configuration
    (id "evolution")
    (match (app-powerd-match-configuration
            (executable '("evolution"))))
    (policy (app-powerd-policy-configuration
             (use-profile "email"))))

   (app-powerd-rule-configuration
    (id "geary")
    (match (app-powerd-match-configuration
            (executable '("geary"))))
    (policy (app-powerd-policy-configuration
             (use-profile "email"))))

   ;; Media players: ignore
   (app-powerd-rule-configuration
    (id "smplayer")
    (match (app-powerd-match-configuration
            (wm-class '("Smplayer" "smplayer"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   (app-powerd-rule-configuration
    (id "media-players")
    (match (app-powerd-match-configuration
            (executable '("vlc" "mpv" "celluloid" "totem" "parole" "mplayer"
                          "haruna" "kaffeine" "xplayer" "dragon"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   (app-powerd-rule-configuration
    (id "music-players")
    (match (app-powerd-match-configuration
            (executable '("spotify" "rhythmbox" "lollypop" "amberol" "elisa"
                          "clementine" "strawberry" "audacious" "deadbeef"
                          "cmus" "quodlibet" "sayonara" "gnome-music"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   (app-powerd-rule-configuration
    (id "audio-production")
    (match (app-powerd-match-configuration
            (executable '("ardour" "audacity" "lmms" "hydrogen" "musescore"
                          "rosegarden" "qtractor" "zrythm" "carla"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   ;; Graphics / Design
   (app-powerd-rule-configuration
    (id "gimp")
    (match (app-powerd-match-configuration
            (executable '("gimp" "gimp-2.10" "gimp-2.99"))))
    (policy (app-powerd-policy-configuration
             (use-profile "throttle")
             (cpu-quota "30%"))))

   (app-powerd-rule-configuration
    (id "inkscape")
    (match (app-powerd-match-configuration
            (executable '("inkscape"))))
    (policy (app-powerd-policy-configuration
             (use-profile "throttle")
             (cpu-quota "30%"))))

   (app-powerd-rule-configuration
    (id "krita")
    (match (app-powerd-match-configuration
            (executable '("krita"))))
    (policy (app-powerd-policy-configuration
             (use-profile "throttle")
             (cpu-quota "30%"))))

   (app-powerd-rule-configuration
    (id "blender")
    (match (app-powerd-match-configuration
            (executable '("blender"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   (app-powerd-rule-configuration
    (id "obs")
    (match (app-powerd-match-configuration
            (executable '("obs" "obs-studio"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   (app-powerd-rule-configuration
    (id "kdenlive")
    (match (app-powerd-match-configuration
            (executable '("kdenlive"))))
    (policy (app-powerd-policy-configuration
             (use-profile "background-worker"))))

   (app-powerd-rule-configuration
    (id "darktable")
    (match (app-powerd-match-configuration
            (executable '("darktable" "rawtherapee" "digikam"))))
    (policy (app-powerd-policy-configuration
             (use-profile "background-worker"))))

   ;; Office / Documents
   (app-powerd-rule-configuration
    (id "libreoffice")
    (match (app-powerd-match-configuration
            (cmdline-regex "soffice|libreoffice")))
    (policy (app-powerd-policy-configuration
             (use-profile "throttle-aggressive"))))

   (app-powerd-rule-configuration
    (id "onlyoffice")
    (match (app-powerd-match-configuration
            (executable '("onlyoffice-desktopeditors" "DesktopEditors"))))
    (policy (app-powerd-policy-configuration
             (use-profile "throttle-aggressive"))))

   ;; Viewers: freeze fast
   (app-powerd-rule-configuration
    (id "pdf-viewers")
    (match (app-powerd-match-configuration
            (executable '("evince" "okular" "zathura" "mupdf" "xreader"
                          "atril" "qpdfview" "sioyek"))))
    (policy (app-powerd-policy-configuration
             (use-profile "freeze-fast"))))

   (app-powerd-rule-configuration
    (id "ebook-readers")
    (match (app-powerd-match-configuration
            (executable '("calibre" "calibre-gui" "foliate" "fbreader"))))
    (policy (app-powerd-policy-configuration
             (use-profile "freeze-fast"))))

   (app-powerd-rule-configuration
    (id "image-viewers")
    (match (app-powerd-match-configuration
            (executable '("eog" "eom" "feh" "sxiv" "nsxiv" "imv" "ristretto"
                          "shotwell" "gthumb" "gwenview" "nomacs" "loupe"))))
    (policy (app-powerd-policy-configuration
             (use-profile "freeze-fast"))))

   ;; File managers
   (app-powerd-rule-configuration
    (id "file-managers")
    (match (app-powerd-match-configuration
            (executable '("nautilus" "thunar" "pcmanfm" "pcmanfm-qt" "nemo"
                          "caja" "dolphin" "konqueror" "spacefm" "krusader"
                          "mc"))))
    (policy (app-powerd-policy-configuration
             (use-profile "freeze-fast"))))

   ;; Virtual machines: ignore
   (app-powerd-rule-configuration
    (id "virt-manager")
    (match (app-powerd-match-configuration
            (executable '("virt-manager" "virt-viewer" "remote-viewer"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   (app-powerd-rule-configuration
    (id "qemu")
    (match (app-powerd-match-configuration
            (cmdline-regex "qemu-system")))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   (app-powerd-rule-configuration
    (id "virtualbox")
    (match (app-powerd-match-configuration
            (executable '("VirtualBox" "VirtualBoxVM" "VBoxHeadless"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   (app-powerd-rule-configuration
    (id "gnome-boxes")
    (match (app-powerd-match-configuration
            (executable '("gnome-boxes"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   ;; Remote desktop: ignore
   (app-powerd-rule-configuration
    (id "remote-desktop")
    (match (app-powerd-match-configuration
            (executable '("remmina" "xfreerdp" "vncviewer" "vinagre"
                          "rustdesk" "anydesk" "krdc" "tigervnc"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   ;; Database / API clients
   (app-powerd-rule-configuration
    (id "db-clients")
    (match (app-powerd-match-configuration
            (executable '("dbeaver" "pgadmin4" "beekeeper-studio" "dbgate"))))
    (policy (app-powerd-policy-configuration
             (use-profile "throttle")
             (cpu-quota "20%"))))

   (app-powerd-rule-configuration
    (id "api-clients")
    (match (app-powerd-match-configuration
            (executable '("insomnia" "postman" "hoppscotch"))))
    (policy (app-powerd-policy-configuration
             (use-profile "throttle")
             (cpu-quota "20%"))))

   ;; Settings / Control panels
   (app-powerd-rule-configuration
    (id "settings")
    (match (app-powerd-match-configuration
            (executable '("gnome-control-center" "pavucontrol" "blueman-manager"
                          "lxappearance" "qt5ct" "qt6ct" "arandr"
                          "xfce4-settings-manager" "systemsettings"
                          "gnome-tweaks"))))
    (policy (app-powerd-policy-configuration
             (use-profile "freeze-fast"))))

   ;; Torrent clients
   (app-powerd-rule-configuration
    (id "torrent-clients")
    (match (app-powerd-match-configuration
            (executable '("transmission-gtk" "transmission-qt" "qbittorrent"
                          "deluge" "deluge-gtk" "fragments"))))
    (policy (app-powerd-policy-configuration
             (use-profile "throttle-aggressive"))))

   ;; Gaming: ignore
   (app-powerd-rule-configuration
    (id "steam")
    (match (app-powerd-match-configuration
            (executable '("steam" "steamwebhelper"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   (app-powerd-rule-configuration
    (id "game-launchers")
    (match (app-powerd-match-configuration
            (executable '("lutris" "heroic" "gamescope" "mangohud"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   (app-powerd-rule-configuration
    (id "wine-proton")
    (match (app-powerd-match-configuration
            (cmdline-regex "wine|proton")))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   ;; CAD / 3D / Engineering
   (app-powerd-rule-configuration
    (id "cad-3d")
    (match (app-powerd-match-configuration
            (executable '("freecad" "openscad" "prusa-slicer" "PrusaSlicer"
                          "cura" "kicad" "kicad-wayland"))))
    (policy (app-powerd-policy-configuration
             (use-profile "ignore"))))

   ;; Electron generic fallback
   (app-powerd-rule-configuration
    (id "electron-generic")
    (match (app-powerd-match-configuration
            (cmdline-regex "electron")))
    (policy (app-powerd-policy-configuration
             (use-profile "throttle")
             (cpu-quota "30%"))))))

;;;
;;; Service implementation
;;;

(define (add-app-powerd-package config)
  (list (home-app-powerd-configuration-package config)))

(define (add-xsession-component config)
  "app-powerd run >$XDG_STATE_HOME/log/app-powerd.log 2>&1 &")

(define (add-app-powerd-xdg-config-file config)
  (list
   (list "app-powerd/config.yaml"
         (computed-file
          "app-powerd-config.yaml"
          #~(call-with-output-file #$output
              (lambda (port)
                (display #$(serialize-configuration config) port)))))))

(define home-app-powerd-service-type
  (service-type
   (name 'home-app-powerd)
   (extensions
    (list
     (service-extension home-profile-service-type add-app-powerd-package)
     (service-extension home-xsession-service-type add-xsession-component)
     (service-extension home-xdg-configuration-files-service-type
                        add-app-powerd-xdg-config-file)))
   (default-value (home-app-powerd-configuration))
   (description "Install app-powerd and configure it for managing background GUI applications.")))
