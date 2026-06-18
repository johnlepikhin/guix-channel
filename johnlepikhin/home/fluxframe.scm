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

;; Home Shepherd service that runs `fluxframe run --preset NAME' as a
;; per-user long-running daemon.  No `user' field exists on the
;; configuration record: a home service always runs as the calling
;; user.  For a system-wide variant the same record + start procedure
;; can be reused with `shepherd-service' wired under
;; `shepherd-root-service-type'.

(define-module (johnlepikhin home fluxframe)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (johnlepikhin packages fluxframe)
  #:export (home-fluxframe-configuration
            home-fluxframe-configuration?
            home-fluxframe-service-type))

(define-record-type* <home-fluxframe-configuration>
  home-fluxframe-configuration make-home-fluxframe-configuration
  home-fluxframe-configuration?
  (package           home-fluxframe-configuration-package
                     (default fluxframe))
  ;; `#f' → let fluxframe's XDG resolver pick
  ;; $XDG_CONFIG_HOME/fluxframe/fluxframe.toml.  Absolute path or a
  ;; g-expression resolving to one → passed via `--config'.  Never
  ;; combined with `--no-default-config' (TZ R4).
  (config-file       home-fluxframe-configuration-config-file
                     (default #f))
  (preset            home-fluxframe-configuration-preset
                     (default "default"))
  ;; `#f' → cwd of the daemon defaults to $HOME at runtime.  Set to an
  ;; absolute path when the preset's TOML references models via
  ;; relative paths (`model = "models/foo.onnx"').
  (working-directory home-fluxframe-configuration-working-directory
                     (default #f))
  (extra-arguments   home-fluxframe-configuration-extra-arguments
                     (default '()))
  ;; When `#t' export FLUXFRAME_FORCE_BLUR_BACKEND=cpu.  Use on hosts
  ;; without a working Vulkan loader (headless boxes, containers).
  (force-cpu-blur?   home-fluxframe-configuration-force-cpu-blur?
                     (default #f))
  ;; `#f' → $XDG_STATE_HOME/log/fluxframe.log (or
  ;; $HOME/.local/state/log/fluxframe.log when XDG_STATE_HOME is unset).
  (log-file          home-fluxframe-configuration-log-file
                     (default #f)))

(define (add-fluxframe-packages config)
  (list (home-fluxframe-configuration-package config)))

(define (add-fluxframe-activation config)
  "Create the log-file's parent directory and (optionally) the
working directory at `guix home reconfigure' time.  Shepherd's
`make-forkexec-constructor' does not auto-create the log file's
parent, so on a fresh machine the first start would otherwise fail
with ENOENT."
  (let ((log-file          (home-fluxframe-configuration-log-file config))
        (working-directory (home-fluxframe-configuration-working-directory config)))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (let* ((home (getenv "HOME"))
                 (xdg-state (or (getenv "XDG_STATE_HOME")
                                (string-append home "/.local/state")))
                 (log (or #$log-file
                          (string-append xdg-state "/log/fluxframe.log"))))
            (mkdir-p (dirname log))
            #$(if working-directory
                  #~(mkdir-p #$working-directory)
                  #~#t))))))

(define (add-fluxframe-shepherd-service config)
  (let* ((package           (home-fluxframe-configuration-package config))
         (config-file       (home-fluxframe-configuration-config-file config))
         (preset            (home-fluxframe-configuration-preset config))
         (working-directory (home-fluxframe-configuration-working-directory config))
         (extra-arguments   (home-fluxframe-configuration-extra-arguments config))
         (force-cpu-blur?   (home-fluxframe-configuration-force-cpu-blur? config))
         (log-file          (home-fluxframe-configuration-log-file config)))
    (list
     (shepherd-service
      (provision '(fluxframe))
      (documentation
       (string-join
        '("Run fluxframe video processing daemon (long-running)."
          ""
          "Config lookup:"
          "  --config <path> when `config-file' is set on the service record;"
          "  otherwise $XDG_CONFIG_HOME/fluxframe/fluxframe.toml (XDG"
          "  default; fluxframe's own resolver)."
          ""
          "Control socket:"
          "  $XDG_RUNTIME_DIR/fluxframe.sock when XDG_RUNTIME_DIR is set,"
          "  /tmp/fluxframe.sock otherwise (mode 0600).  This is what"
          "  fluxframe-gui connects to."
          ""
          "Models:"
          "  Paths under `model = …' in a preset are resolved relative to"
          "  the daemon's working directory (`#:directory' here) OR taken"
          "  as absolute.  Set `working-directory' on the service when a"
          "  preset uses relative model paths."
          ""
          "Dlopen libraries:"
          "  ORT_DYLIB_PATH, OPENVINO_INSTALL_DIR and OCL_ICD_VENDORS"
          "  are baked into the `fluxframe' wrapper at package build"
          "  time — not set here.  See"
          "  johnlepikhin/packages/fluxframe.scm `wrap-binaries' phase."
          ""
          "Known quirk:"
          "  v4l2loopback's `keep_format=1' breaks idle-mode format"
          "  renegotiation — do not enable that kernel-module option.")
        "\n"))
      (respawn? #t)
      ;; 3 restarts inside 30 s, then shepherd gives up.  Without a cap
      ;; a systemic failure (missing model file, camera unplugged)
      ;; would fill the log file with crash-loops.
      (respawn-limit #~'(3 . 30))
      (start
       #~(make-forkexec-constructor
          (list #$(file-append package "/bin/fluxframe")
                "run" "--preset" #$preset
                #$@(if config-file (list "--config" config-file) '())
                #$@extra-arguments)
          ;; chdir before exec.  Falls back to $HOME when no explicit
          ;; working-directory was configured.
          #:directory
          (or #$working-directory (getenv "HOME"))
          #:log-file
          (or #$log-file
              (string-append
               (or (getenv "XDG_STATE_HOME")
                   (string-append (getenv "HOME") "/.local/state"))
               "/log/fluxframe.log"))
          ;; #:environment-variables REPLACES the child env.  Build it
          ;; by extending shepherd's own env (`(environ)') with our
          ;; overrides; place overrides FIRST so getenv finds them
          ;; before any inherited value of the same name.
          #:environment-variables
          (append
           (list
            ;; ORT_DYLIB_PATH, OPENVINO_INSTALL_DIR and OCL_ICD_VENDORS
            ;; come from the fluxframe wrapper itself — see TZ R1/R2 and
            ;; the `wrap-binaries' phase in the package recipe.
            (string-append "XDG_RUNTIME_DIR="
                           (or (getenv "XDG_RUNTIME_DIR")
                               (format #f "/run/user/~a" (getuid))))
            (string-append "XDG_CONFIG_HOME="
                           (or (getenv "XDG_CONFIG_HOME")
                               (string-append (getenv "HOME") "/.config")))
            (string-append "XDG_STATE_HOME="
                           (or (getenv "XDG_STATE_HOME")
                               (string-append (getenv "HOME") "/.local/state")))
            #$@(if force-cpu-blur?
                   '("FLUXFRAME_FORCE_BLUR_BACKEND=cpu")
                   '()))
           (environ))))
      (stop #~(make-kill-destructor))))))

(define home-fluxframe-service-type
  (service-type
   (name 'home-fluxframe)
   (extensions
    (list
     (service-extension home-profile-service-type
                        add-fluxframe-packages)
     (service-extension home-activation-service-type
                        add-fluxframe-activation)
     (service-extension home-shepherd-service-type
                        add-fluxframe-shepherd-service)))
   (default-value (home-fluxframe-configuration))
   (description "Run fluxframe (virtual-camera daemon) under shepherd
with explicit working directory, log file, XDG environment, and an
optional CPU-only blur backend.  Dlopen runtime paths
(ORT_DYLIB_PATH, OPENVINO_INSTALL_DIR, OCL_ICD_VENDORS) live in the
@code{fluxframe} package wrapper, not here.")))
