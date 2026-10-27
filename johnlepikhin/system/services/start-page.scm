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

;;; A static start page listing the services of a network, served by nginx,
;;; so that their addresses live in one place.

(define-module (johnlepikhin system services start-page)
  #:use-module (gnu services)
  #:use-module (gnu services web)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (sxml simple)
  #:use-module (srfi srfi-1)
  #:export (start-page-link
            start-page-link?
            start-page-section
            start-page-section?
            start-page-configuration
            start-page-configuration?
            start-page-service-type))

(define-record-type* <start-page-link>
  start-page-link
  make-start-page-link
  start-page-link?
  (title        start-page-link-title)             ;string
  ;; #f for an entry without a web interface, e.g. one reached by an app.
  (url          start-page-link-url
                (default #f))
  (description  start-page-link-description
                (default #f))
  (icon         start-page-link-icon               ;an emoji
                (default #f)))

(define-record-type* <start-page-section>
  start-page-section
  make-start-page-section
  start-page-section?
  (title  start-page-section-title)
  (links  start-page-section-links))               ;list of <start-page-link>

(define-record-type* <start-page-configuration>
  start-page-configuration
  make-start-page-configuration
  start-page-configuration?
  (title         start-page-configuration-title
                 (default "Home"))
  (language      start-page-configuration-language
                 (default "en"))
  (server-names  start-page-configuration-server-names
                 (default '()))
  (listen        start-page-configuration-listen
                 (default '("80")))
  (sections      start-page-configuration-sections
                 (default '())))

;; sxml->xml escapes the text of the style element, which HTML does not
;; unescape there: no "<", ">" or "&" in it.
(define %style "
:root { color-scheme: light dark; --bg: #f6f7f9; --card: #ffffff;
  --text: #1d2125; --muted: #5f6b76; --border: #dde2e7; --accent: #2f6fdf; }
@media (prefers-color-scheme: dark) {
  :root { --bg: #15181c; --card: #1e2227; --text: #e6e9ec; --muted: #9aa5b0;
    --border: #2d333a; --accent: #7aa7ff; } }
* { box-sizing: border-box; }
body { margin: 0; padding: 24px 16px 48px; background: var(--bg);
  color: var(--text); font: 16px/1.4 system-ui, sans-serif; }
main { max-width: 960px; margin: 0 auto; }
h1 { font-size: 28px; margin: 0 0 24px; }
h2 { font-size: 15px; font-weight: 600; text-transform: uppercase;
  letter-spacing: .04em; color: var(--muted); margin: 28px 0 10px; }
ul { list-style: none; margin: 0; padding: 0; display: grid; gap: 10px;
  grid-template-columns: repeat(auto-fill, minmax(260px, 1fr)); }
li a, li div.entry { display: flex; gap: 12px; align-items: center;
  padding: 12px 14px; background: var(--card); border: 1px solid var(--border);
  border-radius: 10px; color: inherit; text-decoration: none; height: 100%; }
li a:hover { border-color: var(--accent); }
.icon { font-size: 24px; width: 32px; text-align: center; flex: none; }
.title { font-weight: 600; }
li a .title { color: var(--accent); }
.description { color: var(--muted); font-size: 14px; overflow-wrap: anywhere; }
")

(define (link->sxml link)
  (match-record link <start-page-link> (title url description icon)
    (let ((body `((span (@ (class "icon")) ,(or icon ""))
                  (span (span (@ (class "title")) ,title)
                        ,@(if description
                              `((br) (span (@ (class "description"))
                                           ,description))
                              '())))))
      `(li ,(if url
                `(a (@ (href ,url)) ,@body)
                `(div (@ (class "entry")) ,@body))))))

(define (start-page-html config)
  (match-record config <start-page-configuration> (title language sections)
    (plain-file
     "index.html"
     (string-append
      "<!DOCTYPE html>\n"
      (call-with-output-string
        (lambda (port)
          (sxml->xml
           `(html (@ (lang ,language))
                  (head (meta (@ (charset "utf-8")))
                        (meta (@ (name "viewport")
                                 (content "width=device-width, initial-scale=1")))
                        (title ,title)
                        (style ,%style))
                  (body
                   (main
                    (h1 ,title)
                    ,@(append-map
                       (lambda (section)
                         `((h2 ,(start-page-section-title section))
                           (ul ,@(map link->sxml
                                      (start-page-section-links section)))))
                       sections))))
           port)))))))

(define (start-page-nginx-servers config)
  (match-record config <start-page-configuration> (server-names listen)
    (list (nginx-server-configuration
           (listen listen)
           (server-name server-names)
           (root (file-union "start-page"
                             `(("index.html" ,(start-page-html config)))))
           (index '("index.html"))))))

(define start-page-service-type
  (service-type
   (name 'start-page)
   (extensions
    (list (service-extension nginx-service-type start-page-nginx-servers)))
   (default-value (start-page-configuration))
   (description "Serve a static start page with links to the services of a
network, grouped in sections.")))
