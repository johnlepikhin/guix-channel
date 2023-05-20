;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2019, 2020 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2016, 2017 John Darrington <jmd@gnu.org>
;;; Copyright © 2014-2022 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2014 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2014 Mathieu Lirzin <mathieu.lirzin@openmailbox.org>
;;; Copyright © 2015–2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015-2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015 Fabian Harfert <fhmgufs@web.de>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2018, 2020, 2021 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016-2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2018, 2019, 2020, 2021 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017, 2019, 2022 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Dave Love <me@fx@gnu.org>
;;; Copyright © 2018, 2019, 2020, 2021, 2022 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
;;; Copyright © 2018 Nadya Voronova <voronovank@gmail.com>
;;; Copyright © 2018 Adam Massmann <massmannak@gmail.com>
;;; Copyright © 2018, 2020-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2018 Eric Brown <brown@fastmail.com>
;;; Copyright © 2018, 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018 Amin Bandali <bandali@gnu.org>
;;; Copyright © 2019, 2021, 2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019 Steve Sprang <scs@stevesprang.com>
;;; Copyright © 2019 Robert Smith <robertsmith@posteo.net>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020–2023 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2020 R Veera Kumar <vkor@vkten.in>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2020 B. Wilson <elaexuotee@wilsonb.com>
;;; Copyright © 2020, 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2021 Gerd Heber <gerd.heber@gmail.com>
;;; Copyright © 2021 Franck Pérignon <franck.perignon@univ-grenoble-alpes.fr>
;;; Copyright © 2021 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2021, 2022 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2021 Ivan Gankevich <i.gankevich@spbu.ru>
;;; Copyright © 2021 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Pierre-Antoine Bouttier <pierre-antoine.bouttier@univ-grenoble-alpes.fr>
;;; Copyright © 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2022 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2022 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2022 Marek Felšöci <marek@felsoci.sk>
;;; Copyright © 2022 vicvbcun <guix@ikherbers.com>
;;; Copyright © 2022, 2023 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2022 Maximilian Heisinger <mail@maxheisinger.at>
;;; Copyright © 2022 Akira Kyle <akira@akirakyle.com>
;;; Copyright © 2022 Roman Scherer <roman.scherer@burningswell.com>
;;; Copyright © 2023 Jake Leporte <jakeleporte@outlook.com>
;;; Copyright © 2023 Evgenii Lepikhin <johnlepikhin@gmail.com>
;;;
;;; This file is part of GNU Guix.
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
(define-module (johnlepikhin packages frama-c)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages ocaml)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system ocaml)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public frama-c
  (package
    (name "frama-c")
    (version "24.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://frama-c.com/download/frama-c-"
                                  version "-Chromium.tar.gz"))
              (sha256
               (base32
                "0x1xgip50jdz1phsb9rzwf2ra8lshn1hmd9g967xia402wrg3sjf"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f; no test target in Makefile
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'export-shell
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CONFIG_SHELL"
                     (search-input-file inputs "/bin/sh")))))))
    (inputs
     (list gmp
           zlib))
    (propagated-inputs
     (list ocaml-biniou
           ocaml-easy-format
           ocaml-graph
           ocaml-yojson
           ocaml-zarith
           ocaml-lablgtk3-sourceview3
           lablgtk3
           why3))

    (native-search-paths
     (list (search-path-specification
            (variable "FRAMAC_SHARE")
            (files '("share/frama-c"))
            (separator #f))
           (search-path-specification
            (variable "FRAMAC_LIB")
            (files '("lib/frama-c"))
            (separator #f))))
    (home-page "https://frama-c.com")
    (synopsis "C source code analysis platform")
    (description "Frama-C is an extensible and collaborative platform dedicated
to source-code analysis of C software.  The Frama-C analyzers assist you in
various source-code-related activities, from the navigation through unfamiliar
projects up to the certification of critical software.")
    (license license:lgpl2.1+)))
