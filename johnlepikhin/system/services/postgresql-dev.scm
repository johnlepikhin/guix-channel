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
(define-module (johnlepikhin system services postgresql-dev)
  #:export (postgresql-dev-service))

(define-public postgresql-dev-service
  (service postgresql-service-type
           (postgresql-configuration
            ;; PostGIS compiled for PostgreSQL 14
            (postgresql postgresql-14)
            (extension-packages (list postgis))
            (config-file (postgresql-config-file (log-destination "stderr")
                                                 (hba-file (plain-file
                                                            "pg_hba.conf"
                                                            "
local   all all         trust
host    all all 127.0.0.1/32    md5
host    all all ::1/128     md5"))
                                                 (extra-config '(("work_mem"
                                                                  "500 MB")
                                                                 ("max_parallel_workers_per_gather"
                                                                  6))))))))
