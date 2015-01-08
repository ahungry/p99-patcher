;; p99-patcher - A patcher/launcher for the project1999.org Everquest server
;; Copyright (C) 2013 Matthew Carter
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; p99-patcher.asd

(asdf:defsystem #:p99-patcher
  :serial t
  :description "A patcher/launcher for the project1999.org Everquest server"
  :author "Matthew Carter <m@ahungry.com>"
  :license "AGPLv3"
  :depends-on (#:cl-ppcre
               #:cl-fad
               #:bordeaux-threads
               #:hunchentoot
               #:cl-who
               #:cl-json
               #:drakma
               #:md5
               #:parenscript
               #:css-lite
               #:gzip-stream
               #:zip
               #:split-sequence)
  :components ((:file "package")
               (:file "js")
               (:file "css")
               (:file "map-zone-names")
               (:file "map")
               (:file "p99-patcher")))
