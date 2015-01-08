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

;;;; js.lisp

(in-package #:p99-patcher)

;;; "p99-patcher" goes here. Hacks and glory await!

(defun page-js ()
  "Deliver the necessary JS"
  (parenscript:ps

    (defun init ()
      (set-interval (lambda ()
                      (chain $ (ajax (create url "/download-percent-done/"
                                             type "get"
                                             success (lambda (res)
                                                       (if (>= res 100)
                                                           (chain ($ "#download-percent-done")
                                                                  (hide))
                                                           (chain ($ "#download-percent-done")
                                                                  (show)))
                                                       (chain ($ "#download-percent-done")
                                                              (animate (create width (+ res "%")) 90)))))))
                    100)
      (set-interval (lambda ()
                      (chain $ (ajax (create url "/status/"
                                             type "get"
                                             success (lambda (res)
                                                       (chain ($ "#status")
                                                              (html res)))))))
                    100)
      (set-interval (lambda ()
                      (chain $ (ajax (create url "/percent-done/"
                                             type "get"
                                             success (lambda (res)
                                                       (chain ($ "#loadfill")
                                                              (animate (create width (+ res "%")) 90))
                                                       (chain ($ "#loadpercent")
                                                              (html (+ res "%"))))))))
                    100)
      (chain
       ($ ".async")
       (click (lambda ()
                (let ((uri (chain ($ this) (attr "href"))))
                  (chain $
                         (ajax (create
                                url uri
                                type "post"
                                success (lambda (res)
                                          (chain ($ "#message")
                                                 (html res)
                                                 (fade-in 500))
                                          (set-timeout
                                           (lambda () (chain ($ "#message")
                                                             (fade-out 500)))
                                           8000)
                                          )
                                )))
                  false)))))

    (chain ($ document) (ready (lambda () (init))))))
