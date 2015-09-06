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

;;;; css.lisp

(in-package #:p99-patcher)

;;; "p99-patcher" goes here. Hacks and glory await!

(defun page-css ()
  "Deliver the necessary CSS"
  (css-lite:css
    (("body")
     (:background "#333"
                  :color "#fff"
                  :font-family "monospace"
                  :padding-bottom "100px"))
    (("a")
     (:color "lime"
             :font-weight "bold"
             :text-decoration "none"))
    (("a:hover")
     (:text-decoration "underline"))
    (("#main")
     (:background "#000"
                  :border "2px solid #fff"
                  :border-radius "20px"
                  :box-shadow "0px 0px 60px #000"
                  :color "#fff"
                  :margin "auto"
                  :margin-top "20px"
                  :margin-bottom "60px"
                  :text-align "center"
                  :padding "8px 30px 8px 30px"
                  :position "relative"
                  :width "700px"
                  :z-index "1"))
    (("#widgets")
     (:background "rgba(0,0,0,.5)"
                  :margin "-10px 0px 0px 0px"
                  :position "relative"
                  :text-align "center"
                  :width "100%"
                  :z-index "9"))
    ((".async")
     (:background "#999"
                  :border-radius "3px"
                  :box-shadow "3px 3px 6px #000"
                  :border "2px solid #666"
                  :color "#fff"
                  :display "inline-block"
                  :margin "10px"
                  :text-decoration "none"
                  :opacity ".7"
                  :padding "10px"
                  :width "40%"))
    (("#message")
     (:background "rgba(0,0,0,.5)"
                  :border-bottom "2px solid #fff"
                  :border-top "2px solid #fff"
                  :box-shadow "0px 3px 6px #000"
                  :color "#fff"
                  :font-family "sans-serif"
                  :font-size "20px"
                  :display "none"
                  :left "100px"
                  :padding "4px"
                  :position "absolute"
                  :text-align "center"
                  :top "400px"
                  :width "560px"))
    ((".loadscreen")
     (:background "#000"
                  :height "480px"
                  :width "640px"))
    (("#loadbar")
     (:background "#666"
                  :border "2px solid #333"
                  :font-weight "bold"
                  :height "16px"
                  :position "relative"
                  :width "100%"))
    (("#loadfill")
     (:background "yellow"
                  :height "16px"
                  :left "0px"
                  :opacity ".8"
                  :position "absolute"
                  :top "0px"
                  :width "100%"
                  :z-index "0"))
    (("#status")
     (:color "#000"
             :font-size "8px"
             :left "4px"
             :line-height "16px"
             :text-align "left"
             :position "absolute"
             :top "0px"
             :width "100%"
             :z-index "3"))
    (("#download-percent-done")
     (:background "lime"
                  :color "#000"
                  :display "none"
                  :left "0px"
                  :height "8px"
                  :top "8px"
                  :position "absolute"
                  :width "100%"))
    (("#loadpercent")
     (:color "#000"
             :left "0px"
             :line-height "16px"
             :position "absolute"
             :top "0px"
             :width "100%"
             :z-index "3"))
    ((".mod-description")
     (:color "#999"
             :font-size "10px"))
    (("#path-info")
     (:background "#000"
                  :color "#fff"
                  :font-size "9px"
                  :left "0px"
                  :padding-bottom "10px"
                  :position "fixed"
                  :bottom "0px"
                  :width "100%"
                  :z-index "10"))
    (("#path")
     (:background "#fff"
                  :border-radius "5px"
                  :color "red"
                  :display "inline-block"
                  :font-weight "bold"
                  :margin "9px"
                  :padding "4px"))
    ((".async:hover")
     (:background "#333"
                  :box-shadow "inset 3px 3px 6px #000"
                  :opacity "1"
                  :text-decoration "none"
                  :text-shadow "0px 0px 9px #fff"))
    (("#map-container")
     (:background "#666"
                  :margin "auto"))
    (("#map-instructions")
     (:background "rgba(0,155,50,.6)"
                  :position "fixed"
                  :top "100px"
                  :right "0px"
                  :padding "10px"
                  :width "150px"))
    (("#map")
     (:background "#eee"))
    (("#px, #py")
     (:width "20px"))
    (("#map-stuff")
     (:background "#333")
     (("canvas")
      (:border-radius "40px"
                      :box-shadow "6px 6px 12px #000"))
     (("input")
      (:border-radius "10px"
                      :margin "10px"
                      :padding "2px"
                      :text-align "center"
                      :font-size "10px")))
    ))
