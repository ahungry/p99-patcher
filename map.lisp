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

;;;; map.lisp

(in-package #:p99-patcher)

;;; "p99-patcher" goes here. Hacks and glory await!

(defparameter *sync-url* "http://ahungry.com:4444")
(defparameter *zone-name* (make-hash-table :test 'equal))
(defparameter *friends* (make-hash-table :test 'equal))
(defparameter *player* '())

(defun map-populate-zone-names ()
  "Fill out the zone-name hash table with data from map-zone-names.lisp"
  (loop for x from 0 to (length *zone-name-map*)
     by 2 do (setf (gethash (nth x *zone-name-map*) *zone-name*)
                   (nth (1+ x) *zone-name-map*))))

(defun map-read-file (map-name)
  "Read the map out of the directory if it exists"
  (with-open-file (s (merge-pathnames
                      (format nil "maps/~a.txt" map-name)
                      *eq-path*) :direction :input
                      :if-does-not-exist nil)
    (when s (loop for line = (read-line s nil 'eof)
               until (eq line 'eof)
               collect line))))

(defun map-parse-line (line)
  "Parse out an individual map line"
  (let ((type (subseq line 0 1))
        (coords (split-sequence #\, (subseq line 1))))
    (setf coords
          (mapcar (lambda (coord)
                    (string-trim " " coord)) coords))
    (cons type coords)))

(defun map-parse-file (map-data)
  "Run across the map data and parse into lists"
  (mapcar #'map-parse-line map-data))

(defun map-log-read-optimized (log-name total-lines)
  "Only concerned with the last 5000 lines or so, if no /loc
within that time frame, forget it"
  (with-open-file (s log-name :if-does-not-exist nil)
    (when s
      (let ((lines (loop for line = (read-line s nil 'eof)
                      until (eq line 'eof)
                      when (cl-ppcre:scan "(Your Location|You have entered|You think you)" line)
                      collect (subseq line 0 (- (length line) 1))))) ;; Strip off Windows newline
        (if (> (length lines) total-lines)
            (subseq lines (- (length lines) total-lines))
            lines))))) ;; Only send back the last X amount of lines

(defun map-last-loc (log-name)
  "Pull out the last found location match from the log file"
  (let ((last-coords #("0" "0" "0"))
        (last-zone #("East Commonlands"))
        (last-dir #("North"))
        (lines (map-log-read-optimized log-name 10000)))
    (loop for line in lines
       do (let ((coords (nth-value 1 (cl-ppcre:scan-to-strings
                                      "Your Location is (.+), (.+), (.+)"
                                      line)))
                (zone (nth-value 1 (cl-ppcre:scan-to-strings
                                    "You have entered (.*)\."
                                    line)))
                (direction (nth-value 1 (cl-ppcre:scan-to-strings
                                         "You think you are heading (.*)\."
                                         line))))
            (when coords (setf last-coords coords))
            (when zone (setf last-zone zone))
            (when direction (setf last-dir direction))))
    (list :coords last-coords
          :zone-name last-zone
          :zone (gethash (aref last-zone 0) *zone-name*)
          :direction last-dir)))

(defun map-newest-log-file ()
  "Find the newest log file in the log dir, also don't leave out the
PVP players"
  (let ((files
         (append (directory (merge-pathnames #P"Logs/*_project1999.*" *eq-path*))
                 (directory (merge-pathnames #P"Logs/*_P1999PVP.*" *eq-path*)))))
    (when files
      (let ((newest (car files)))
        (loop for file in files
           when (> (file-write-date file)
                   (file-write-date newest))
           do (setf newest file))
        newest))))

(defun map-player-name ()
  "Populate the player name"
  (cadr (split-sequence #\_ (file-namestring (map-newest-log-file)))))

(defun map-player-loc ()
  "Get the player's location and add it to the map"
  (let* ((location (map-last-loc (map-newest-log-file)))
         (coords (getf location :coords)))
    (setf (getf location :coords)
          (list "C"
                (* -1 (read-from-string (aref coords 1)))
                (* -1 (read-from-string (aref coords 0)))
                (read-from-string (aref coords 2))
                0 0 0 0 (map-player-name)))
    (setf *player* location)
    location))

(defun map-player-loc-json-friendly ()
  "Get the #'map-player-loc data and alist it from the plist"
  (let ((loc (map-player-loc)))
    (list
     (cons :coords (getf loc :coords))
     (cons :zone-name (aref (getf loc :zone-name) 0))
     (cons :zone (getf loc :zone))
     (cons :direction (aref (getf loc :direction) 0)))))

(defun map-friends-loc ()
  "Pull in our friend locations"
  (let ((friends (loop for friend being the hash-values of *friends*
                    collect friend)))
        ;; (player (or *player* (map-player-loc))))
    (loop for friend in friends
       ;; removing the filter for now, see if we need later
       ;; when (equal (getf player :zone) (nth 4 friend))
       collect friend)))

(defun map-get (map-name)
  "Check which map we want to return to the user"
  (let ((map-data (map-read-file map-name)))
    (cl-json:encode-json-to-string
     (cons (list "C" 0 0 0 0 0 0 0 "YOU")
           (map-parse-file map-data)))))

(defun map-share-loc ()
  "Send player's current loc to another location"
  (let ((player (or *player* (map-player-loc))))
    (drakma:http-request
     (format nil "~a/map-receive-loc/" *sync-url*)
     :method :post
     :parameters (list (cons "name" (stringify (nth 8 (getf player :coords))))
                       (cons "x" (stringify (nth 1 (getf player :coords))))
                       (cons "y" (stringify (nth 2 (getf player :coords))))
                       (cons "zone" (stringify (getf player :zone)))))))

(defun map-receive-loc (name x y zone)
  "Receive the player coordinates posted in"
  (let ((friend (list "F" x y 0 zone 0 0 0 name)))
    (setf (gethash name *friends*) friend)
    (map-friends-loc)))

(defun map-sync ()
  "This is where the magic happens, send our location to share with
the remote end, in exchange we receive the location of everyone else
and can store it for our use as well (and we're added to the data set
to boot)"
  (let ((friends (cl-json:decode-json-from-string (map-share-loc))))
    (loop for friend in friends
       do (setf (gethash (nth 8 friend) *friends*) friend))))

(defun map-js ()
  "Generate the relevant map js"
  (ps
    (defvar *can* nil)
    (defvar *ctx* nil)
    (defvar *coords* [])
    (defvar *x-scale* .5)
    (defvar *y-scale* .5)
    (defvar *x-offset* 400)
    (defvar *y-offset* 400)
    (defvar *player* (create coords []
                             zone ""
                             zone-name ""
                             direction "North"))
    (defvar *friends* [])

    (defun get-player-map-coords ()
      "Request active player's position"
      (chain $ (ajax
                (create
                 data ""
                 type "post"
                 url "/get-map-player/"
                 success (lambda (data)
                           (let ((player data)) ;;(eval (+ "(" data ")"))))
                             (setf (aref *coords* 0) (@ player coords))
                             (unless (eq (@ player zone)
                                         (@ *player* zone))
                               (get-active-map-coords (@ player zone)))
                             (chain ($ "#zone-name") (val (@ player zone-name)))
                             (chain ($ "#zone-file") (val (@ player zone)))
                             ;;(setf (@ window location) (+ "/mapper/?map-name=" (@ player zone))))
                             (setf *player* player)
                             (set-timeout #'get-player-map-coords 10000)))
                 ))))

    (defun map-sync ()
      "Request friend's positions from remote end"
      (chain $ (ajax
                (create
                 data ""
                 type "post"
                 url "/map-sync/"
                 success (lambda (data)
                           (set-timeout #'get-friend-map-coords 500)))
                 )))

    (defun get-friend-map-coords ()
      "Request friend's positions"
      (chain $ (ajax
                (create
                 data ""
                 type "post"
                 url "/get-map-friends/"
                 success (lambda (data)
                           (let ((friends data))
                             (setf *friends* friends)
                             (set-timeout #'map-sync 10000)))
                 ))))

    (defun get-active-map-coords (map-name)
      "Pull out map coords for the active map"
      (chain $ (ajax
                (create
                 data (+ "map-name=" map-name)
                 type "post"
                 url "/get-map/"
                 success (lambda (data)
                           (setf *coords*
                                 (eval (+ "(" data ")")))
                           (init-map))
                 ))))

    (defun init-map ()
      "Create initial canvas object and draw our lines"
      (setf *can* (chain document (get-element-by-id "map"))
            *ctx* (chain *can* (get-context "2d")))
      (setf (@ *ctx* stroke-style) "rgba(0,100,0,0,.3)"
            (@ *ctx* line-width) 0.5)
      (setf (@ *can* width) 800
            (@ *can* height) 600)
      (bind-keys)
      (draw-map))

    (defun draw-map ()
      "Draw the appropriate lines"
      (if *coords*
          (progn
            (chain *ctx* (clear-rect 0 0 800 800))
            (chain *ctx* (save)) ;; Save map state
            (chain *ctx* (scale *x-scale* *y-scale*))
            (chain *ctx* (translate *x-offset* *y-offset*))
            (loop for line in *coords*
               do (if (= (aref line 0) "L")
                      (draw-line (aref line 1)
                                 (aref line 2)
                                 (aref line 4)
                                 (aref line 5))
                      (draw-object (aref line 0)
                                   (aref line 1)
                                   (aref line 2)
                                   (aref line 8))))
            (loop for friend in *friends*
               unless (equal (aref friend 8)
                             (aref (@ *player* coords) 8))
               when (equal (aref friend 4)
                           (aref (@ *player* coords) 4))
               do (draw-object (aref friend 0)
                               (aref friend 1)
                               (aref friend 2)
                               (aref friend 8)
                               "0,0,255"))
            (chain *ctx* (restore))
            (set-timeout #'draw-map 100))
          (get-active-map-coords)))

    (defun draw-line (sx sy dx dy)
      "Draw a line given source/destination"
      (setf (@ *ctx* stroke-style) "#000000")
      (chain *ctx* (begin-path))
      (chain *ctx* (move-to sx sy))
      (chain *ctx* (line-to dx dy))
      (chain *ctx* (stroke)))

    (defun draw-object (type sx sy name &optional color)
      "Draw an important object on map"
      (let ((color (cond (color color)
                         ((eq type "P") "255,0,0") ;; NPC
                         (t "0,255,0")))           ;; Player
            (font-size (/ 11 *x-scale*)))
        (setf (@ *ctx* fill-style) (+ "rgba(" color ",.3)")
              (@ *ctx* font) (+ "bold " font-size "px mono"))
        (chain *ctx* (fill-rect sx sy 10 10))
        (when (eq type "C")
          (let ((dirlen (/ 60 *x-scale*)))
            (setf (@ *ctx* stroke-style) (+ "rgba(" color ",1)"))
            (chain *ctx* (save))
            (chain *ctx* (translate sx sy))
            (chain *ctx* (begin-path))
            (chain *ctx* (move-to 0 0))
            (with-slots (direction) *player*
              (cond ((eq "North" direction) (chain *ctx* (line-to 0 (- sy dirlen))))
                    ((eq "South" direction) (chain *ctx* (line-to 0 (+ sy dirlen))))
                    ((eq "West" direction) (chain *ctx* (line-to (- sx dirlen) 0)))
                    ((eq "East" direction) (chain *ctx* (line-to (+ sx dirlen) 0)))
                    ((eq "NorthEast" direction) (chain *ctx* (line-to (+ sx dirlen) (- sy dirlen))))
                    ((eq "NorthWest" direction) (chain *ctx* (line-to (- sx dirlen) (- sy dirlen))))
                    ((eq "SouthEast" direction) (chain *ctx* (line-to (+ sx dirlen) (+ sy dirlen))))
                    ((eq "SouthWest" direction) (chain *ctx* (line-to (- sx dirlen) (+ sy dirlen))))
                    (t)))
            (chain *ctx* (stroke))
            (chain *ctx* (restore))))
        (setf (@ *ctx* fill-style) (+ "rgba(" color ",.8)"))
        (chain *ctx* (fill-text name sx sy))))

    (defun bind-keys ()
      "Grab the key event code"
      (setf (@ document onkeydown)
            (lambda (e)
              (let ((e (or e (@ window event))))
                (down-keys (@ e key-code)))))
      (setf (@ document onkeyup)
            (lambda (e)
              (let ((e (or e (@ window event))))
                (up-keys (@ e key-code))))))

    (defun down-keys (k)
      "Swap based on keybind pressed"
      (cond
        ((eq k 38) (incf *y-offset* (/ 50 *y-scale*))) ;; up
        ((eq k 87) (incf *y-offset* (/ 50 *y-scale*))) ;; w
        ((eq k 75) (incf *y-offset* (/ 50 *y-scale*))) ;; k
        ((eq k 40) (decf *y-offset* (/ 50 *y-scale*))) ;; down
        ((eq k 83) (decf *y-offset* (/ 50 *y-scale*))) ;; s
        ((eq k 74) (decf *y-offset* (/ 50 *y-scale*))) ;; j
        ((eq k 37) (incf *x-offset* (/ 50 *x-scale*))) ;; left
        ((eq k 65) (incf *x-offset* (/ 50 *x-scale*))) ;; a
        ((eq k 72) (incf *x-offset* (/ 50 *x-scale*))) ;; h
        ((eq k 39) (decf *x-offset* (/ 50 *x-scale*))) ;; right
        ((eq k 68) (decf *x-offset* (/ 50 *x-scale*))) ;; d
        ((eq k 76) (decf *x-offset* (/ 50 *x-scale*))) ;; l
        ((eq k 67) (center-on-player))                 ;; c
        ((eq k 73) (progn (setf *x-scale* (* *x-scale* 2)        ;; i
                                *y-scale* (* *y-scale* 2))))
        ((eq k 79) (progn (setf *x-scale* (/ *x-scale* 2)        ;; o
                                *y-scale* (/ *y-scale* 2))))
        ))

    (defun up-keys (k)
      "Swap based on key raised"
      (return-from up-keys))

    (defun coord-override ()
      "Override any auto read in coordinates with what the user inputs"
      (let ((x (chain ($ "#px") (val)))
            (y (chain ($ "#py") (val))))
        (when (and x y)
          (setf (aref *coords* 0 1) (* -1 x)
                (aref *coords* 0 2) (* -1 y)))))

    (defun center-on-player ()
      "Center the map on the player character"
      (setf *x-offset* (* (aref *coords* 0 2) *x-scale* 5)
            *y-offset* (* (aref *coords* 0 1) *y-scale* 3)))

    (chain ($ document)
           (ready
            (lambda ()
              (set-timeout #'get-player-map-coords 1000)
              (set-timeout #'map-sync 1000)
              (set-interval #'coord-override 100))))
    ))

(defun mapper (&optional map-name)
  "Find the active map/zone and the latest coords"
  (with-html-output-to-string (s nil :prologue t :indent t)
    (htm
     (:html
      (:head
       (:link :href "/css/main.css" :type "text/css" :rel "stylesheet")
       (:script :src "/js/jquery-1.10.2.min.js" :type "text/javascript")
       (:script :src "/js/map.js" :type "text/javascript")
       (:script :type "text/javascript"
                "$(document).ready(function() { getActiveMapCoords('" (str map-name) "'); });")
       )
      (:body
       (:div :id "map-stuff"
             "Y: " (:input :id "py" :val "")
             "X: " (:input :id "px" :val "")
             "Zone Name: " (:input :id "zone-name" :val "")
             "Zone File: " (:input :id "zone-file" :val "")
             (:div :id "map-container"
                   (:div :id "map-instructions"
                         (:p "c - center map on character (sort of works)")
                         (:p "i - zoom in")
                         (:p "o - zoom out")
                         (:p "wasd/hjkl/arrows - move map up/down/left/right"))
                   (:br)
                   (:canvas :id "map"))))))))
