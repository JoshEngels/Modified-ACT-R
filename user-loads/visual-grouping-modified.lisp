; ----------------------------------------------------------------------
; Begin file: visual-grouping.lisp
; ----------------------------------------------------------------------

;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : John K. Lindstedt
;;; Address     : Rice University, (???MS-25)
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : j.k.l@rice.edu
;;; 
;;; Copyright   : (c)2018 John K. Lindstedt
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : visual-grouping.lisp
;;; Version     : 0.9
;;; 
;;; Description : some
;;;             : important
;;;             : sentences.
;;; 
;;; Bugs        : - get global inheritance collision type piped into the inheritance collisions. Forgot this. 
;;;             
;;;
;;; Todo        : - rename methods to be more unique and related to their classes
;;;             : - test with more cases, for each of: box-collisions, inheritance
;;;             : - add some more hooks (:after?) to clean up the scene variables when ACT-R resets
;;;             : - want to do a bit of a rewrite to add a middle-layer object, vis-group
;;;             :    - ultimately results in "vis-scene containing vis-groups containing vis-points"
;;;             :    - makes a lot of calculations simpler and less repetitive
;;;             : - need a simple "inheritance threshold"
;;;             :    - currently a single collision triggers inheritance
;;;             :    - but may want some proportional overlap to determine this
;;;             : - implement 'merge' and 'split' behavior for group inheritance
;;;             :    - "new" (current default), "size-dominant", "overlap-dominant"
;;;             :    - likely where important errors will come from
;;;             : - add "color collide" function that can be switched on and off.
;;;             : - add some rudimentary form of hierarchy-- two or three layers of group
;;;             : - "subgroup", "group", "supergroup"
;;;             : - add support for N-back group inheritance
;;;             :    - empirical: how far back can group knowledge be retained before re-study?
;;; --- History ---
;;; 2018.03.19  jkl [0.95]
;;;             : added option to produce sequential group names for debugging
;;; 2018.02.14   author [0.9]
;;;             : First commit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;; ---------------------------------------------------------------------- ;;;;
;;;; section split


;;; def-name      [type (class, method, fun, etc)]
;;; Date        : 
;;; Description : say 
;;;             : some
;;;             : important
;;;             : words.

#|
comment out a block
|#




;parameters and options
(defparameter vg-glomming-radius 25)
(defparameter vg-collision-type 'box) ;box or point
(defparameter vg-naming-type 'generic) ;generic or sequential



;; A fully bottom-up visual-grouping system for the ACT-R visicon

;simple visual point object with slots for all visicon entries
(defclass vispoint ()
    (
        (id       :initarg :id       :accessor id        :initform NIL)
        (group-name    :initarg :group-name    :accessor group-name     :initform NIL)
        (size     :initarg :size     :accessor size      :initform NIL)
        (distance :initarg :distance :accessor distance  :initform NIL)
        (screen-x :initarg :screen-x :accessor screen-x  :initform NIL)
        (screen-y :initarg :screen-y :accessor screen-y  :initform NIL)
        (width    :initarg :width    :accessor width     :initform NIL)
        (height   :initarg :height   :accessor height    :initform NIL)
        (color    :initarg :color    :accessor color     :initform NIL)
        (value    :initarg :value    :accessor value     :initform NIL)
        (kind     :initarg :kind     :accessor kind      :initform NIL)
        (checked                     :accessor checked   :initform NIL)
        (group-ix :initarg :group-ix :accessor group-ix  :initform NIL)
        (visloc   :initarg :visloc   :accessor visloc    :initform NIL)
        (coord    :initarg :coord    :accessor coord     :initform NIL)
    )
)

;method stub for accepting a complete visloc object
(defmethod initialize-instance :after ((vg vispoint) &key)
  (if (visloc vg)
    NIL
  )
)

;calculates euclidean distance between two point objects
(defmethod vispoint-distance ((pt1 vispoint) (pt2 vispoint))
  (with-slots ((x1 screen-x) (y1 screen-y)) pt1
    (with-slots ((x2 screen-x) (y2 screen-y)) pt2
      (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))
    )
  )
)

;alternate version using coordinate lists
(defmethod vispoint-distance ((pt1 list) (pt2 list))
  (let ((x1 (first pt1)) (y1 (second pt1)))
    (let ((x2 (first pt2)) (y2 (second pt2)))
      (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))
    )
  )
)



(defmethod box-nearest-pt ((orig vispoint) (trgt vispoint))
  (let
    (
      (x (screen-x orig))
      (y (screen-y orig))
      (left   (if (width trgt)  (- (screen-x trgt) (/ (width trgt)  2)) 0))
      (right  (if (width trgt)  (+ (screen-x trgt) (/ (width trgt)  2)) 0))
      (bottom (if (height trgt) (- (screen-y trgt) (/ (height trgt) 2)) 0))
      (top    (if (height trgt) (+ (screen-y trgt) (/ (height trgt) 2)) 0))
    )
    (cond
      ;when centroid in diagonal quadrant, simply return nearest corner
      ((and (< x left) (< y bottom))
        (list left bottom))
      ((and (< x left) (> y top))
        (list left top))
      ((and (> x right) (< y bottom))
        (list right bottom))
      ((and (> x right) (> y top))
        (list right top))
      ;when center is adjacent quadrant, return nearest point along the edge
      ((and (< x left) (>= y bottom) (<= y top)) ;left edge
        (list left y))
      ((and (> x right) (>= y bottom) (<= y top)) ;right edge
        (list right y))
      ((and (> y top) (>= x left) (<= x right)) ;top edge
        (list x top))
      ((and (< y bottom) (>= x left) (<= x right)) ;bottom edge
        (list x bottom))
      ;default case, orig is inside the target box, return NIL and skip the further checks
      (T
        NIL)
    )
  )
)

;Desired additional types of collision detection:
;; color
;; kind
;; value
;; continuity bias
;; row or column bias (or is this merely a function of distance?)
;; accommodation for width and height (could just subtract half width in distance!)
;; collision with width and height:
; check collision inside point-box, 
; in boxes extended one radius out from each side of the point-box
; and each corner-point
; 
;detects a distance collision between two point objects within radius 'r'
(defmethod collide ((pt1 vispoint) (pt2 vispoint) radius &optional (mode 'box))
  (case mode
    (point
      (<= (vispoint-distance pt1 pt2) radius)
    )
    (box
      (let
        (
          (target (box-nearest-pt pt1 pt2))
          (left   (if (width pt1)  (- (screen-x pt1) (/ (width pt1)  2)) 0))
          (right  (if (width pt1)  (+ (screen-x pt1) (/ (width pt1)  2)) 0))
          (bottom (if (height pt1) (- (screen-y pt1) (/ (height pt1) 2)) 0))
          (top    (if (height pt1) (+ (screen-y pt1) (/ (height pt1) 2)) 0))
          tx
          ty
          hit-vert
          hit-horz
          hit-tl
          hit-tr
          hit-bl
          hit-br
        )
        (if target ;if there was a target, check it, otherwise we are overlapping and just return T
          (progn
            (setf tx (first target))
            (setf ty (second target))
            (setf hit-vert (and (<= tx right) (>= tx left) 
                                (<= ty (+ top radius)) (>= ty (- bottom radius))))
            (setf hit-horz (and (<= tx (+ right radius)) (>= tx (- left radius)) 
                                (<= ty top) (>= ty bottom)))
            (setf hit-tl (<= (vispoint-distance (list left top) (list tx ty)) radius))
            (setf hit-tr (<= (vispoint-distance (list right top) (list tx ty)) radius))
            (setf hit-bl (<= (vispoint-distance (list left bottom) (list tx ty)) radius))
            (setf hit-br (<= (vispoint-distance (list right bottom) (list tx ty)) radius))
            (or hit-vert hit-horz hit-tl hit-tr hit-bl hit-br)
          )
          T
        )
      )
    )
  )
)








;add some slot info to the object print function
(defmethod print-object ((pt vispoint) out)
  (print-unreadable-object (pt out :type t)
    (format out "~s" (list (id pt) (group-ix pt) (group-name pt) (screen-x pt) (screen-y pt)))
  )
)




;visual grouping object
;  contains a collection of vispoint objects
;  and has methods for grouping and group inheritance
;  need: defaults for glom radius, likely 1/2, 1, or 2 degrees of visual angle?
;        above would need screen height, width, dpi, resolution, distance, etc
(defclass visual-groups ()
  (
    (coords         :initarg :coords      :accessor coords           :initform NIL)
    (points                               :accessor points)
    (clusters                             :accessor clusters)
    (id-coords      :initarg :id-coords   :accessor id-coords        :initform NIL)
    (glom-radius    :initarg :glom-radius :accessor glom-radius)
    (glom-type      :initarg :glom-type   :accessor glom-type        :initform 'box)
    (group-count                          :accessor group-count      :initform 0 )
    (group-names                          :accessor group-names      :initform NIL)
    (group-ixs                            :accessor group-ixs        :initform NIL)
    (inherit-thresh                       :accessor inherit-thresh   :initform 1)
    (visicon-fts    :initarg :visicon-fts :accessor visicon-fts      :initform NIL )
  )
)

;when intializing a visual-groups object...
(defmethod initialize-instance :after ((vg visual-groups) &key)
  (if (visicon-fts vg)
      (build-from-visicon vg)
      (build-points vg))
  (if (glom-radius vg)
      (glom-groups vg (glom-radius vg))
  )
)

;build a set of vispoint objects from a set of coordinates
(defmethod build-from-visicon ((vg visual-groups))
  (loop for ft in (visicon-fts vg)
        append (list (make-instance 'vispoint 
                       :id ft
                       :screen-x (chunk-slot-value-fct ft 'screen-x)
                       :screen-y (chunk-slot-value-fct ft 'screen-y)
                       :width    (chunk-slot-value-fct ft 'width)
                       :height   (chunk-slot-value-fct ft 'height)
                       :color    (chunk-slot-value-fct ft 'color)
                     )) into pts
        finally (setf (points vg) pts)
  )
)

;build a set of vispoint objects from a set of coordinates
(defmethod build-points ((vg visual-groups))
  (if (coords vg)
    (loop for c in (coords vg)
        append (list (make-instance 'vispoint :screen-x (first c) :screen-y (second c))) into pts
        finally (setf (points vg) pts)
    )
  )
  (if (id-coords vg)
    (loop for c in (id-coords vg)
        append (list (make-instance 'vispoint :id (first c) :screen-x (second c) :screen-y (third c))) into pts
        finally (setf (points vg) pts)
    )
  )
)

;resets the "checked" slot for the set of points (just good housekeeping)
(defmethod reset-checked ((vg visual-groups))
  (dolist (p (points vg))
    (setf (checked p) NIL)
  )
)

;gets the group for a given point ID
(defmethod get-group ((vg visual-groups) id)
  (let (res)
    (dolist (pt (points vg))
      (if (eq id (id pt)) (setf res (group-name pt)))
    )
    res
  )
)

;helper function for glom-groups
; recursively checks a set of newly incorporated points
; to see if they, in turn, can recruit any additional unchecked points
; 
; (this could be more elegantly written, 
; but the same brute-force work will be done either way)
(defmethod glom-groups-grow ((vg visual-groups) radius subset group-ix)
  (with-slots ((points points)) vg
    (dolist (point subset)
      (if (not (checked point))
        (let (hits)
          (setf (checked point) T)
          (setf (group-ix point) group-ix)
          (dolist (target points)
            (if (not (checked target))
              (if (collide point target radius (glom-type vg))
                (setf hits (append hits (list target)))
              )
            )
          )
          (if hits
            (glom-groups-grow vg radius hits group-ix)
          )
        )
      )
    )
  )
)

;the simple agglomerative grouping method's main call
; 
; Takes: a set of ungrouped vispoints and a grouping radius
; Returns: NIL, but applies group names to those vispoints based on the radius
; 
; This process selects an unexamined point for a new group, 
; "grows" that group according to a simple grouping radius until it can no longer grow,
; then repeats the process for another unchecked point 
; until the set of points is exhausted
; 
(defmethod glom-groups ((vg visual-groups) radius)
  (with-slots ((points points)) vg
    (setf (group-count vg) 0)
    (setf (group-names vg) '())
    (setf (group-ixs vg) '())
    (dolist (point points)
      (if (not (checked point))
        (let (hits)
          (setf (group-ixs vg) (append (group-ixs vg) (list (group-count vg))))
          (setf (checked point) T)
          (setf (group-ix point) (group-count vg))
          (dolist (target points)
            (if (not (checked target))
              (if (collide point target radius (glom-type vg))
                (setf hits (append hits (list target)))
              )
            )
          )
          (if hits
            (glom-groups-grow vg radius hits (group-count vg))
          )
          (incf (group-count vg))
        )
      )
    )
    (reset-checked vg)
  )
)

;output method to write the vispoint data to a data-friendly text file
(defmethod groups-to-file ((vg visual-groups) filename)
  (let ((outstring (concatenate 'string "screen-x	screen-y	width	height	group	radius
" )))
    (dolist (p (points vg))
      (let 
        (
          (group    (write-to-string (group-name p)))
          (screen-x (write-to-string (screen-x p)))
          (screen-y (write-to-string (screen-y p)))
          (width    (write-to-string (width p)))
          (height   (write-to-string (height p)))
          (radius   (write-to-string (glom-radius vg)))
          line
        )
        (setf line (concatenate 'string screen-x "	" screen-y "	" width "	" height "	" group "	" radius "
"))
        (setf outstring (concatenate 'string outstring line))
      )
    )
    ;write data to file
    (with-open-file (strm filename :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format strm outstring)
    )
  )
)

;print the vispoint objects to the screen
(defmethod print-points ((vg visual-groups))
  (dolist (p (points vg)) (print p))
)

;generate a list of n (new-names)
(defun gen-n-syms (n)
  (case vg-naming-type
    (sequential
      (loop for i from 1 to n
        append (list (new-name "group")) into gps
        finally (return gps)
      )
    )
    (generic
      (loop for i from 1 to n
        append (list (gensym)) into gps
        finally (return gps)
      )
    )
  )
)

;assign proper group names to the internally numbered groups
;  if a previous visual group is given, then the (inherit-names) function is called instead
;  otherwise, each computed group is simply assigned a name based on its number
(defmethod label-groups ((vg visual-groups) &optional prev-vg)
  (if (not prev-vg)
    (with-slots ((points points)) vg
      (setf (group-names vg) (gen-n-syms (group-count vg)))
      (dolist (p points)
        (setf (group-name p) (nth (group-ix p) (group-names vg)))
      )
    )
    (inherit-group-labels vg prev-vg)
  )
)


; method for collision across scenes
;; after assigning anonymous group numbers, check each new point against the previous scene
;; keep track of all group collisions, incrementing a count for multiple in the same
;;   - may want a threshold for inheritance 
;;   - (i.e., number needed before being considered a collision worthy of inheritance)
;; new group collides with X old groups:
;;   - 0: assign new group name to the new group
;;   - 1: inherit old group name
;;   - 2+: both get new names
;;     - possible: group with highest collisions inherits, other gets new name
;;     - THIS FEATURE WOULD BE VERY IMPORTANT IN PRODUCING GROUPING ERRORS ON DISPLAYS
;; if multiple new groups collide with the old group...
;; 
;; This inheritance behavior will be important later. 
;; - how do humans perceive merged or split groups?
;; - is there some sort of dominance? more units overlapping a split group wins?
;; - does a larger group win in a "merged" situation?
;; 
;; Will want to change the way merging and splitting works later
(defmethod inherit-group-labels ((vg visual-groups) (prev-vg visual-groups))
  (let
    (
      (newpts (points vg))
      (oldpts (points prev-vg))
      (newgroups (group-ixs vg))
      (oldgroups (group-ixs prev-vg))
      (oldnames (group-names prev-vg))
      newnames
      collisions
    )
    
    ;set up a collisions table (2d array) to make inheritance decisions simpler
    (setf collisions (make-list (length newgroups)))
    (dotimes (i (length collisions))
      (setf (nth i collisions) (make-list (length oldgroups) :initial-element 0))
    )
    
    ;detect point-wise collisions between all new groups and old groups
    (dolist (n newpts)
      (dolist (o oldpts)
        (if (collide n o (glom-radius vg))
          (incf (nth (group-ix o) (nth (group-ix n) collisions)))
        )
      )
    )
    
    ;determine inherited names for each new group...
    (dotimes (n-ix (length collisions))
      (let 
        ;count number of old groups this new group collided with
        (
          (n-hits (apply '+ (mapcar (lambda (x) (if (= 0 x) 0 1)) (nth n-ix collisions))))
          inherited-name
        )
        (setf inherited-name nil)
        ;for each old group...
        (dotimes (o-ix (length (nth n-ix collisions)))
          (if (not inherited-name)
          ;count the number of new groups that collided with this old group
            (let 
              (
                (o-hits (apply '+ (mapcar (lambda (x) (if (= 0 x) 0 1)) (mapcar (lambda (x) (nth o-ix x)) collisions))))
                (val (nth o-ix (nth n-ix collisions)))
              )
              (if (and (= 1 o-hits) (= 1 n-hits) (> val 0)) (setf inherited-name (nth o-ix oldnames)))
            )
          )
        )
        
        ;if found a name to inherit, add it to the list, otherwise generate a new one.
        (if inherited-name 
            (setf newnames (append newnames (list inherited-name)))
            (setf newnames (append newnames (gen-n-syms 1)))
        )
      )
    )
    ;save the new group names in the new scene object
    (setf (group-names vg) newnames)
    ;now that the list of new names has been determined, apply them to the new points
    (dolist (p newpts)
      (setf (group-name p) (nth (group-ix p) newnames)) 
    )
  )
)








;; ACT-R integration

;after loading ACT-R...
;(load (merge-pathnames "../actr7/load-act-r.lisp" *load-truename*))


;storage of scenes
(defparameter vg-scene NIL)
(defparameter vg-prev-scene NIL)

(extend-possible-slots 'group NIL)
(extend-possible-slots 'screen-left NIL)
(extend-possible-slots 'screen-right NIL)
(extend-possible-slots 'screen-top NIL)
(extend-possible-slots 'screen-bottom NIL)
;:around method for ACT-R to insert this grouping algorithm into the proc-display. 
;
; expects the above global parameters to function
(defmethod build-vis-locs-for :around ((self rpm-window) (vis-mod vision-module))
  (extend-possible-slots 'group NIL)
  (extend-possible-slots 'screen-left NIL)
  (extend-possible-slots 'screen-right NIL)
  (extend-possible-slots 'screen-top NIL)
  (extend-possible-slots 'screen-bottom NIL)

  (let 
    ( 
      (feat-lst (flatten (call-next-method)))
    )
    
    ;store the previous scene and parse the current one
    (setf vg-scene (make-instance 'visual-groups :visicon-fts feat-lst :glom-radius vg-glomming-radius :glom-type vg-collision-type))

    ;label the groups (and inherit if possible)
    (label-groups vg-scene vg-prev-scene)

    ;
    (setf vg-prev-scene vg-scene)

    ;retrieve the group name for each point
    (dolist (feat feat-lst)
      (set-chunk-slot-value-fct feat 'group (get-group vg-scene feat))
    )
	
	

	; This is a hack
	(dolist (feat feat-lst)
		(if (equal (chunk-slot-value-fct feat 'kind) 'OVAL) (set-chunk-slot-value-fct feat 'screen-y (+ (chunk-slot-value-fct feat 'screen-y) 1)))
	)
	
	(dolist (feat feat-lst)
		(set-chunk-slot-value-fct feat 'screen-left (* 2 (floor (/ (- (chunk-slot-value-fct feat 'screen-x) (/ (chunk-slot-value-fct feat 'width) 2)) 2))))
	)
	
	(dolist (feat feat-lst)
		(set-chunk-slot-value-fct feat 'screen-right (* 2 (ceiling (/ (+ (chunk-slot-value-fct feat 'screen-x) (/ (chunk-slot-value-fct feat 'width) 2)) 2))))
	)
	
	(dolist (feat feat-lst)
		(set-chunk-slot-value-fct feat 'screen-top (* 2 (floor (/ (- (chunk-slot-value-fct feat 'screen-y) (/ (chunk-slot-value-fct feat 'height) 2)) 2))))
	)
	
	(dolist (feat feat-lst)
		(set-chunk-slot-value-fct feat 'screen-bottom (* 2 (floor (/ (+ (chunk-slot-value-fct feat 'screen-y) (/ (chunk-slot-value-fct feat 'height) 2)) 2))))
	)

	
    feat-lst
  )
)

;override for including groups
(defun print-visicon ()
  "Print the Vision Module's visicon. For debugging."
  (awhen (get-module :vision)  ;; Test that there is a vision module
         (update-new it)
         (check-finsts it) 
         (command-output "ID                   Group     Loc          Wd / Ht      Boundries (l,r,t,b)   Att  Kind           Color             Value          Left         ")
         (command-output "-------------------  --------  -----------  -----------  --------------------  ---  -------------  ----------------  -------------  -------------")
         
         (mapcar (lambda (x) (print-icon-feature x it)) (visicon-chunks it t))
         nil))

;override for including groups
(defun print-icon-feature (chunk vis-mod)
  (let* ((*print-pretty* nil)
         (coord-slots (vis-loc-slots vis-mod))
         (x-slot (first coord-slots))
         (y-slot (second coord-slots)))
    (command-output "~19A  ~8A  (~4A ~4A)  (~4A ~4A)  (~4A ~4A ~4A ~4A)  ~3A  ~13A  ~16A  ~32A"
                    (chunk-visual-feature-name chunk)
                    (chunk-slot-value-fct chunk 'group)
                    (chunk-slot-value-fct chunk x-slot) 
                    (chunk-slot-value-fct chunk y-slot) 
                    (chunk-slot-value-fct chunk 'width)
                    (chunk-slot-value-fct chunk 'height)
					(chunk-slot-value-fct chunk 'screen-left)
					(chunk-slot-value-fct chunk 'screen-right)
					(chunk-slot-value-fct chunk 'screen-top)
					(chunk-slot-value-fct chunk 'screen-bottom)
                    (feat-attended chunk (get-module :vision))
                    (chunk-slot-value-fct chunk 'kind) 
                    (chunk-slot-value-fct chunk 'color)
                    (if (null (chunk-real-visual-value chunk))
                        (chunk-slot-value-fct chunk 'value) 
                      (chunk-real-visual-value chunk))
					)))



;message on-load
(format t 
"Visual grouping system loaded. 
Visicon entries now have an automatically assigned 'group' slot.
Set the 'vg-glomming-radius' parameter to a new value (in pixels) to adjust the grouping method." )



; ----------------------------------------------------------------------
; End file: visual-grouping.lisp
; ----------------------------------------------------------------------