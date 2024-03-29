;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2005 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : env-device.lisp
;;; Version     : 3.3
;;; 
;;; Description : No system dependent code.
;;;             : This file contains the code that handles passing
;;;             : the virtual windows out through Tk - the visible
;;;             : virtuals, and the AGI support.
;;; Bugs        : [ ] What should happen if a button goes away between when the
;;;             :     model initiates an action and finishes it?
;;; 
;;; Todo        :   
;;; ----- History -----
;;;
;;; 10/01/2002  Dan
;;;             : Added this header. 
;;; 10/01/2002  Dan
;;;             : Updated version to 1.1 and fixed the packaging
;;;             : for building a standalone in ACL.
;;; 12/04/2002  Dan
;;;             : Added the definition for *use-environment-window*
;;;             : even though it's still not used because it
;;;             : does get set by the environment and generates a
;;;             : warning.
;;; 01/06/2003  Dan
;;;             : Updated it so that the colors were sent out
;;;             : properly for all supported colors and so that
;;;             : text colors worked.
;;; 4/22/2004   Dan [1.5]
;;;             : Added the license info.
;;; -----------------------------------------------------------------------
;;; 2005.04.13  Dan [2.0]
;;;             : * Moved over to ACT-R 6.
;;; 2005.06.07 Dan
;;;             : * Replaced a call to device-interface with 
;;;             :   current-device-interface.
;;; 2005.06.28 Dan
;;;             : * Removed a pm-proc-display call that was still lingering
;;;             :   in the device-move-cursor-to method.
;;; 2007.07.13 Dan
;;;             : * Added the sending of a button's color to the env.
;;; 2010.05.21 Dan
;;;             : * Changed the name of the dialog items to add a vv on the
;;;             :   to avoid interning a name which could shift the names
;;;             :   for things like visual chunks (text items in particular).
;;;             :   Doesn't affect performance, just keeps traces consistent
;;;             :   when shifting between visible and virtual.
;;; -------------------------------------------------------------------------
;;; 2011.05.20 Dan [3.0]
;;;             : * Start of a complete overhaul to eliminate most of the 
;;;             :   global variable usage and better encapsulate things so
;;;             :   that multiple model support can be added.
;;; 2011.11.21 Dan
;;;             : * Using model-generated-action instead of *actr-enabled-p*
;;;             :   to detect when the button needs to be visibly "clicked".
;;; 2012.06.26 Dan
;;;             : * Adding a clearattention flag that gets sent and save the
;;;             :   window used in the visual-fixation-marker slot so that if
;;;             :   the window changes it can clear the old marker before
;;;             :   displaying the new one.
;;; 2012.06.27 Dan [3.1]
;;;             : * Start the process of supporting multiple windows on the
;;;             :   environment side.
;;;             :   - Give the window a unique id using new-symbol instead of
;;;             :     new-name.
;;;             :   - Use the symbol-name of the ids instead of the symbol since
;;;             :     if the window persists across a reset those symbols will
;;;             :     get uninterned which means they won't match what was
;;;             :     originally sent.
;;;             :   - Create the items in the context of the model in which the
;;;             :     window was created so that the names are guaranteed to be
;;;             :     unique without having to use new-symbol.
;;; 2012.07.12 Dan
;;;             : * Added a custom vw-output method for visible-virtual windows
;;;             :   because in a multiple model situation the output should
;;;             :   be based on the owning model's settings instead of whichever
;;;             :   model (if any) happens to be current.
;;; 2012.08.10 Dan
;;;             : * Support more than one fixation ring per window since 
;;;             :   multiple models may be sharing a device by using the
;;;             :   model name in a tag.
;;; 2012.08.31 Dan
;;;             : * Added env-window-click so that I can catch all the user
;;;             :   clicks on the environment side window and call the
;;;             :   rpm-window-click-event-handler appropriately.
;;; 2012.09.21 Dan
;;;             : * Grabbing the environment lock before sending the updates
;;;             :   with send-env-window-update.
;;;             : * Bad idea - can deadlock things...
;;; 2013.01.10 Dan
;;;             : * Env. sends keys over as strings now and uses a function
;;;             :   named convert-env-key-name-to-char to convert that to a
;;;             :   character instead of throwing a warning on the read of an
;;;             :   invalid character.  If it's an invalid character it still
;;;             :   prints a warning, but now that should be more informative.
;;;             : * Instead of redefining visible-virtuals-available? here the
;;;             :   function in the virtual's uwi file now calls check-with-environment-for-visible-virtuals
;;;             :   which is defined here instead.
;;; 2013.07.18 Dan
;;;             : * Fixed a potential bug in vv-click-event-handler because
;;;             :   the button could be removed between the model's click 
;;;             :   initiation and execution in which case it can't send
;;;             :   the update to the environment.  Probably shouldn't eval
;;;             :   the action either, but I'm not fixing that now.
;;; 2014.08.29 Dan
;;;             : * Changed vv-click-event-handler so that the action can be
;;;             :   either a function or a symbol naming a function.
;;; 2015.04.29 Dan
;;;             : * Changed convert-env-key-name-to-char so that it can handle
;;;             :   things like space and return.
;;; 2015.05.26 Dan
;;;             : * Added a font-size parameter to make-static-text-for-rpm-window.
;;;             :   Should be a point size for the font, defaults to 12 and the
;;;             :   height and width are based on the ratios for the previous
;;;             :   defaults which were 10 high and 7 wide for 12 point.  Then
;;;             :   pass that over to the environment to use when drawing it
;;;             :   in add-visual-items-to-rpm-window.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;; 2016.04.01 Dan [3.2]
;;;             : * Treat human clicks in the visible virtuals the same as
;;;             :   model clicks and let the virtual window deal with the
;;;             :   processing.  Avoids a lot of crazyness on the Env. side
;;;             :   and is consistent with respect to the click event handler.
;;;             :   This also coincides to a shift on the Env. side of not
;;;             :   using real buttons in the visible virtual window so that
;;;             :   cursor and visual attention always show over items.  
;;;             : * That means that all clicks are handled by the subviews and
;;;             :   button "clicks" all need to be sent over.
;;; 2016.04.04 Dan [3.3]
;;;             : * Actually use the show-focus value to determine a color if
;;;             :   it's a valid symbol or a string.
;;; 2016.06.08 Dan
;;;             : * Change add-visual-items-to-rpm-window method so that it
;;;             :   only sends a message when it recognizes the item type
;;;             :   instead of sending a null message when it's a new type
;;;             :   of item to allow for easier customizing/updating.
;;;             : * Add the methods to handle modifying the items.  For now
;;;             :   it takes the simple approach of calling the virtual item
;;;             :   code to make the changes and then removing and redrawing
;;;             :   the item in the Env. window.
;;; 2016.08.09 Dan
;;;             : * Changed the modify methods to just specify allow-other-keys
;;;             :   since they don't directly use any of them.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "UNI-FILES" "ACT-R-support:uni-files")

;;; virtual windows for the environment that go out through
;;; Tcl/Tk to display i.e. a visible virtual
;;;
;;; Inherits much of the functionality from the rpm-virtual-window
;;; but needs to send the commands to Tcl/Tk as well

(defun check-with-environment-for-visible-virtuals () 
  "Return whether or not the visible-virtuals are available"
  (and (environment-control-connections *environment-control*)
       (environment-control-use-env-windows *environment-control*)))


(defvar *vv-table* (make-hash-table :test 'equal))

;;; First define all the stuff necessary for it and its possible 
;;; 'subviews' as it relates to the device interface

(defclass visible-virtual-window (rpm-virtual-window)
  ((vv-model :accessor vv-model :initform (current-model))))

(defmethod initialize-instance :after ((win visible-virtual-window) &key)
  (let ((name (string-downcase (symbol-name (new-symbol-fct "VVW"))))) ;; need a unique name and is should be downcase so 
                                                                       ;; it can be used directly on the tcl side
    (setf (id win) name)
    
    ;; store the window in the lookup table
    (setf (gethash name *vv-table*) win)
  
    (send-env-window-update (list 'open name (window-title win) (x-pos win)
                                  (y-pos win) (width win) (height win)))))

(defclass env-text-vdi (static-text-vdi)
  ()
  (:default-initargs
      :id (symbol-name (new-name-fct "VVTEXT"))))

(defclass env-button-vdi (button-vdi)
  ()
  (:default-initargs
      :id (symbol-name (new-name-fct "VVBUTTON"))))

(defclass env-line-vdi (v-liner)
  ()
  (:default-initargs
      :id (symbol-name (new-name-fct "VVLINE"))))


(defmethod vv-click-event-handler ((btn env-button-vdi) where)
  (declare (ignore where))
  
  ;; Always send the click notice over
  (when (view-container btn)
    (send-env-window-update (list 'click (id (view-container btn)) (id btn))))
  
  ;; let the button-vdi method do the real work ...
  (call-next-method))


(defmethod device-move-cursor-to ((vw visible-virtual-window) (loc vector))
  (setf (cursor-pos vw) loc)
  (when (with-cursor-p (current-device-interface))
    (proc-display))
  (send-env-window-update (list 'cursor (id vw) (px loc) (py loc))))

(defmethod device-update-attended-loc ((wind visible-virtual-window) xyloc)
  
  (when (and (visual-fixation-marker) (not (eq wind (visual-fixation-marker))))
    (send-env-window-update (list 'clearattention (id wind) (current-model))))
  
  (setf (visual-fixation-marker) wind)
  
  (if xyloc
      (let ((color (show-focus-p (current-device-interface))))
        (cond ((eq color t)
               (setf color (color-symbol->env-color 'red)))
              ((symbolp color)
               (let ((new-color (color-symbol->env-color color)))
                 (if (and (string-equal new-color "black") (neq color 'black))
                     (setf color (color-symbol->env-color 'red))
                   (setf color new-color))))
              (t ))  ;; if it's a string assume it's good
        (send-env-window-update (list 'attention (id wind) (px xyloc) (py xyloc) (current-model) color)))
    
    (send-env-window-update (list 'clearattention (id wind) (current-model)))))


;;; Redefine the vw-output command since in a multiple model situation
;;; this could be called in other than the owning model's context (particularly
;;; if a person performs the action, but also when there is more than one 
;;; model using the window as its device), but always use the owner's 
;;; settings to determine if and where to output things.


(defmethod vw-output ((vw visible-virtual-window) (base string) &rest args)
  ;; DAN
  ;; seems like this should be in the trace
  ;;(format (outstrm vw)
  
  (multiple-value-bind (mp model) (exp-window-owner vw)
    (if (and mp model)
        (with-meta-process-eval mp
          (with-model-eval model
            (when (car (no-output (sgp :vwt)))
              (model-output "~&~%<< Window ~S ~? >>~%" (window-title vw) base args))))
      (print-warning "No owning model available for window output: << Window ~S ~? >>" (window-title vw) base args))))


;;; Then define the UWI functions - which get called by the exp interface

(defmethod close-rpm-window ((win visible-virtual-window))
  (setf (window-open? win) nil)
  (remhash (id win) *vv-table*)
  (send-env-window-update (list 'close (id win))))


(defmethod select-rpm-window ((win visible-virtual-window))
  (send-env-window-update (list 'select (id win))))

(defmethod add-visual-items-to-rpm-window ((win visible-virtual-window) &rest items)
  (dolist (item items)
    (add-subviews win item)
    (case (type-of item)
      (env-button-vdi
       (send-env-window-update 
        (list 'button (id win) (id item) (x-pos item) (y-pos item) 
              (width item) (height item) (dialog-item-text item) (color-symbol->env-color (color item)))))
      (env-text-vdi
       (send-env-window-update 
        (list 'text (id win) (id item) (x-pos item) (y-pos item) 
              (color-symbol->env-color (color item))  (dialog-item-text item) (round (text-height item) 10/12))))
      (env-line-vdi
       (send-env-window-update 
        (list 'line (id win) (id item) (x-pos item) (y-pos item) 
              (color-symbol->env-color (color item)) (width item) (height item)))))))



(defun color-symbol->env-color (color)
  (cond ((equal color 'red) "red")
        ((equal color 'blue) "blue")
        ((equal color 'green) "green")
        ((equal color 'black) "black")
        ((equal color 'white) "white")
        ((equal color 'pink)  "pink")
        ((equal color 'yellow) "yellow")
        ((equal color 'dark-green) "green4")
        ((equal color 'light-blue) "cyan")
        ((equal color 'purple) "purple")
        ((equal color 'brown) "brown")
        ((equal color 'light-gray) "gray90")
        ((equal color 'gray) "gray")
        ((equal color 'dark-gray) "gray40")
        (t "black")))



(defmethod remove-visual-items-from-rpm-window ((win visible-virtual-window) 
                                                &rest items)
  (dolist (item items)
    (remove-subviews win item)
    (send-env-window-update (list 'remove (id win) (id item)))))



(defmethod remove-all-items-from-rpm-window ((win visible-virtual-window))
  (apply #'remove-subviews win (subviews win))
  (send-env-window-update (list 'clear (id win))))


(defmethod make-button-for-rpm-window ((win visible-virtual-window) 
                                       &key (x 0) (y 0) (text "Ok")  
                                       (action nil) (height 18) (width 60) (color 'gray))
  (with-model-eval (vv-model win)
    (make-instance 'env-button-vdi
      :x-pos x 
      :y-pos y
      :dialog-item-text text
      :action action
      :height height
      :width width
      :color color)))

(defmethod modify-button-for-rpm-window ((button-item env-button-vdi) &key &allow-other-keys)
  "Modify a button item for the rpm window"
  (call-next-method)
  (awhen (view-container button-item) ;; it's on the screen
         ;; simple approach for the environment -- remove the old one and draw a new one
         ;; do that directly instead of calling remove/add for simplicity
         (send-env-window-update (list 'remove (id it) (id button-item)))
         (send-env-window-update 
          (list 'button (id it) (id button-item) (x-pos button-item) (y-pos button-item) 
                (width button-item) (height button-item) (dialog-item-text button-item) (color-symbol->env-color (color button-item)))))
  button-item)


  
(defmethod make-static-text-for-rpm-window ((win visible-virtual-window) 
                                            &key (x 0) (y 0) (text "") 
                                            (height 20) (width 80) (color 'black)
                                            font-size)
  (unless (numberp font-size)
    (setf font-size 12))
  (with-model-eval (vv-model win)
    (make-instance 'env-text-vdi
      :x-pos x 
      :y-pos y
      :dialog-item-text text
      :height height
      :width width
      :color color
      :text-height (round font-size 12/10)
      :str-width-fct (let ((w (round font-size 12/7))) (lambda (str) (* (length str) w))))))

(defmethod modify-text-for-rpm-window ((text-item env-text-vdi) &key &allow-other-keys)
  "Modify a text item for the rpm window"
  (call-next-method)
  (awhen (view-container text-item) ;; it's on the screen
         ;; simple approach for the environment -- remove the old one and draw a new one
         ;; do that directly instead of calling remove/add for simplicity
         (send-env-window-update (list 'remove (id it) (id text-item)))
         (send-env-window-update 
          (list 'text (id it) (id text-item) (x-pos text-item) (y-pos text-item) 
              (color-symbol->env-color (color text-item))  (dialog-item-text text-item) (round (text-height text-item) 10/12))))
  text-item)


(defmethod rpm-window-visible-status ((win visible-virtual-window))
  t)

(defmethod make-line-for-rpm-window ((wind visible-virtual-window) 
                                     start-pt end-pt &optional (color 'black))
  (with-model-eval (vv-model wind)
    (make-instance 'env-line-vdi
      :color color
      :x-pos (first start-pt)
      :y-pos (second start-pt)
      :width (first end-pt)
      :height (second end-pt))))

(defmethod modify-line-for-rpm-window ((line env-line-vdi) start-pt end-pt &key &allow-other-keys)
  (declare (ignorable start-pt end-pt))
  (call-next-method)
  (awhen (view-container line) ;; it's on the screen
         ;; simple approach for the environment -- remove the old one and draw a new one
         ;; do that directly instead of calling remove/add for simplicity
         (send-env-window-update (list 'remove (id it) (id line)))
         (send-env-window-update 
          (list 'line (id it) (id line) (x-pos line) (y-pos line) 
              (color-symbol->env-color (color line)) (width line) (height line))))
  line)


;;; This is a little tricky, because if it's a real window in an IDE'ed
;;; Lisp, it's still got to do the right thing to prevent hanging, 
;;; which is lisp specific...

(defmethod allow-event-manager ((win visible-virtual-window))
  (uni-process-system-events))


;;; This function relies on the handlers from the environment existing...

(defun send-env-window-update (cmd)
  (dolist (x (environment-control-windows *environment-control*))
    (setf (update-form x) #'(lambda (x) (declare (ignore x)) cmd))
    (update-handler x nil)))


;;; Functions that get called because of actions on the Tcl/Tk side
;;; and need to map a Tcl/Tk window name (has a . on the front) to
;;; the actual object in the table.

(defun map-env-window-name-to-window (name)
  (gethash (subseq name 1) *vv-table*))

(defun env-window-key-pressed (win-name key)
  (let ((win (map-env-window-name-to-window win-name))
        (char (convert-env-key-name-to-char key)))
    (cond ((and win char)
           (device-handle-keypress win char))
          (win ;; key is invalid
           (print-warning "Window titled ~s received invalid Lisp character ~s as a key press." (window-title win) key))
          (t ;; somehow the window is invalid
           (print-warning "Keypress received from unknown environment window ~s." win-name)))))

(defun convert-env-key-name-to-char (key)
  (or (ignore-errors (coerce key 'character)) 
      (ignore-errors (let ((c (read-from-string (concatenate 'string "#\\" key))))
                       (when (characterp c) c)))))

(defun env-window-click (win-name x y)
  (let ((win (map-env-window-name-to-window win-name)))
    (when win
      (let ((current (cursor-pos win)))
        (unwind-protect 
            ;; move the cursor to where the person clicked
            ;; without actually updating the cursor for the
            ;; model for now
            ;; Maybe, it should just call device-move-cursor-to
            ;; so the person is moving the same virtual cursor
            ;; that the model is, but for now consider them
            ;; seprate.
            (progn
              (setf (cursor-pos win) (vector x y))
              (device-handle-click win))
          (setf (cursor-pos win) current))))))

#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
