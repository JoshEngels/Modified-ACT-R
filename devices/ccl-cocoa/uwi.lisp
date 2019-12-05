; ----------------------------------------------------------------------
; Begin file: actr6/devices/ccl/uwi.lisp
; ----------------------------------------------------------------------


;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2004 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : uwi.lisp
;;; Version     : 1.0a1
;;; 
;;; Description : MCL-specific functions to implement the UWI.
;;;             : NOTE: The UWI is only still around to support the 
;;;             :       ACT-R GUI interface. I don't advocate using it directly.      
;;; 
;;; Bugs        : 
;;; --- History ---
;;; 2002.06.30 Dan
;;;             : Added this header.
;;;             : Moved all of the UWI code from mcl-interface
;;;               to this file where it belongs.
;;;               Actually documented the code!
;;; 2002.12.19 Dan
;;;             : Modified the window class and make-static-text-...
;;;               so that it can handle the color attribute.
;;; 04.04.13   Dan [2.2] (previous change is "new" as of 2.2 as well)
;;;             : Changed the copyright notice and added the LGPL stuff.
;;;
;;; 04.10.19   Dan [Moved into ACT-R 6]
;;;             : Reset the version to 1.0a1
;;;             : added the packaging switches
;;;             : changed the name to uwi to be placed in a folder called mcl
;;; 2007.07.13 Dan
;;;             : Added the color keyword to make-button-for-rpm-window
;;;               though it's not actually used at this point.
;;; 2012.08.07 cts
;;;             : Tweaked original MCL uwi.lisp code, and used it to build a
;;;               uwi.lisp for CCL that leverages ccl-simple-view.lisp.
;;; 2012.08.27 Dan
;;;             : In the view-key-event-handler, when it is a model generated
;;;               keypress, signal the *keypress-wait* semaphore so that the
;;;               device-handle-keypress method for the device can return.
;;; 2012.08.30 cts
;;;             : Created a post-view-(key|click)-event handler method that
;;;               gets called after all view-(key|click)-event handler methods
;;;               are called. 'Even the around methods on the most specific class'.
;;;               This post method calls the rpm handler methods. This guarantees
;;;               that all view-(key|click)-event handler methods and rpm
;;;               handler methods are called before the semaphore is triggered.
;;;               Using this technique for both keypress and mouse clicks
;;; 2012.09.04 Dan
;;;             : Changed it so the rpm-window-click-event-handler is called
;;;               with a vector of the mouse position.
;;; 2013.04.20 cts
;;;             : Now spell checking comments and strings in the code.
;;; 2013.04.27 cts
;;;             : Added make-liner-from-points utility function that can create a 
;;;               liner object or subclassed liner object.
;;;               Refactored make-line-for-rpm-window to call the utility function
;;; 2013.05.13 cts
;;;             : Suppressed compiler warning in make-button-for-rpm-window when not
;;;               in development mode
;;; 2014.02.10 Dan
;;;             : Save the color of the button in the background slot.  It 
;;;               doesn't actually change the button's color, but the model 
;;;               can read it from there for consistency with other devices.
;;; 2014.04.15 cts
;;;             : Ensure that print output within the rpm-window-(view|click)-event-handler
;;;               methods goes to the same stream that the ACT-R model is running on.
;;; 2015.05.26 Dan
;;;             : Added a font-size keyword parameter to make-static-text-for-exp-window.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;; 2016.06.08 Dan
;;;             : * Allow the class in make-rpm-window to override visible-
;;;             :   virtual-windows if it's a subtype of that class.
;;; 2016.06.15 Dan
;;;             : * Added the modify actions to support the new AGI functionality
;;;             :   and actually modify things -- not just replace.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "CCL-SIMPLE-VIEW" "ACT-R-support:ccl-simple-view")

;;; RPM-REAL-WINDOW  [Class]
;;; Description : This is the UWI's window class to produce an MCL.
;;;             : It inherits from the MCL dialog class (a real window) and
;;;             : the rpm-window class which is an abstract class used by the
;;;             : ACT-R GUI interface.

(defclass rpm-real-window (rpm-window color-dialog)
  ())

;;; VIEW-KEY-EVENT-HANDLER  [Method]
;;; Description : The method called when a key is pressed.  It
;;;             : calls the rpm-window-key-event-handler which is
;;;             : to be defined by the modeler and after that
;;;             : if it was a model which generated the action
;;;             : set the semaphore so that the device knows it
;;;             : has been dealt with.

(defun get-listener-output-stream ()
  (two-way-stream-output-stream *terminal-io*))

(defparameter *listener-output* (get-listener-output-stream))

;;; INITIALIZE-INSTANCE :after [Method]
;;; Description : Update *listener-output* when an 'rpm-real-window instance is created
;;;             : This update handles the case that a user has launched a new
;;;             : lisp listener before running/rerunning a model

(defmethod initialize-instance :after ((win rpm-real-window) &key)
  (setf *listener-output* (get-listener-output-stream)))

(defmethod post-view-key-event-handler ((device rpm-real-window) key)
  (sv-log-n 1 "Finished calling all view-key-event-handlers for ~a" device)
  (let ((*standard-output* *listener-output*))
    (rpm-window-key-event-handler device key))
  (when (model-generated-action)
    (signal-semaphore *keypress-wait*)))

;;; RPM-WINDOW-KEY-EVENT-HANDLER  [Method]
;;; Description : The UWI method called when a key is pressed.  
;;;             : This is just a default that does nothing because the
;;;             : modeler is supposed to define this.

(defmethod rpm-window-key-event-handler ((device rpm-real-window) key)
  (declare (ignore device key))
  (call-next-method))

;;; VIEW-CLICK-EVENT-HANDLER  [Method]
;;; Description : The method called when a mouse click occurs.  It
;;;             : just calls the rpm-window-click-event-handler with the
;;;             : mouse position converted to a list. 
;;;             : The rpm-window-click-event-handler is supposed 
;;;             : to be defined by the modeler.

(defmethod post-view-click-event-handler ((device rpm-real-window) position)
  (sv-log-n 1 "Finished calling all view-click-event-handlers for ~a" device)
  (let ((*standard-output* *listener-output*))
    (rpm-window-click-event-handler device (vector (point-h position) (point-v position))))
  (when (model-generated-action)
    (signal-semaphore *mouseclick-wait*)))

;;; RPM-WINDOW-CLICK-EVENT-HANDLER  [Method]
;;; Description : The UWI method called when the mouse is clicked.  
;;;             : This is just a default that does nothing because the
;;;             : modeler is supposed to define this.

(defmethod rpm-window-click-event-handler ((device rpm-real-window) position)
  (declare (ignore device position))
  (call-next-method))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; These are the UWI Methods.
;;;; ---------------------------------------------------------------------- ;;;;

;;; OPEN-RPM-WINDOW?  [Method]
;;; Description : Returns t if the window is open and nil if not.

(defmethod open-rpm-window? ((win rpm-real-window))
  (wptr win))

;;; CLOSE-RPM-WINDOW  [Method]
;;; Description : Closes the window.

(defmethod close-rpm-window  ((win rpm-real-window))
  (window-close win))

;;; SELECT-RPM-WINDOW  [Method]
;;; Description : Brings the specified window to the foreground.

(defmethod select-rpm-window ((win rpm-real-window))
  (window-select win))

;;; ADD-VISUAL-ITEMS-TO-RPM-WINDOW  [Method]
;;; Description : Makes the specified items subviews of the window and
;;;             : calls view-draw-contents and event-dispatch to make sure
;;;             : that they show up.

(defmethod add-visual-items-to-rpm-window ((win rpm-real-window) &rest items )
  (dolist (item items)
    (add-subviews win item))
  (view-draw-contents win)
  (event-dispatch))

;;; REMOVE-VISUAL-ITEMS-FROM-RPM-WINDOW  [Method]
;;; Description : Take the specified items out of the subviews of the
;;;             : window and make it redraw.

(defmethod remove-visual-items-from-rpm-window ((win rpm-real-window) 
                                                &rest items)
  (dolist (item items)
    (remove-subviews win item))
  (view-draw-contents win)
  (event-dispatch))

;;; REMOVE-ALL-ITEMS-FROM-RPM-WINDOW  [Method]
;;; Description : Remove all the subviews of the window and redisplay it.

(defmethod remove-all-items-from-rpm-window ((win rpm-real-window))
  (awhen (remove-if #'rpm-overlay-p (subviews win))
    (apply #'remove-subviews win it)
    (view-draw-contents win)
    (event-dispatch)))

;;; RPM-WINDOW-TITLE  [Method]
;;; Description : Return the title of the window.

(defmethod rpm-window-title ((win rpm-real-window))
  (window-title win))

;;; RPM-WINDOW-VISIBLE-STATUS  [Method]
;;; Description : Return t to indicate that this is a visible window.

(defmethod rpm-window-visible-status ((win rpm-real-window))
  t)

;;; MAKE-RPM-WINDOW  [Function]
;;; Description : Make and return a window based on the parameters supplied.
;;;             : Visible determines whether or not it should be a real or
;;;             : virtual and if the environment is connected it will use a 
;;;             : visible-virtual for the real window unless the user explicitly
;;;             : specifies the class to use.

(defun make-rpm-window (&key (visible nil) (class nil) (title "RPM Window") 
                             (width 100) (height 100) (x 0 ) (y 0))
  "Make and return a window for use with the UWI"
  (if visible
      (if (and (visible-virtuals-available?) (or (null class) (subtypep class 'visible-virtual-window)))
          (make-instance (if class class 'visible-virtual-window) :window-title title :width width :height height :x-pos x :y-pos y)
        (make-instance (if class class 'rpm-real-window) 
          :window-title title :view-size (make-point width height) 
          :view-position (make-point x y)))
    (make-instance (if class class 'rpm-virtual-window) :window-title title 
      :width width :height height :x-pos x :y-pos y)))


;;; MAKE-BUTTON-FOR-RPM-WINDOW  [Method]
;;; Description : Build and return a button-dialog-item based on the
;;;             : parameters supplied.

(defmethod make-button-for-rpm-window ((win rpm-real-window) &key (x 0) (y 0) 
                                                             (text "Ok") (action nil) (height 25)
                                                             (width 60) (color 'gray))
  ;#-:sv-dev (declare (ignore color))
  (let ((item (make-dialog-item 'button-dialog-item
                                (make-point x y)
                                (make-point width height)
                                text
                                action
                                :default-button nil)))
    (set-back-color item (color-symbol->system-color color))
    item))

(defmethod modify-button-for-rpm-window ((button button-dialog-item) &key (x nil xp) (y nil yp) (text nil textp) (height nil heightp) (width nil widthp) (color nil colorp) (action nil actionp))
  "Modify (replace) a text item for the rpm window"
  (unless colorp 
    (setf color  (system-color->symbol (get-back-color button))))
 
  (unless textp
    (setf text (dialog-item-text button)))
  (unless heightp
    (setf height (point-v (view-size button))))
  (unless widthp
    (setf width  (point-h (view-size button))))
  (unless xp
    (setf x (point-h (view-position button))))
  (unless yp
    (setf y (point-v (view-position button))))
  (unless actionp
    (setf action (dialog-item-action button)))

  (let ((visible (view-window button)))

    (when visible 
      (remove-visual-items-from-rpm-window visible button))

    (set-back-color button (color-symbol->system-color color))
    
    (set-dialog-item-text button text)
    (set-view-position button x y)
    (set-view-size button width height)
 

    ;; neither of these slots seems to really matter
    ;; only the action function call, but setting 
    ;; everything to be safe

    (when (null action)
      (setf action (lambda (x) (declare (ignore x)))))


    (setf (slot-value button 'dialog-item-action) action)

    ;; the real action gets called with no args and needs to call our action
    (let ((a2 (lambda () (funcall action button))))

      (setf (slot-value button 'easygui::action) a2)
    
      (setf (easygui::action button) a2))  


    (when visible 
      (add-visual-items-to-rpm-window visible button)))
  button)


;;; MAKE-STATIC-TEXT-FOR-RPM-WINDOW  [Method]
;;; Description : Build and return a static-text-dialog-item based on the
;;;             : parameters supplied.

(defmethod make-static-text-for-rpm-window ((win rpm-real-window) 
                                            &key (x 0) (y 0) (text "") 
                                            (height 20) (width 80) (color 'black)
                                            (font-size 12))
  (let ((item (make-dialog-item 'static-text-dialog-item
                                (make-point x y)
                                (make-point width height)
                                text
                                nil
                                :view-font (list "Lucida Grande" font-size :SRCCOPY :PLAIN '(:COLOR-INDEX 0)))))
    (set-part-color item :text (color-symbol->system-color color))
    item))


(defmethod modify-text-for-rpm-window ((text-item static-text-dialog-item) &key (x nil xp) (y nil yp) (text nil textp) (height nil heightp) (width nil widthp) (color nil colorp) (font-size nil font-sizep))
  "Modify (replace) a text item for the rpm window"
  (unless colorp 
    (setf color  (system-color->symbol (part-color text-item :text))))
  (unless font-sizep
    (setf font-size (slot-value (view-font text-item) 'ns:_size)))
  (unless textp
    (setf text (dialog-item-text text-item)))
  (unless heightp
    (setf height (point-v (view-size text-item))))
  (unless widthp
    (setf width  (point-h (view-size text-item))))
  (unless xp
    (setf x (point-h (view-position text-item))))
  (unless yp
    (setf y (point-v (view-position text-item))))
  
  (let ((visible (view-window text-item)))

    (when visible 
      (remove-visual-items-from-rpm-window visible text-item))

    (set-part-color text-item :text (color-symbol->system-color color))
    (setf (slot-value (view-font text-item) 'ns:_size) (coerce font-size 'double-float))
    (set-dialog-item-text text-item text)
    (set-view-position text-item x y)
    (set-view-size text-item width height)

    (when visible 
      (add-visual-items-to-rpm-window visible text-item)))
  text-item)


;;; MAKE-LINE-FOR-RPM-WINDOW  [Method]
;;; Description : Build and return the appropriate liner object for the
;;;             : window based on the parameters supplied.

(defmethod make-line-for-rpm-window ((wind rpm-real-window) start-pt end-pt &optional (color 'black))
  (make-liner-from-points start-pt end-pt color))

; make-liner-from-points is a utility function that can create a liner object or subclassed liner object.
; A separate function is used rather than extending make-line-for-rpm-window so that the interface to 
; make-line-for-rpm-window remains the same, since that interface is public, and that interface does 
; not allow the caller to provide a subclassed liner class to initialize

(defun make-liner-from-points (start-pt end-pt color &key (liner-class 'liner))
  (destructuring-bind (startx starty) start-pt
    (destructuring-bind (endx endy) end-pt
      (unless (> endx startx)
        (rotatef startx endx)
        (rotatef starty endy))
      (let ((vs (make-point (+ 1 (abs (- endx startx)))
                            (+ 1 (abs (- endy starty)))))
            (vp (make-point startx (min starty endy))))
        (make-instance liner-class
                       :liner-type (if (> endy starty) 'td 'bu)
                       :position vp
                       :size vs
                       :color (color-symbol->system-color color))))))



(defmethod modify-line-for-rpm-window ((line liner) start-pt end-pt &key (color nil colorp))
  "Modify a line item for the rpm window"
  (unless colorp 
    (setf color (system-color->symbol (color line))))
  (let* ((p1 (view-position line))
         (x1 (point-h p1))
         (y1 (point-v p1))
         (p2 (view-size line))
         (x2 (point-h p2))
         (y2 (point-v p2))
         (sx (if start-pt (first start-pt) x1))
         (ex (if end-pt (first end-pt) (1- (+ x1 x2))))
         (sy (if start-pt 
                 (second start-pt)
                 (if (eq (liner-type line) 'td) 
                     y1
                     (1- (+ y1 y2)))))
         (ey (if end-pt
                 (second end-pt)
                 (if (eq (liner-type line) 'td)
                     (1- (+ y1 y2))
                     y1))))
    
    (unless (> ex sx)
      (rotatef sx ex)
      (rotatef sy ey))
    (let ((visible (view-window line)))

      (when visible 
        (remove-visual-items-from-rpm-window visible line))

      (set-view-position line sx (min sy ey))
      (set-view-size line (+ 1 (abs (- ex sx))) (+ 1 (abs (- ey sy))))
      (setf (slot-value line 'liner-type) (if (> ey sy) 'td 'bu))
      (set-fore-color line (color-symbol->system-color color))
      
      (when visible 
        (add-visual-items-to-rpm-window visible line)))
    line))


;;; ALLOW-EVENT-MANAGER  [Method]
;;; Description : Call event-dispatch.  This is used while waiting for
;;;             : a person to respond.

(defmethod allow-event-manager ((win rpm-real-window))
  (sv-log-n 1 "starting allow-event-manager")
  (event-dispatch)
  (sv-log-n 1 "ending-allow-event-manager"))

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



; ----------------------------------------------------------------------
; End file: actr6/devices/ccl/uwi.lisp
; ----------------------------------------------------------------------
