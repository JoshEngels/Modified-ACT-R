;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2010 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : imaginal-compilation.lisp
;;; Version     : 3.0
;;; 
;;; Description : Production compilation IMAGINAL style definition.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;;
;;; ----- History -----
;;;
;;; 2010.12.10 Dan
;;;             : * Created automatically by build-compilation-type-file.
;;; 2010.12.13 Dan
;;;             : * Put the control functions and tests in place.
;;; 2010.12.15 Dan
;;;             : * Added module to the mapping functions args list.
;;; 2010.12.20 Dan
;;;             : * Added some additional code to handle things better when 
;;;             :   :ppm is enabled -- creates productions that match more closely
;;;             :   to the same conditions as the original pair did and does 
;;;             :   the "same" thing.
;;;             : * Allow subtypes to be composed in the consistency check.
;;; 2011.01.07 Dan
;;;             : * Fixed a bug with map-imaginal-buffer since it needs to be a
;;;             :   let* for ppm to be used.
;;; 2011.04.28 Dan
;;;             : * Added some declares to avoid compiler warnings.
;;; 2012.04.04 Dan [1.1]
;;;             : * Added the whynot reason function.
;;; 2014.05.07 Dan [2.0]
;;;             : * Start of conversion to typeless chunks.
;;;             : * Pass the module to constant-value-p.
;;;             : * References to compilation-module-previous are now using a
;;;             :   structure instead of list.
;;; 2016.08.04 Dan [3.0]
;;;             : * Updated the compilation type definition with one that was
;;;             :   created automatically by build-compilation-type-file from the
;;;             :   new spreadsheet that better handles "safe" compilation based
;;;             :   on whether the buffer is strict harvested or not.  Also added
;;;             :   some cases which should have been allowed but weren't like 13,40
;;;             :   where the * in the second can be combined with the + from the
;;;             :   first and 40,9 which can combine an = with a preceeding * (there
;;;             :   are some other similar additions but not listing all of them here).
;;;             :   Those new cases make things more consistent with respect to the
;;;             :   indicated constraints.
;;;             : * Added the code necessary to deal with the new cases and the 
;;;             :   new test functions.
;;; 2016.11.18 Dan
;;;             : * When the buffer is strict harvested in p1 (8 or 24) and p2 has
;;;             :   a query for buffer empty (16 or 20) then drop that buffer
;;;             :   empty query from the composed production.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defun MAP-IMAGINAL-BUFFER (module p1 p1-s p2 p2-s buffer)
 "map references from p1 to p2 for imaginal style buffer"
  ;; Possible cases: 
  ;;    a RHS + to a LHS = (includes when the RHS has both + and =)
  ;;    a RHS = to a LHS = 
  ;;    a LHS = with null RHS to a LHS =
  ;;    a RHS * to a LHS =
 
  (let* ((p1-style (cdr (assoc buffer (production-buffer-indices p1))))
         (p2-style (cdr (assoc buffer (production-buffer-indices p2))))
         (ppm (compilation-module-ppm module))
         (bindings (when ppm 
                     (append (previous-production-bindings (compilation-module-previous module))
                             (production-compilation-instan (production-name p2))))))
    
    (cond (;; The RHS + to LHS = case
           (and (find p1-style '(4 12 13 20 28 29))
                (find p2-style '(8 9 12 13 24 25 28 29 40 56)))
           
           ;; Map the RHS +'s with the LHS ='s
           
           ;; here the slots of interest are just the intersection 
           ;; of the two sets
           ;;
           
           (let* ((mappings nil)
                  (p1-slots (second (find (intern (concatenate 'string "+" (symbol-name buffer) ">")) (second p1-s) :key 'car)))
                  (p2-slots (second (find (intern (concatenate 'string "=" (symbol-name buffer) ">")) (first p2-s) :key 'car)))
                  (interesting-slots (intersection (mapcan (lambda (x)
                                                             (when (eq (spec-slot-op x) '=)
                                                               (list (spec-slot-name x))))
                                                     p1-slots)
                                                   (mapcan (lambda (x)
                                                               (when (eq (spec-slot-op x) '=)
                                                                 (list (spec-slot-name x))))
                                                     p2-slots))))
             
             (dolist (slot (remove-duplicates interesting-slots))
               (dolist (p1slots (remove-if-not (lambda (x) (and (eq (spec-slot-op x) '=) (eq (spec-slot-name x) slot))) p1-slots))
                 (dolist (p2slots (remove-if-not (lambda (x) (and (eq (spec-slot-op x) '=) (eq (spec-slot-name x) slot))) p2-slots))
                   (if (constant-value-p (spec-slot-value p2slots) module)
                       (if ppm
                           (if (constant-value-p (spec-slot-value p1slots) module)
                               (push (cons (spec-slot-value p1slots) (spec-slot-value p2slots)) mappings)
                             (push (find (spec-slot-value p1slots) bindings :key 'car) mappings))
                         
                         (push (cons (spec-slot-value p1slots) (spec-slot-value p2slots)) mappings))
                     (push (cons (spec-slot-value p2slots) (spec-slot-value p1slots)) mappings)))))
             mappings))
          
          (;; The RHS = to a LHS = case
           (and (find p1-style '(9 25))
                (find p2-style '(8 9 12 13 24 25 28 29)))
           
           ;; Map the RHS ='s and LHS ='s not in the RHS with
           ;; the LHS ='s
           
           ;; Here the slots of interest are the union of the
           ;; p1 bits with the RHS superseding the LHS intersected
           ;; with the LHS of the second one
           
           
           (let* ((mappings nil)
                  (p1-slotsa (second (find (intern (concatenate 'string "=" (symbol-name buffer) ">")) (first p1-s) :key 'car)))
                  (p1-slotsb (second (find (intern (concatenate 'string "=" (symbol-name buffer) ">")) (second p1-s) :key 'car)))
                  (p2-slots (second (find (intern (concatenate 'string "=" (symbol-name buffer) ">")) (first p2-s) :key 'car)))
                  
                  (p1-slots (append (remove-if (lambda (x)
                                                   (or (not (eq (spec-slot-op x) '=))
                                                       (find (spec-slot-name x) p1-slotsb :key 'spec-slot-name)))
                                               p1-slotsa)
                                    p1-slotsb))
                  (interesting-slots  (intersection (mapcan (lambda (x) 
                                                              (list (spec-slot-name x)))
                                                      p1-slots)
                                                    (mapcan (lambda (x)
                                                                (when (eq (spec-slot-op x) '=)
                                                                  (list (spec-slot-name x))))
                                                      p2-slots))))
             
                                     
             (dolist (slot (remove-duplicates interesting-slots))
               (dolist (p1slots (remove-if-not (lambda (x) (eq (spec-slot-name x) slot)) p1-slots))
                 (dolist (p2slots (remove-if-not (lambda (x) (eq (spec-slot-name x) slot)) p2-slots))
                   
                   (if (constant-value-p (spec-slot-value p2slots) module)
                       (if ppm
                           (if (constant-value-p (spec-slot-value p1slots) module)
                               (push (cons (spec-slot-value p1slots) (spec-slot-value p2slots)) mappings)
                             (push (find (spec-slot-value p1slots) bindings :key 'car) mappings))
                         
                         (push (cons (spec-slot-value p1slots) (spec-slot-value p2slots)) mappings))
                     (push (cons (spec-slot-value p2slots) (spec-slot-value p1slots)) mappings)))))
             
             mappings))
          
          (;; The RHS * to a LHS = case
           (and (find p1-style '(40 56))
                (find p2-style '(8 9 24 25 40 56)))
           
           ;; Map the RHS *'s and LHS ='s not in the RHS with
           ;; the LHS ='s
           
           ;; Here the slots of interest are the union of the
           ;; p1 bits with the RHS superseding the LHS intersected
           ;; with the LHS of the second one
           
           
           (let* ((mappings nil)
                  (p1-slotsa (second (find (intern (concatenate 'string "=" (symbol-name buffer) ">")) (first p1-s) :key 'car)))
                  (p1-slotsb (second (find (intern (concatenate 'string "*" (symbol-name buffer) ">")) (second p1-s) :key 'car)))
                  (p2-slots (second (find (intern (concatenate 'string "=" (symbol-name buffer) ">")) (first p2-s) :key 'car)))
                  
                  (p1-slots (append (remove-if (lambda (x)
                                                   (or (not (eq (spec-slot-op x) '=))
                                                       (find (spec-slot-name x) p1-slotsb :key 'spec-slot-name)))
                                               p1-slotsa)
                                    p1-slotsb))
                  (interesting-slots  (intersection (mapcan (lambda (x) 
                                                              (list (spec-slot-name x)))
                                                      p1-slots)
                                                    (mapcan (lambda (x)
                                                                (when (eq (spec-slot-op x) '=)
                                                                  (list (spec-slot-name x))))
                                                      p2-slots))))
             
                                     
             (dolist (slot (remove-duplicates interesting-slots))
               (dolist (p1slots (remove-if-not (lambda (x) (eq (spec-slot-name x) slot)) p1-slots))
                 (dolist (p2slots (remove-if-not (lambda (x) (eq (spec-slot-name x) slot)) p2-slots))
                   
                   (if (constant-value-p (spec-slot-value p2slots) module)
                       (if ppm
                           (if (constant-value-p (spec-slot-value p1slots) module)
                               (push (cons (spec-slot-value p1slots) (spec-slot-value p2slots)) mappings)
                             (push (find (spec-slot-value p1slots) bindings :key 'car) mappings))
                         
                         (push (cons (spec-slot-value p1slots) (spec-slot-value p2slots)) mappings))
                     (push (cons (spec-slot-value p2slots) (spec-slot-value p1slots)) mappings)))))
             
             mappings))
                    
          (;; The LHS = RHS null to a LHS = case
           (and (find p1-style '(8 24))
                (find p2-style '(8 9 12 13 24 25 28 29 40 56)))
           
           ;; Map the LHS ='s with the LHS ='s
           
           
           ;; The slots of interest are the ones at the intersection of the
           ;; two sets - the mappings are then done for those
           ;; such that 
           ;;   - if it's a variable in both then p2 vars go to p1 vars 
           ;;   - if it's a constant in one then it goes from the var to the constant
           ;;     (note that buffer variables are considered constants and not variables)
           ;;   - if it's a constant in both we're in trouble if they aren't equal
           ;;     because how did they fire...
           ;;
           ;; When there is more than one option we have to add both but they need to
           ;; be evaluated in the order of variables before constants (that's handled
           ;; elsewhere though)
           
           
           (let* ((mappings nil)
                  (p1-slots (cadr (find (intern (concatenate 'string "=" (symbol-name buffer) ">")) (first p1-s) :key 'car)))
                  (p2-slots (cadr (find (intern (concatenate 'string "=" (symbol-name buffer) ">")) (first p2-s) :key 'car)))
                  (interesting-slots (intersection (mapcan (lambda (x)
                                                               (when (eq (spec-slot-op x) '=)
                                                                  (list (spec-slot-name x))))
                                                     p1-slots)
                                                   (mapcan (lambda (x)
                                                               (when (eq (spec-slot-op x) '=)
                                                                  (list (spec-slot-name x))))
                                                     p2-slots))))
             
             (dolist (slot (remove-duplicates interesting-slots))
               (dolist (p1slots (remove-if-not (lambda (x) (and (eq (spec-slot-op x) '=) (eq (spec-slot-name x) slot))) p1-slots))
                 (dolist (p2slots (remove-if-not (lambda (x) (and (eq (spec-slot-op x) '=) (eq (spec-slot-name x) slot))) p2-slots))
                   (if (constant-value-p (spec-slot-value p2slots) module)
                       (if ppm
                           (if (constant-value-p (spec-slot-value p1slots) module)
                               (push (cons (spec-slot-value p1slots) (spec-slot-value p2slots)) mappings)
                             (push (find (spec-slot-value p1slots) bindings :key 'car) mappings))
                         
                         (push (cons (spec-slot-value p1slots) (spec-slot-value p2slots)) mappings))
                     (push (cons (spec-slot-value p2slots) (spec-slot-value p1slots)) mappings)))))
             mappings))
          
          (t
           nil))))


(defun COMPOSE-IMAGINAL-BUFFER (p1 p1-s p2 p2-s buffer)
  ;; Generally:
  ;;   If the first has a + (4, 12, 13, 20, 28 29) then
  ;;      the conditions are those of the first including a query
  ;;      the actions are the = or * of the first if there is one and
  ;;      the + will be the + of the first (can't be a + in the second)
  ;;      with the = or * of the second unioned in and overriding
  
  ;;   If the first has no actions (0, 8, 16 24)
  ;;      the buffer conditions are the union of those in the first
  ;;      and those of the second with the query from the first being
  ;;      used if there is one (16 24) otherwise the query from the second
  ;;      is used if there is one except that if the buffer is strict
  ;;      harvested a buffer empty query from p2 is dropped in cases 8 and 24
  ;;      the actions are those of the second
  
  ;;   Otherwise (9 40 25 56)
  ;;      the conditions are the union of those in the first
  ;;      and those from the second that are not set by the 
  ;;      actions of the first If there is a query in the first it is
  ;;      used (25 56) otherwise a query from the second is used
  ;;      the actions are the = or * from the first with the = or * from
  ;;      the second unioned in and overriding and
  ;;      the + of the second if there is one
  ;;
  
  
  (let* ((bn (intern (concatenate 'string (symbol-name buffer) ">")))
         (b= (intern (concatenate 'string "=" (symbol-name bn))))
         (b+ (intern (concatenate 'string "+" (symbol-name bn))))
         (b? (intern (concatenate 'string "?" (symbol-name bn))))
         (b* (intern (concatenate 'string "*" (symbol-name bn))))
         
         (c1 (copy-tree (find b= (first p1-s) :key 'car)))
         (c2 (copy-tree (find b= (first p2-s) :key 'car)))
         (q1 (copy-tree (find b? (first p1-s) :key 'car)))
         (q2 (copy-tree (find b? (first p2-s) :key 'car)))
         
         (a1= (copy-tree (find b= (second p1-s) :key 'car)))
         (a2= (copy-tree (find b= (second p2-s) :key 'car)))
         
         (a1+ (copy-tree (find b+ (second p1-s) :key 'car)))
         (a2+ (copy-tree (find b+ (second p2-s) :key 'car)))
         (a1* (copy-tree (find b* (second p1-s) :key 'car)))
         (a2* (copy-tree (find b* (second p2-s) :key 'car))))
    
    (case (aif (cdr (assoc buffer (production-buffer-indices p1))) it 0)
      ((4 12 13 20 28 29)
       (list (append 
              (when c1 
                (list c1)) 
              (when q1 
                (list q1)))
             (append 
              (when (or a1= a1*) ;; can't have both with current description
                (if a1= 
                    (list a1=) 
                  (list a1*))) 
              (cond ((and a1+ a2=)
                     (awhen (buffer+-union a1+ a2=) 
                            (list it)))
                    ((and a1+ a2*)
                     (awhen (buffer+-union a1+ a2*) 
                            (list it)))
                    (a1+
                     (list a1+))
                    (t 
                     nil)))))
      ((0 16)
       (list (append 
              (awhen (buffer-condition-union c1 c2 a1=) 
                     (list it))  
              (if q1 
                  (list q1) 
                (if q2 
                    (list q2)
                  nil)))
             (append 
              (when a2= 
                (list a2=))
              (when a2* 
                (list a2*)) 
              (when a2+ 
                (list a2+)))))
      ((8 24)
       (list (append 
              (awhen (buffer-condition-union c1 c2 a1=) 
                     (list it))  
              (if q1 
                  (list q1) 
                (if q2 
                    (if (find buffer (no-output (car (sgp :do-not-harvest))))
                        (list q2)
                      ;; strict harvested so need to ignore a buffer empty query from p2
                      (progn
                        (setf (second q2) (remove '(= buffer empty) (second q2) :test 'equalp))
                        (if (second q2)
                            (list q2)
                          nil)))
                  nil)))
             (append 
              (when a2= 
                (list a2=))
              (when a2* 
                (list a2*)) 
              (when a2+ 
                (list a2+)))))
      ((9 40 25 56)
       (let ((p1-index (cdr (assoc buffer (production-buffer-indices p1))))
             (p2-index (cdr (assoc buffer (production-buffer-indices p2)))))
         (list (append 
              (awhen (buffer-condition-union c1 c2 (if a1= a1= a1*)) 
                     (list it))
              (if q1 
                  (list q1) 
                (if (and q2 ;; when it's a * followed by a state free query drop the query
                         (not (and (= p1-index 40) (or (= p2-index 24) (= p2-index 25) (= p2-index 56)))))
                    (list q2) 
                  nil)))
             (append (cond ((and a1* a2=)
                             (awhen (buffer=-union a1* a2=)
                                    (list it)))
                            ((or a1= a2=) ;; if there's at least one = union those                             
                             (awhen (buffer=-union a1= a2=) 
                                    (list it)))
                            ((or a1* a2*) ;; if there's at least one * union those
                             (awhen (buffer=-union a1* a2*) 
                                    (list it)))

                            (t nil)) ;; can't have other mix of = and * so just ignore 
                     (when a2+ 
                       (list a2+)))))))))


(defun CHECK-IMAGINAL-CONSISTENCY (buffer module p1 p2)
  (case (get-buffer-index p1 buffer)
    ((4 12 13 20 28 29) ;; a RHS +
     (check-consistency module (find-if (lambda (x)
                                          (and (eq (production-statement-op x) #\+)
                                               (eq (production-statement-target x) buffer)))
                                        (production-rhs p1))
                        (previous-production-bindings (compilation-module-previous module))
                        (find-if (lambda (x)
                                          (and (eq (production-statement-op x) #\=)
                                               (eq (production-statement-target x) buffer)))
                                 (production-lhs p2))
                        (production-bindings p2)))
       
    ((9 25) ;; a RHS =
     (check-consistency module (find-if (lambda (x)
                                          (and (eq (production-statement-op x) #\=)
                                               (eq (production-statement-target x) buffer)))
                                        (production-rhs p1))
                        (previous-production-bindings (compilation-module-previous module))
                        (find-if (lambda (x)
                                          (and (eq (production-statement-op x) #\=)
                                               (eq (production-statement-target x) buffer)))
                                 (production-lhs p2))
                        (production-bindings p2)))
    
    
    ((40 56) ;; a RHS *
     (check-consistency module (find-if (lambda (x)
                                          (and (eq (production-statement-op x) #\*)
                                               (eq (production-statement-target x) buffer)))
                                        (production-rhs p1))
                        (previous-production-bindings (compilation-module-previous module))
                        (find-if (lambda (x)
                                          (and (eq (production-statement-op x) #\=)
                                               (eq (production-statement-target x) buffer)))
                                 (production-lhs p2))
                        (production-bindings p2)))
    
    (t
     t)))

(defun PRE-INSTANTIATE-IMAGINAL (buffer-and-index p2)
  (declare (ignore p2 buffer-and-index))
  t ;; Should always be done, right???
  )


(defun I-B-C3 (buffer p1 p2)
  "Compilation check queries in p2 for 'state free'"
  (declare (ignore p1))
  ;; there can be only one
  (let ((query (find-if (lambda (x)
                          (and (eq (production-statement-op x) #\?)
                               (eq (production-statement-target x) buffer)))
                        (production-lhs p2))))
    (every (lambda (x)      
             (equalp x '(= state free)))
           (chunk-spec-slot-spec (production-statement-spec query)))))


(defun I-B-C1 (buffer p1 p2)
"Compilation check for queries such that p2 only uses 'buffer empty' or 'state busy'"
  (declare (ignore p1))
  (let ((query (find-if (lambda (x)
                          (and (eq (production-statement-op x) #\?)
                               (eq (production-statement-target x) buffer)))
                        (production-lhs p2))))
    (every (lambda (x)      
             (or 
              (equalp x '(= state busy))
              (equalp x '(= buffer empty))))
           (chunk-spec-slot-spec (production-statement-spec query)))))

(defun NO-RHS-IMAGINAL-REF (buffer p1 p2)
  "Can't compile if the variable naming the buffer is used in the actions of p2"
  (declare (ignore p1))
  (not (recursive-find (intern (concatenate 'string "=" (symbol-name buffer)))
                       (second (production-standard-rep p2))))) 

(defun I-B-C4 (buffer p1 p2)
  (and (i-b-c3 buffer p1 p2)
       (no-rhs-imaginal-ref buffer p1 p2)))

(defun I-B-C2 (buffer p1 p2)
 "queries in p1 and p2 must be the same
   NOTE: this doesn't take into account any variables at this time"
  (let ((query1 (awhen (find-if (lambda (x)
                                  (and (eq (production-statement-op x) #\?)
                                       (eq (production-statement-target x) buffer)))
                                (production-lhs p1))
                       (chunk-spec-slot-spec (production-statement-spec it))))
        (query2 (awhen (find-if (lambda (x)
                                  (and (eq (production-statement-op x) #\?)
                                       (eq (production-statement-target x) buffer)))
                                (production-lhs p2))
                       (chunk-spec-slot-spec (production-statement-spec it)))))
    
    (= (length query1) (length query2) 
       (length (remove-duplicates (append query1 query2) :test 'equal)))))

(defun I-B-C5 (buffer p1 p2)
  "only if the buffer is not strict harvested"
  (declare (ignore p1 p2))
  (find buffer (no-output (car (sgp :do-not-harvest)))))

(defun I-B-C6 (buffer p1 p2)
  "Not strict harvested and same queries"
  (and (i-b-c5 buffer p1 p2) (i-b-c2 buffer p1 p2)))

(defun I-B-C7 (buffer p1 p2)
  "Not strict harvested and no RHS imaginal references"
  (and (i-b-c5 buffer p1 p2) (no-rhs-imaginal-ref buffer p1 p2)))

(defun I-B-C8 (buffer p1 p2)
  "Not strict harvested and p2 query must be state free"
  (and (i-b-c5 buffer p1 p2) (i-b-c3 buffer p1 p2)))
           

(defun imaginal-reason (p1-index p2-index failed-function)
  (cond ((eql failed-function 'no-rhs-imaginal-ref)
         "the buffer variable cannot be used in the actions of the second production if there is a request in the first production.")
        ((eql failed-function 'i-b-c1)
         "when the first production makes a request and the second does not harvest it the second can only query for state busy or buffer empty.")
        ((eql failed-function 'i-b-c2)
         "the queries in both productions must be the same.")
        ((eql failed-function 'i-b-c3)
         "when the first production makes a request and the second harvests it the second can only query for state free.")
        ((eql failed-function 'i-b-c4)
         "when the first production makes a request and the second harvests it the second can only query for state free and the buffer variable cannot be used in the actions of the second.")
        ((eql failed-function 'i-b-c5)
         "strict harvesting is enabled for the buffer and either the first production cleared the buffer or the combined production would leave the buffer with a chunk while the original pair would have left it empty.")
        ((eql failed-function 'i-b-c6)
         "either strict harvesting is enabled for the buffer and the combined production would leave the buffer with a chunk while the original pair would have left it empty or the queries in the productions are not the same.")
        ((eql failed-function 'i-b-c7)
         "either strict harvesting is enabled for the buffer and the combined production would leave the buffer with a chunk while the original pair would have left it empty or the buffer variable cannot be used in the actions of the second production if there is a request in the first production.")
        ((eql failed-function 'i-b-c8)
         "either strict harvesting is enabled for the buffer and the combined production would leave the buffer with a chunk while the original pair would have left it empty or the first production makes a request and the second harvests it so it can only query for state free.")
        
        (t 
         (case p1-index
           ((2 6 10 11 14 15 42 43 46 47 18 22 26 27 30 31 58 59 63 62)
            "the buffer is explicitly cleared in the first production.")
           ((44 45 60 61)
            "the first production makes both a request and a modification request.")
           ((41 57)
            "the first production makes both a modification and a modification request.")
           (t
            (case p2-index
              ((2 6 10 11 14 15 42 43 46 47 18 22 26 27 30 31 58 59 63 62)
               "the buffer is explicitly cleared in the second production.")
              ((44 45 60 61)
               "the second production makes both a request and a modification request.")
              ((41 57)
               "the second production makes both a modification and a modification request.")
              ((40 56)
               "the first production makes a modification and the second makes a modification request.")
              (t
               (case p1-index
                 ((4 12 13 20 28 29)
                  "both productions make requests.")
                 (t
                  (case p2-index
                    ((4 12 13 20 28 29)
                     "the first production makes a modification request and the second makes a request.")
                    (t
                     "the first production makes a modification request and the other makes a modification.")))))))))))

(define-compilation-type IMAGINAL ((56 56 I-B-C3)
                                   (56 40 T)
                                   (56 25 I-B-C3)
                                   (56 24 I-B-C8)
                                   (56 16 I-B-C1)
                                   (56 9 T)
                                   (56 8 I-B-C5)
                                   (56 0 T)
                                   (40 56 I-B-C3)
                                   (40 40 T)
                                   (40 25 I-B-C3)
                                   (40 24 I-B-C8)
                                   (40 16 I-B-C1)
                                   (40 9 T)
                                   (40 8 I-B-C5)
                                   (40 0 T)
                                   (29 56 I-B-C4)
                                   (29 40 NO-RHS-IMAGINAL-REF)
                                   (29 25 I-B-C3)
                                   (29 24 I-B-C8)
                                   (29 16 I-B-C1)
                                   (29 9 NO-RHS-IMAGINAL-REF)
                                   (29 8 I-B-C7)
                                   (29 0 T)
                                   (28 56 I-B-C4)
                                   (28 40 NO-RHS-IMAGINAL-REF)
                                   (28 25 I-B-C3)
                                   (28 24 I-B-C8)
                                   (28 16 I-B-C1)
                                   (28 9 NO-RHS-IMAGINAL-REF)
                                   (28 8 I-B-C7)
                                   (28 0 T)
                                   (25 29 I-B-C2)
                                   (25 28 I-B-C2)
                                   (25 25 I-B-C2)
                                   (25 24 I-B-C6)
                                   (25 20 I-B-C2)
                                   (25 16 I-B-C2)
                                   (25 13 T)
                                   (25 12 T)
                                   (25 9 T)
                                   (25 8 I-B-C5)
                                   (25 4 T)
                                   (25 0 T)
                                   (24 56 I-B-C6)
                                   (24 40 I-B-C6)
                                   (24 29 I-B-C6)
                                   (24 28 I-B-C6)
                                   (24 25 I-B-C6)
                                   (24 24 I-B-C6)
                                   (24 20 I-B-C2)
                                   (24 16 I-B-C2)
                                   (24 13 I-B-C5)
                                   (24 12 I-B-C5)
                                   (24 9 I-B-C5)
                                   (24 8 I-B-C5)
                                   (24 4 T)
                                   (24 0 T)
                                   (20 56 I-B-C4)
                                   (20 40 NO-RHS-IMAGINAL-REF)
                                   (20 25 I-B-C3)
                                   (20 24 I-B-C8)
                                   (20 16 I-B-C1)
                                   (20 9 NO-RHS-IMAGINAL-REF)
                                   (20 8 I-B-C7)
                                   (20 0 T)
                                   (16 56 I-B-C2)
                                   (16 40 T)
                                   (16 29 I-B-C2)
                                   (16 28 I-B-C2)
                                   (16 25 I-B-C2)
                                   (16 24 I-B-C2)
                                   (16 20 I-B-C2)
                                   (16 16 I-B-C2)
                                   (16 13 T)
                                   (16 12 T)
                                   (16 9 T)
                                   (16 8 T)
                                   (16 4 T)
                                   (16 0 T)
                                   (13 56 I-B-C4)
                                   (13 40 NO-RHS-IMAGINAL-REF)
                                   (13 25 I-B-C3)
                                   (13 24 I-B-C8)
                                   (13 16 I-B-C1)
                                   (13 9 NO-RHS-IMAGINAL-REF)
                                   (13 8 I-B-C7)
                                   (13 0 T)
                                   (12 56 I-B-C4)
                                   (12 40 NO-RHS-IMAGINAL-REF)
                                   (12 25 I-B-C3)
                                   (12 24 I-B-C8)
                                   (12 16 I-B-C1)
                                   (12 9 NO-RHS-IMAGINAL-REF)
                                   (12 8 I-B-C7)
                                   (12 0 T)
                                   (9 29 T)
                                   (9 28 T)
                                   (9 25 T)
                                   (9 24 I-B-C5)
                                   (9 20 T)
                                   (9 16 T)
                                   (9 13 T)
                                   (9 12 T)
                                   (9 9 T)
                                   (9 8 I-B-C5)
                                   (9 4 T)
                                   (9 0 T)
                                   (8 56 I-B-C5)
                                   (8 40 I-B-C5)
                                   (8 29 I-B-C5)
                                   (8 28 I-B-C5)
                                   (8 25 I-B-C5)
                                   (8 24 I-B-C5)
                                   (8 20 T)
                                   (8 16 T)
                                   (8 13 I-B-C5)
                                   (8 12 I-B-C5)
                                   (8 9 I-B-C5)
                                   (8 8 I-B-C5)
                                   (8 4 T)
                                   (8 0 T)
                                   (4 56 I-B-C4)
                                   (4 40 NO-RHS-IMAGINAL-REF)
                                   (4 25 I-B-C3)
                                   (4 24 I-B-C8)
                                   (4 16 I-B-C1)
                                   (4 9 NO-RHS-IMAGINAL-REF)
                                   (4 8 I-B-C7)
                                   (4 0 T)
                                   (0 56 T)
                                   (0 40 T)
                                   (0 29 T)
                                   (0 28 T)
                                   (0 25 T)
                                   (0 24 T)
                                   (0 20 T)
                                   (0 16 T)
                                   (0 13 T)
                                   (0 12 T)
                                   (0 9 T)
                                   (0 8 T)
                                   (0 4 T)) (IMAGINAL) MAP-IMAGINAL-BUFFER COMPOSE-IMAGINAL-BUFFER CHECK-IMAGINAL-CONSISTENCY PRE-INSTANTIATE-IMAGINAL NIL IMAGINAL-REASON)

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
