(defvar *stick-a*)
(defvar *stick-b*)
(defvar *stick-c*)
(defvar *target*)
(defvar *current-stick*)
(defvar *current-line*)
(defvar *done*)
(defvar *choice*)
(defvar *experiment-window* nil)
(defvar *visible* nil)
(defvar *learning* nil)

(defvar *bst-exp-data* '(20.0 67.0 20.0 47.0 87.0 20.0 80.0 93.0
                         83.0 13.0 29.0 27.0 80.0 73.0 53.0))

(defvar *exp-stimuli* '((15  250  55  125)(10  155  22  101)
                        (14  200  37  112)(22  200  32  114)
                        (10  243  37  159)(22  175  40  73)
                        (15  250  49  137)(10  179  32  105)
                        (20  213  42  104)(14  237  51  116)
                        (12  149  30  72)
                        (14  237  51  121)(22  200  32  114)
                        (14  200  37  112)(15  250  55  125)))

(defvar *no-learn-stimuli* '((15  200  41 103)(10  200 29 132)))

(defun build-display (a b c target)
  (setf *experiment-window* (open-exp-window "Building Sticks Task"
                                             :visible *visible*
                                             :width 600
                                             :height 400))
  (setf *stick-a* a)
  (setf *stick-b* b)
  (setf *stick-c* c)
  (setf *target* target)
  (setf *current-stick* 0)
  (setf *done* nil)
  (setf *choice* nil)
  (setf *current-line* nil)
  
  (allow-event-manager *experiment-window*)  
  
  (add-button-to-exp-window :x 5 :y 23  :height 24 :width 40 :text "A"     :action 'button-a-pressed)
  (add-button-to-exp-window :x 5 :y 48  :height 24 :width 40 :text "B"     :action 'button-b-pressed)
  (add-button-to-exp-window :x 5 :y 73  :height 24 :width 40 :text "C"     :action 'button-c-pressed)
  (add-button-to-exp-window :x 5 :y 123 :height 24 :width 65 :text "Reset" :action 'reset-display)
  
  (add-line-to-exp-window (list 75 35)  (list (+ a 75) 35) :color 'black)
  (add-line-to-exp-window (list 75 60)  (list (+ b 75) 60) :color 'black)
  (add-line-to-exp-window (list 75 85)  (list (+ c 75) 85) :color 'black)
  (add-line-to-exp-window (list 75 110) (list (+ target 75) 110) :color 'green)
  
  (allow-event-manager *experiment-window*))

(defun button-a-pressed (button)
  (declare (ignore button))
  
  (unless *choice* 
    (setf *choice* 'under))

  (unless *done*
    (if (> *current-stick* *target*)
        (setf *current-stick* (- *current-stick* *stick-a*))
      (setf *current-stick* (+ *current-stick* *stick-a*)))
    (update-current-line)))

(defun button-b-pressed (button)
  (declare (ignore button))
  
  (unless *choice* 
    (setf *choice* 'over))
  
  (unless *done*
    (if (> *current-stick* *target*)
        (setf *current-stick* (- *current-stick* *stick-b*))
      (setf *current-stick* (+ *current-stick* *stick-b*)))
    (update-current-line)))

(defun button-c-pressed (button)
  (declare (ignore button))
  
  (unless *choice* 
    (setf *choice* 'under))
  
  (unless *done*
    (if (> *current-stick* *target*)
        (setf *current-stick* (- *current-stick* *stick-c*))
      (setf *current-stick* (+ *current-stick* *stick-c*)))
    (update-current-line)))

(defun reset-display (button)
  (declare (ignore button))

  (unless *done*
    (setf *current-stick* 0)
    (update-current-line)))

(defun update-current-line ()
  (cond ((= *current-stick* *target*)
         (setf *done* t) 
         (modify-line-for-exp-window *current-line* (list 75 135) (list  (+ *target* 75) 135))
         (add-text-to-exp-window :x 180 :y 200 :width 50 :text "Done"))
        ((zerop *current-stick*)
         (when *current-line*
           (remove-items-from-exp-window *current-line*)
           (setf *current-line* nil)))
        (*current-line*
          (modify-line-for-exp-window *current-line* (list 75 135) (list (+ *current-stick* 75) 135)))
        (t
         (setf *current-line* (add-line-to-exp-window (list 75 135) (list (+ *current-stick* 75) 135) :color 'blue))))
  
   (allow-event-manager *experiment-window*)

   (proc-display))

(defun do-experiment (sticks who)
  (apply 'build-display sticks)
  (install-device *experiment-window*)
  
  (if (eq who 'human)
      (wait-for-human)
    (progn
      (proc-display :clear t)
      (run 60 :real-time *visible*))))

(defun wait-for-human ()
  (while (not *done*)
    (allow-event-manager *experiment-window*))
  (sleep 1))

(defun bst-set (who visible stims)
  (setf *visible* visible)
  (let ((result nil))
    (reset)
    (dolist (stim stims)
      (do-experiment stim who)
      (push *choice* result))
    (reverse result)))

(defun bst-test (n &optional who)
  (let ((stims *no-learn-stimuli*))
    (setf *learning* nil)
    (let ((result (make-list (length stims) :initial-element 0)))
      (dotimes (i n result)
        (setf result (mapcar '+ 
                       result 
                       (mapcar (lambda (x) 
                                 (if (equal x 'over) 1 0))
                         (bst-set who (or (eq who 'human) (= n 1)) stims))))))))

(defun bst-experiment (n &optional who)
  (let ((stims *exp-stimuli*))
    (setf *learning* t)
    (let ((result (make-list (length stims) :initial-element 0))
          (p-values (list '(decide-over 0) '(decide-under 0))))
      (dotimes (i n result)
        (setf result (mapcar '+ result 
                       (mapcar (lambda (x) 
                                 (if (equal x 'over) 1 0)) 
                         (bst-set who (eq who 'human) stims))))
        (setf p-values (mapcar (lambda (x) 
                                 (list (car x) (+ (second x) (production-u-value (car x)))))
                         p-values)))
      
      (setf result (mapcar (lambda (x) (* 100.0 (/ x n))) result))
      
      (when (= (length result) (length *bst-exp-data*))
        (correlation result *bst-exp-data*)
        (mean-deviation result *bst-exp-data*))
      
      (format t "~%Trial ")
      
      (dotimes (i (length result))
        (format t "~8s" (1+ i)))
      
      (format t "~%  ~{~8,2f~}~%~%" result)
      
      (dolist (x p-values)
        (format t "~12s: ~6,4f~%" (car x) (/ (second x) n))))))

(defun production-u-value (prod)
   (caar (no-output (spp-fct (list prod :u)))))


(defun number-sims (a b)
  (when (and (numberp a) (numberp b))
    (/ (abs (- a b)) -300)))

(defun compute-difference ()
  (let* ((chunk (buffer-read 'imaginal))
         (new-chunk (copy-chunk-fct chunk)))
    (mod-chunk-fct new-chunk (list 'difference (abs (- (chunk-slot-value-fct chunk 'length)
                                                       (chunk-slot-value-fct chunk 'goal-length)))))))
  
(clear-all)

(define-model bst-learn

  (sgp :v nil :esc t :egs 3 :show-focus t :trace-detail medium :ul t :ult t)
  (sgp :ppm 40 :sim-hook number-sims)
  
(chunk-type try-strategy strategy state)
(chunk-type encoding a-loc b-loc c-loc goal-loc length goal-length b-len c-len difference)

(define-chunks (start)(find-line)(looking)(attending)(choose-strategy)
  (consider-next)(check-for-done)(read-done)(evaluate-c)(prepare-mouse)
  (evaluate-a)(over)(under)(move-mouse)(wait-for-click))

(add-dm (goal isa try-strategy state start))

(declare-buffer-usage imaginal encoding difference length)

(p start-trial
    =goal>
      isa    try-strategy
      state  start
    ?visual-location>
      buffer unrequested
   ==>
    =goal>
      state  find-line)

(p find-next-line
    =goal>
      isa       try-strategy
      state     find-line
   ==>
    +visual-location>
      isa       visual-location
      :attended nil
      kind      line
      screen-y  lowest
    =goal>
      state     looking)

(p attend-line
    =goal>
      isa     try-strategy
      state   looking
    =visual-location>
    ?visual>
      state   free
   ==>
    =goal>
      state      attending
    +visual>
      cmd        move-attention
      screen-pos =visual-location)

(p encode-line-a
    =goal>
      isa        try-strategy
      state      attending
    =visual>
      isa        line
      screen-pos =pos
    ?imaginal>
      buffer     empty
      state      free
   ==>
    +imaginal>
      isa      encoding
      a-loc    =pos
    =goal>
      state    find-line)

(p encode-line-b
    =goal>
      isa        try-strategy
      state      attending
    =imaginal>
      isa        encoding
      a-loc      =a
      b-loc      nil
    =visual>
      isa        line
      screen-pos =pos
      width      =b-len
   ==>
    =imaginal>
      b-loc    =pos
      b-len    =b-len
    =goal>
      state    find-line)

(p encode-line-c
    =goal>
      isa        try-strategy
      state      attending
    =imaginal>
      isa        encoding
      b-loc      =b
      c-loc      nil
    =visual>
      isa        line
      screen-pos =pos
      width      =c-len
   ==>
    =imaginal>
      c-loc    =pos
      c-len    =c-len
    =goal>
      state    find-line)

(p encode-line-goal
    =goal>
      isa        try-strategy
      state      attending
    =imaginal>
      isa        encoding
      c-loc      =c
      goal-loc   nil
    =visual>
      isa        line
      screen-pos =pos
      width      =length
    ?visual>
      state      free
   ==>
    =imaginal>
      goal-loc     =pos
      goal-length  =length
    =goal>
      state        choose-strategy)

(p encode-line-current
    =goal>
      isa      try-strategy
      state    attending
    =imaginal>
      isa      encoding
      goal-loc =goal-loc
    =visual>
      isa      line
      width    =current-len
    ?visual>
      state    free
    ?imaginal-action> 
      state    free   
  ==>
    =imaginal>
      length     =current-len
    +imaginal-action>
      action     compute-difference
      simple     t
    =goal>
      state      consider-next
    +visual>
      cmd        move-attention
      screen-pos =goal-loc)

(p check-for-done
    =goal>
      isa         try-strategy
      state       consider-next
    =imaginal>
      isa         encoding
     > difference -1  
     < difference 1
    ?imaginal>
      state       free
   ==>
    =goal>
      state     check-for-done
    +visual-location>
      isa       visual-location
     > screen-y 200)

(p find-done
    =goal>
      isa      try-strategy
      state    check-for-done
    =visual-location>
    ?visual>
      state    free
   ==>
    +visual>
      cmd        move-attention
      screen-pos =visual-location
    =goal>
      state      read-done)

(p read-done
    =goal>
      isa      try-strategy
      state    read-done
    =visual>
      isa      text
      value    "done"
==>
    +goal>
      isa      try-strategy
      state    start)

(p consider-c
    =goal>
      isa         try-strategy
      state       consider-next
    =imaginal>
      isa         encoding
      c-loc       =c-loc
     - difference 0 
    ?imaginal>
     state        free
    ?visual>
       state      free
   ==>
    =imaginal>
    =goal>
      state      evaluate-c
    +visual>
      cmd        move-attention
      screen-pos =c-loc)

(p choose-c
    =goal>
      isa        try-strategy
      state      evaluate-c
    =imaginal>
      isa        encoding
      difference =difference
    =visual>
      isa        line
    <= width     =difference
   ==>
    =imaginal>
    =goal>
      state    prepare-mouse
    +visual-location>
      isa      visual-location
      kind     oval
      screen-y 85)

(p consider-a
    =goal>
      isa        try-strategy
      state      evaluate-c
    =imaginal>
      isa        encoding
      a-loc      =a-loc
      difference =difference
    =visual>
      isa        line
    > width      =difference
    ?visual>
      state free
   ==>
    =imaginal>
    =goal>
      state      evaluate-a
    +visual>
      cmd        move-attention
      screen-pos =a-loc)

(p choose-a
    =goal>
      isa        try-strategy
      state      evaluate-a
    =imaginal>
      isa        encoding
      difference =difference
    =visual>
      isa        line
    <= width     =difference
   ==>
    =imaginal>
    =goal>
      state    prepare-mouse
    +visual-location>
      isa      visual-location
      kind     oval
      screen-y 35)

(p reset
    =goal>
      isa        try-strategy
      state      evaluate-a
    =imaginal>
      isa        encoding
      difference =difference
    =visual>
      isa        line
     > width     =difference
   ==>
    =imaginal>
    =goal>
      state    prepare-mouse
    +visual-location>
      isa      visual-location
      kind     oval
      screen-y 135)

(p decide-over
    =goal>
      isa         try-strategy
      state       choose-strategy
     - strategy   over
    =imaginal>
      isa         encoding
      goal-length =d
      b-len       =d
   ==>
    =imaginal>
    =goal>
      state    prepare-mouse
      strategy over
    +visual-location>
      isa      visual-location
      kind     oval
      screen-y 60)

(p decide-under
    =goal>
      isa         try-strategy
      state       choose-strategy
    -  strategy   under
    =imaginal>
      isa         encoding
      goal-length =d
      c-len       =d
   ==>
    =imaginal>
    =goal>
      state    prepare-mouse
      strategy under
    +visual-location>
      isa      visual-location
      kind     oval
      screen-y 85)

(p move-mouse
    =goal>
      isa    try-strategy
      state  prepare-mouse
    =visual-location>
    ?visual>
      state  free
    ?manual>
      state  free
   ==>
    =visual-location>
    +visual>
      cmd        move-attention
      screen-pos =visual-location
    =goal>
      state      move-mouse
    +manual>
      cmd        move-cursor
      loc        =visual-location)

(p click-mouse
    =goal>
      isa     try-strategy
      state   move-mouse
    ?manual>
      state   free
   ==>
    =goal>
      state   wait-for-click
    +manual>
      cmd     click-mouse)

(p look-for-current
    =goal>
      isa       try-strategy
      state     wait-for-click
    ?manual>
      state     free
    =visual-location>
      isa       visual-location
     < screen-y 100
   ==>
    +visual-location>
      isa       visual-location
      :attended nil
      kind      line
      screen-y  highest
    =goal>
      state     looking)

(p pick-another-strategy
    =goal>
      isa       try-strategy
      state     wait-for-click
    ?manual>
      state     free
    =visual-location>
      isa       visual-location
     > screen-y 100
   ==>
    =goal>
      state choose-strategy)

(start-hand-at-mouse)

(goal-focus goal)

(spp decide-over :u 10)
(spp decide-under :u 10)

(spp pick-another-strategy :reward 0)
(spp read-done :reward 20)
)
