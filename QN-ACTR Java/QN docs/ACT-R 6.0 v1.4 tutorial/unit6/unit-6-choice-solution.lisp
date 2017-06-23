(defconstant *unit8-choice-data* '(0.66 0.78 0.82 0.84))

(defvar *response*)

(defun do-trial ()
   (if *actr-enabled-p*
       (do-trial-model)
     (do-trial-person)))

(defun do-trial-model ()
   (let ((window (open-exp-window "Choice Experiment" :visible nil)) feedback)

     (add-text-to-exp-window :text "choose")
     (setf *response* nil)

     (install-device window)
     (proc-display)
     (run 30.0)

     (clear-exp-window)
     (setf feedback (if (< (random 1.0) .9) "H" "T"))
     (add-text-to-exp-window :text (if (equal feedback "H") "heads" "tails")
                             :x 50 :y 100)

     (proc-display)
     (run-full-time 1.0)
     *response*))


(defun do-trial-person ()
   (let ((window (open-exp-window "Choice Experiment" :visible t)))

     (add-text-to-exp-window :text "choose" :x 50 :y 100)

     (setf *response* nil)

     (while (null *response*)
            (allow-event-manager window))

     (clear-exp-window)

     (add-text-to-exp-window :text (if (< (random 1.0) .9) "heads" "tails")
                             :x 50 :y 100)

     (sleep 1.0)
     *response*))


(defun do-block-of-m-trials (m)
   (let ((count 0))
     (dotimes (i m count)
       (when (string-equal "H" (do-trial))
         (incf count)))))

(defun do-n-blocks-of-m-trials (n m)
   (let (result)
     (dotimes (i n (reverse result))
       (push (do-block-of-m-trials m) result))))


(defun collect-data (n)
   (let (data)
     (dotimes (i n)
       (when *actr-enabled-p*  (reset))
       (push (do-n-blocks-of-m-trials 4 12) data))
     (print-results (analyze data))))

(defun analyze (data)
   (let ((n (length data))
         (result nil))
     (dotimes (i (length (car data)) (reverse result))
       (push (/ (apply #'+ (mapcar #'(lambda (x) (nth i x)) data)) (* n 12)) result))))

(defun print-results (results)
   (correlation results *unit8-choice-data*)
   (mean-deviation results *unit8-choice-data*)
   (format t " Original     Current~%")
   (dotimes (i 4)
     (format t "~8,3F    ~8,3F~%" (nth i *unit8-choice-data*) (nth i results))))


(defmethod rpm-window-key-event-handler ((win rpm-window) key)
   (setf *response* (string-upcase (string key))))

(clear-all)

(define-model choice

    (sgp :esc t :egs 0.7 :v nil :ul t)

(chunk-type choose state item)

(add-dm (goal isa choose))

(P attend-item
    =goal>
       ISA         choose
       state        nil
    =visual-location>
       ISA         visual-location
    ?visual>
        state         free
==>
    +visual>
       ISA         move-attention
       screen-pos  =visual-location
    =goal>
       state       attending
)

(p heads
    =goal>
       ISA         choose
       state       attending
    =visual>
       isa         text
    value       "choose"

    ?manual>
     state      free

==>
    =goal>
       item        "heads"
       state       nil
    +manual>
       ISA         press-key
       key         "H"
    )

(p tails
    =goal>
       ISA         choose
       state       attending
    =visual>
       isa         text
       value       "choose"
  ?manual>
    state      free

==>
    =goal>
       item        "tails"
       state       nil
    +manual>
       ISA         press-key
       key         "T"
    )

(p match
    =goal>
       ISA         choose
       state       attending
       item        =val
    =visual>
       isa         text
       value       =val
==>
    +goal>
       isa         choose)

(p mismatch
    =goal>
       ISA         choose
       state       attending
       item        =val
    =visual>
       isa         text
    -  value       =val
==>
    +goal>
       isa         choose)

(setf *actr-enabled-p* t)

(goal-focus goal)

(spp match :reward 2)
(spp mismatch :reward 0)
)