(defconstant *unit8-choice-data* '(0.66 0.78 0.82 0.84))

(defvar *responses*)
(defvar *trials-left*)

(defun present-choice ()
  
  (clear-exp-window)
  (add-text-to-exp-window :text "choose")
  
  (proc-display))


(defun present-response ()
  (clear-exp-window)
  
  (add-text-to-exp-window :text (if (< (random 1.0) .9) "heads" "tails")
                          :x 50 :y 100)
  
  (proc-display)
  
  (if (zerop (decf *trials-left*))
      (schedule-event-relative 1.0 #'(lambda ()))
      (schedule-event-relative 1.0 #'present-choice))
  )


(defun do-experiment ()
  (let ((window (open-exp-window "Choice Experiment" :visible nil)))
    (install-device window)
    (setf *trials-left* 48)
    (setf *responses* nil)
    (present-choice)
    (run 100)
    (reverse *responses*)))
 
(defun collect-data (n)
  (let (data)
    (dotimes (i n)
      (when *actr-enabled-p*  (reset))
      (push (do-experiment) data))
    (print-results (analyze data))))

(defun analyze (data)
  (let ((n (length data))
        (result nil))
    (dotimes (i 4)
      (let ((count 0))
        (dotimes (j 12)
          (incf count (apply #'+ (mapcar (lambda (x)
                                           (nth (+ j (* i 12)) x))
                                   data))))
        (push (/ count (* n 12)) result)))
    (reverse result)))
 
(defun print-results (results)
  (correlation results *unit8-choice-data*)
  (mean-deviation results *unit8-choice-data*)
  (format t " Original     Current~%")
  (dotimes (i 4)
    (format t "~8,3F    ~8,3F~%" (nth i *unit8-choice-data*) (nth i results))))

(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (push (if (string-equal (string-upcase (string key)) "H") 1 0) *responses*)
  (present-response))

(clear-all)

(define-model choice

(sgp :v nil
     :esc t 

     ;; Change these  :egs 6.5 :pl t 

     :ul t
     :egs 0.7

     ;; Don't need this :ut -100
)
 
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
 
;; Don't need to set the default successes
;(spp :successes 10000000)
 
;; Don't use priors as the parameters to fit 
;(spp heads :successes 1 :failures 0)
;(spp tails :successes 1 :failures 0)

;; Don't just set these flags
;(spp match :success t)
;(spp mismatch :failure t)

;; Instead, the amount of reward is now the
;; parameters to adjust.

(spp match :reward 2)
(spp mismatch :reward 0)
)