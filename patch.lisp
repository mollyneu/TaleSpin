;;; -*- Mode: LISP; Syntax: Common-lisp; Package: TALE-SPIN; Base: 10 -*-

;;; (in-package "TSPIN")


(defvar *real-goals* '(thirsty))


;;; The main program function. Most of the meat to the program 
;;; is in the assert-fact function. [cox]
;;;
;;; Alternative parameters were added as follows:
;;; alt? t -> use different background knowledge.
;;; person-num  - to automatically choose a person without choosing from menu in pick-one
;;; problem-num - to automatically choose a problem without choosing from menu in pick-one
;;; test-cd - cd to seed the story with rather than problem.
;;; pick-yarn? t -> present a list of yarn types that the user can specify to spin.
;;;           num -> use that number rather than choose it interactively.
;;;
(defun spin (&optional alt? person-num problem-num test-cd pick-yarn?)
  (do-break spin)
;  (setq *goals* *real-goals*) ;Commented this out. [cox]
  (setf *ALL* nil)				;Start a new record each story. [cox]
  (setq *story-cds* nil)
  (setq say-stream nil)
  (terpri *tspin-stream*)
  (terpri *tspin-stream*)
  ;; The following  s-expressions is to control spinning of stories. [cox 16jan95]
  (if pick-yarn?
      (multiple-value-setq
	(person-num problem-num)
	(specify-yarns person-num problem-num pick-yarn?)))
  (let* ((main-character-index
	   (or person-num			;[cox 18aug93]
	       (pick-one 
                 (mapcar #'(lambda (p) 
                             (list (string-capitalize (string p))
                                   (string-capitalize (string (get p 'is-a)))))
                         (cons 'officer-roberts	;So can test his behavior directly [cox 17aug93]
			       *personae*
			       )
			 ))))
         (main-character (nth main-character-index 
			      (cons 'officer-roberts	;[cox 17aug93]
				    *personae*)))
         (g (get main-character 'gender)))
    (setq *main-char* main-character)
    ;;|||||| Should set this back to the way it was when I received it from Pazzani.
;    (when (member *main-char* '(karen lynn))
;      (push 'thirsty *goals*)
;      (push 'bored *goals*))
;    (when (eq *main-char* 'lynn)
;      (push 'bored *goals*))
    (when
      (or (eq *main-char* 'dad)			; Dad now smokes cigarettes. [cox 23feb95]
	  (eq *main-char* 'lynn)		; So does lynn. [cox 16mar95]
	  (eq *main-char* 'elvis))
      (push 'jonesing *goals*))			;[cox]
;;;     (setf *goals* (append *goals* (list 'sad)))
    (when (eq *main-char* 'officer-roberts)
      (setf  *goals* '(concerned)))		;[cox 17aug93 added concerned & 5jun95 limited to concerned]
    (if 
      (and (not (numberp problem-num))		;[cox 18aug93]
	   (not *auto?*))
      (format *tspin-stream* "~%What is ~:(~A~)'s problem??~%" (remove-dashes main-character)))
    (let* ((*him* (if (eq g 'male) main-character nil))
           (*her* (if (eq g 'female) main-character nil))
           (problem-index
	     (or problem-num
		 (pick-one 
                   (mapcar #'(lambda (g)
                               (list 
				 (remove #\Newline
					 (with-output-to-string
					   (*tspin-stream*)
;;;  					     (say-stream)
					   (say (state main-character
						       g 'pos) nil)))))
                           *goals*))))
           (problem (nth problem-index *goals*)))
      ;;[cox 29aug93]
      (print-story-params alt? 
			  main-character-index 
			  problem-index)
      ;; Moved the following here so that print-story-params precede the call of init-world because Mark D.
      ;; put random events in init-world.  Without the move, then, the saved random seed does not recreate
      ;; the story correctly. [cox 10jan95 & 12feb95]
      (if alt?
	  (init-world *new-facts*
		      '*new-facts*
		      *new-objects* 
		      *new-everyone-loc-facts* 
		      *new-world-facts*)
	  (init-world))
      (if (not *auto?*)
	  (format *tspin-stream* "~2%This is a story about ...~%"))
;     (setq *goals* *real-goals*)
      (setq say-stream *terminal-io*)
      (setq *story-cds* (list (time-marker)))
      (do-break spin "Problem: ~s~%Main-Character: ~s~%" problem (remove-dashes main-character))
      (format? *tspin-stream* "~%~%One day ...~a was ~a.~%"
	       (remove-dashes (string main-character)) problem)
      (init-gen)
      (if test-cd
	  (assert-fact test-cd)
	  (assert-fact (mloc 'world (state main-character problem 'pos))))
      (format? *tspin-stream* "~2%~30t--- The End ---~%")
      (new-time)
      (init-yarn-vars)				; [cox feb95]
      (values))))



;;; [cox 1aug93]
;;; The global variable *all* contains all of the cds that represent the story 
;;; Tale-Spin produces. It is a list containing a sublist of cds for each time 
;;; interval during the story. A time interval represents a discrete time segment
;;; of the story during which all acts and state changes (represented by the cds 
;;; in the sublist) are assumed to have occured simultaneously. 
;;;
;;; The list is created by the function process-time below called by function 
;;; new-time.
(defvar *all* nil)


;;; Commented out the first call because of changes to function updated-story in 
;;; file mumble because it now uses tale-spin's original cd representations rather  
;;; than those of OCCAM. [cox 23 jul 93]
;;;
(defun process-time(x &aux cds new)
;  (setq x (mapcar #'cdr x)) ;Was Pazzani's code.
  (push x *all*))


;;; This function is flawed. Also changed default for parameter s
;;; to *terminal-io* from terminal-io. [cox]
;;;
;;; ||||||Where are functions list->cd and cd->list ? [cox 28feb95]
;;; 
(defun print-all( &optional (s *terminal-io*))
  (format s "(setq *all* (append *all* (list ")
  (mapc #'(lambda(x)
	    (format s "~%(list")
	    (mapc #'(lambda(x)
		      (format s " (list->cd '~a)"
			      (cd->list x)))
		  x)
	    (format s ")~%"))
	*all*)
  (format s ")))~%"))


