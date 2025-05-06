;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;
;;;;			    File: tspin-sys-def.lisp
;;;;
;;;;                 Loader file and system definition.
;;;
;;; Tale-Spin is a modification of M. Pazzani's version of J. Meehan's 
;;; Tale-Spin story generator.
;;; 



;;;
;;; Miscellaneous general support functions.
;;; 
(defpackage Miscellaneous
  (:nicknames Misc)
  )

(defpackage FrameSystem
  (:nicknames Frames)
  (:use Misc)
  )



(defpackage Tale-Spin
  (:nicknames TSpin)
  (:use 
   Misc Frames)
  )




(defparameter *tspin-files*
    '("break-facility"
      "utils"
      "tspin"
      "extensions"
      "spin-cd-reps"
      "data"
      "patch"
      "mumble" 
      "verbs"
      "alt-story"
      )
  )


(defparameter *tspin-file-dir*
  (directory-namestring (merge-pathnames  "TaleSpin/" (user-homedir-pathname)))
  "The path to the directory where the TaleSpin system resides.")


(defun load-sys (&optional (file-names *tspin-files*)
                           (directory *tspin-file-dir*))
  (cond ((null file-names) nil)
        (t
         (load (concatenate
		'string
		directory
		(first file-names)))
         (load-sys (rest file-names)
                   directory)))
  )


(defun compile-sys (&optional (file-names *tspin-files*)
                              (directory *tspin-file-dir*))
  (cond ((null file-names) nil)
        (t
         (compile-file (concatenate
                        'string
                        directory
                        (first file-names)))
         (compile-sys (rest file-names) directory)))
  )



(defun compile-and-load-sys (&optional (file-names *tspin-files*)
                                       (directory *tspin-file-dir*))
  (cond ((null file-names) nil)
        (t
         (compile-file (concatenate
                        'string
                        directory
                        (first file-names)))
         (load (concatenate
		'string
		directory
		(first file-names)))
         (compile-and-load-sys (rest file-names) directory)))
  )





;;; If using a Symbolics platform, uncomment this code. 

;(defsystem TALE-SPIN-SYSTEM
;    
;    (:pretty-name "Tale-Spin Story Generation System"
;     :short-name "TSpinSystem"
;     :default-pathname "AZURE:/users/c/cox/Lispm/meta-aqua/tale-spin/"
;     :patchable T)
;    
;  ;; The next two modules constitute the Tale-Spin package.
;  
;  (:module t-imports ("spin-interface.lisp")
;;;; 	   (:in-order-to :load (:load micro-aqua))
;;;; 	   (:in-order-to :compile (:load micro-aqua))
;	   )
;
;  (:module tale-spin ("tspin.lisp"
;		      "extensions.lisp"
;		      "spin-cd-reps.lisp"
;		      "data.lisp"
;		      "patch.lisp"
;		      "mumble.lisp" 
;		      "verbs.lisp"
;		      "alt-story.lisp"
;	   (:in-order-to :load (:load t-imports))
;	   (:in-order-to :compile (:load t-imports)))
;
;
;  )

