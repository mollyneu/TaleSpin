(defun get-facts-hash-table ()
;;make-hash-table is a built in function-of lisp, empty hash table
  (let ((facts-hash-table (make-hash-table)))
    ;; loop over the global *facts* list
    (dolist (fact *facts*)
      (let ((attribute (first fact))
	    (object (second fact))
	    (value (third fact)))
	;; lookup  object in the facts-hash-table
	(let ((hash-element (gethash object facts-hash-table)))
	  (if hash-element
	      ;; if there's already a list in the facts-hash-table for
	      ;; object then cons on the (attribute value) onto the
	      ;; list
	      (progn
		(setf (gethash object facts-hash-table) (cons `(,attribute ,value) hash-element))
		;; (format t "~A=~A~%" object (gethash object facts-hash-table))
		)
	      ;; if there isn't a list in the facts-hash-table for
	      ;; object then add a list that contains (attribute value)
	      (progn
		(setf (gethash object facts-hash-table) `((,attribute ,value)))
		;; (format t "~A=~A~%" object (gethash object facts-hash-table))
		))))
      *facts*)
    ;; return facts-hash-table
    facts-hash-table))


(defun test-get-facts-hash-table (object)
  (let ((fht (get-facts-hash-table)))
    (format t "~A" (gethash object fht))))


(defun list-of-attribute-values-to-name (lst)
  (map 'list (lambda (x) (format nil "~A" (second x))) lst))


(defun get-new-object-names-hash-table ()
  (let ((fht (get-facts-hash-table)))
    (maphash
     (lambda (k v)
       ;; (format t "~A / ~A~%" k (list-of-attribute-values-to-name v))
       (setf (gethash k fht) (list-of-attribute-values-to-name v)))
     (get-facts-hash-table))
    fht))

