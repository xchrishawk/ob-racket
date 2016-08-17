;; ob-racket.el
;; Chris Vig (chris@invictus.so)

;; -- Provide --

(provide 'ob-racket)

;; -- Requires --

(require 'ob)

;; -- Babel Functions --

(defun org-babel-prep-session:racket (session params)
  "Sessions are not currently supported for Racket, so this function will always
signal an error."
  (error "Sessions are not currently supported for Racket."))

(defun org-babel-execute:racket (body params)
  "Executes the Racket code block contained in BODY using the parameters in
PARAMS, and returns the result."
  (let* ((processed-params (org-babel-process-params params))
	 (temp-file (org-babel-expand-body:racket body processed-params)))
    (with-temp-buffer
      (prog2
	  (call-process "racket" nil (current-buffer) nil temp-file)
	  (buffer-substring (point-min) (point-max))
	(delete-file temp-file)))))

(defun org-babel-expand-body:racket (body processed-params)
  "Expands BODY into a complete Racket script using the parameters in
PROCESSED-PARAMS. The following keywords are currently used for expansion:

:lang - sets the languaged specified by the #lang directive
:var - sets a variable (defined using a (define ...) form)"
  (let ((output-file (make-temp-file "ob-racket")))
    (with-temp-file output-file
      (let ((lang "racket") (vars nil))
	;; Parse the processed parameters
	(dolist (param processed-params)
	  (cond
	   ((equal (car param) :lang) (setq lang (cdr param)))
	   ((equal (car param) :var) (push (cdr param) vars))))
	;; Insert #lang directive
	(insert (format "#lang %s\n" lang))
	;; Insert defines for each variable
	(when (not (null vars))
	  (insert "\n;; Header Variables\n")
	  (dolist (var (reverse vars))
	    (insert (format "(define %s %s)\n" (car var) (cdr var)))))
	;; Insert the remaining body
	(insert "\n;; Body\n" body)))
    ;; Return the name of the temporary output file
    output-file))
