;; ob-racket.el
;; Chris Vig (chris@invictus.so)

;; -- Provide --

(provide 'ob-racket)

;; -- Requires --

(require 'ob)

;; -- Variables --

(add-to-list 'org-babel-tangle-lang-exts '("racket" . "rkt"))

(defvar org-babel-default-header-args:racket '((:lang . "racket"))
  "A list of default header args for Racket code blocks.")

(defvar org-babel-command:racket "/usr/bin/racket"
  "The path to the Racket interpreter executable.")

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
	  (call-process org-babel-command:racket
			nil
			(current-buffer)
			nil
			temp-file)
	  (buffer-substring (point-min) (point-max))
	(delete-file temp-file)))))

(defun org-babel-expand-body:racket (body processed-params)
  "Expands BODY into a complete Racket script using the parameters in
PROCESSED-PARAMS. The script is stored in a temporary file, and the name of the
temporary file is returned. The following header arguments are currently used
for script expansion:

:results - sets the result type - should be either \"value\" or \"output\"
:lang - sets the languaged specified by the #lang directive
:func - sets the name of the function when using the \"value\" result type
:var - sets a variable (defined using a (define ...) form)"
  (let ((output-file (make-temp-file "ob-racket")))
    (with-temp-file output-file
      (let ((result-type nil)
	    (lang nil)
	    (func nil)
	    (vars nil))
	;; Loop through list of processed parameters and update arguments
	(dolist (param processed-params)
	  (let ((key (car param)) (value (cdr param)))
	    (cond
	     ((equal key :result-type) (setq result-type value))
	     ((equal key :lang) (setq lang value))
	     ((equal key :func) (setq func value))
	     ((equal key :var) (push value vars)))))
	;; Build script of appropriate type
	(apply
	 (cond
	  ((string-equal result-type "value") 'org-babel-expand-body:racket/value)
	  ((string-equal result-type "output") 'org-babel-expand-body:racket/output)
	  (t (user-error "Unknown result type: %s" result-type)))
	 (list lang func (reverse vars) body))))
    ;; Return the name of the temporary output file
    output-file))

(defun org-babel-expand-body:racket/value (lang func vars body)
  "Expands a Racket code block in the \"value\" format. This format executes the
code block as a function, and returns the output of the function. All variables
will be passed as arguments."
  (let ((function-name (or func "func"))
	(argument-list (mapconcat (lambda (var) (format "%s" (car var))) vars " "))
	(argument-values (mapconcat (lambda (var) (format "%s" (cdr var))) vars " "))
	(space-if-args (if (null vars) "" " ")))
    (insert
     (format "#lang %s\n(define (%s%s%s)\n%s)\n(%s%s%s)\n"
	     lang
	     function-name
	     space-if-args
	     argument-list
	     body
	     function-name
	     space-if-args
	     argument-values))))

(defun org-babel-expand-body:racket/output (lang func vars body)
  "Expands a Racket code block in the \"output\" format. This format executes the
code block as a complete script, and returns the script's entire output from
stdout. All variables will be defined prior to executing the body."
  (let ((define-list
	  (mapconcat (lambda (var) (format "(define %s %s)" (car var) (cdr var))) vars "\n")))
    (insert
     (format "#lang %s\n%s\n%s" lang define-list body))))
