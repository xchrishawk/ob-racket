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

(defun org-babel-expand-body:racket (body params)
  "Expands the body of a Racket code block."
  ;; Currently we don't do any expansion for tangled blocks. Just return
  ;; body unmodified as specified by the user.
  body)

(defun org-babel-execute:racket (body params)
  "Executes a Racket code block."
  ;; Round up the stuff we need
  (let* ((parsed-params (ob-racket--parse-params params))
	 (expanded-body (org-babel-expand-body:racket body params))
	 (result-type (nth 0 parsed-params))
	 (lang (nth 1 parsed-params))
	 (vars (nth 2 parsed-params))
	 (temp-file (make-temp-file "ob-racket-")))
    ;; Build script in temporary file
    (with-temp-file temp-file
      (cond
       ;; Results type is "value" - run in let form
       ((equal result-type 'value)
	(let ((vars-string
	      (mapconcat (lambda (var) (format "[%s (quote %s)]" (car var) (cdr var))) vars " ")))
	  (insert (format "#lang %s\n(let (%s)\n%s)"
			  lang
			  vars-string
			  expanded-body))))
       ;; Results type is "output" - run as script
       ((equal result-type 'output)
	(let ((vars-string
	       (mapconcat (lambda (var) (format "(define %s (quote %s))" (car var) (cdr var))) vars "\n")))
	  (insert (format "#lang %s\n%s\n%s"
			  lang
			  vars-string
			  body))))
       ;; Unknown result type??
       (t (error "Invalid result type: %s" result-type))))
    ;; Run script with Racket interpreter, delete temp file, and return output
    (with-temp-buffer
      (prog2
	  (call-process org-babel-command:racket nil (current-buffer) nil temp-file)
	  (buffer-string)
	(delete-file temp-file)))))

(defun org-babel-prep-session:racket (session params)
  (error "Racket does not currently support sessions."))

;; -- Parameter Parsing --

(defun ob-racket--parse-params (params)
  "Processes and parses parameters for an Org Babel code block. The results are
returned as a list."
  (let ((processed-params (org-babel-process-params params))
	(result-type nil)
	(lang nil)
	(vars nil))
    (dolist (processed-param processed-params)
      (let ((key (car processed-param)) (value (cdr processed-param)))
	(cond
	 ((equal key :result-type) (setq result-type value))
	 ((equal key :lang) (setq lang value))
	 ((equal key :var) (push value vars)))))
    (list result-type lang vars)))
