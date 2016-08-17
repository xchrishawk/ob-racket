;; ob-racket.el
;; Chris Vig (chris@invictus.so)

;; -- Provide --

(provide 'ob-racket)

;; -- Requires --

(require 'ob)
(require 'ob-eval)

;; -- Babel Functions --

(defun org-babel-prep-session:racket (session params)
  "Sessions are not currently supported for Racket, so this function will always
signal an error."
  (error "Sessions are not currently supported for Racket."))

(defun org-babel-execute:racket (body params)
  (let* ((processed-params (org-babel-process-params params))
	 (expanded-body (org-babel-expand-body:racket body params processed-params)))
    (message "called org-babel-execute:racket!")))

(defun org-babel-expand-body:racket (body params &optional processed-params)
  (message "called org-babel-expand-body:racket!"))
