;;; ob-racket.el --- Org supprt for racket

;; Copyright (C) 2010-2020 Chris Vig

;; Author: Chris Vig (chris@invictus.so)
;; Maintainer: Chris Vig (chris@invictus.so)
;; Keywords: languages org babel racket
;; Homepage: https://github.com/xchrishawk/ob-racket

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; org-babel functions for racket evaluation
;;

(provide 'ob-racket)

;; -- Requires --

(require 'ob)

;; -- Variables --

(add-to-list 'org-babel-tangle-lang-exts '("racket" . "rkt"))

(defvar org-babel-default-header-args:racket '((:lang . "racket"))
  "A list of default header args for Racket code blocks.")

(defcustom org-babel-command:racket "/usr/bin/racket"
  "The path to the Racket interpreter executable."
  :group 'org-babel
  :type 'string)

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
         (result-type (cadr (assoc 'result-type parsed-params)))
         (temp-file (make-temp-file "ob-racket-"))
         (requires-string (mapconcat (lambda (r) (format "(require %s)" r))
                                     (cadr (assoc 'requires parsed-params))
                                     "\n")))
    (with-temp-file temp-file
      (cond
       ;; Results type is "value" - run in let form
       ((equal result-type 'value)
        (let ((vars-string (mapconcat (lambda (var) (format "[%s (quote %s)]" (car var) (cdr var)))
                                      (cadr (assoc 'vars parsed-params))
                                      " ")))
          (insert (format "#lang %s \n%s \n(let (%s) \n%s)"
                          (cadr (assoc 'racket-lang parsed-params))
                          requires-string
                          vars-string
                          expanded-body))))
       ;; Results type is "output" - run as script
       ((equal result-type 'output)
        (let* ((vars-string (mapconcat (lambda (var) (format "(define %s (quote %s))" (car var) (cdr var)))
                                       (cadr (assoc 'vars parsed-params))
                                       "\n"))
               (output (format "#lang %s \n%s \n%s \n%s"
                               (cadr (assoc 'racket-lang parsed-params))
                               requires-string
                               vars-string
                               expanded-body)))
          (insert output)))
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
        (requires nil)
        (vars nil)
        (racket-lang "racket"))
    (dolist (processed-param processed-params)
      (let ((key (car processed-param))
            (value (cdr processed-param)))
        (cond
         ((equal key :result-type) (setq result-type value))
         ((equal key :lang) (setq lang value))
         ((equal key :var) (push value vars))
         ((equal key :require) (push value requires))
         ((equal key :racket-lang) (setq racket-lang value)))))
    `((lang ,lang)
      (result-type ,result-type)
      (vars ,vars)
      (requires ,requires)
      (racket-lang ,racket-lang))))
