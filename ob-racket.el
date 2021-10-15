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

(defun ob-racket--format-require (requirement)
  "Format require statement for Racket"
  (format "(require %s)" requirement))

(defun ob-racket--format-var (var-name var-value)
  "Format variable assignment statement for racket"
  (format "(define %s (quote %S))" var-name var-value))

(defun ob-racket--expand-named-src-block  (blockname)
  (save-excursion
    (goto-char (org-babel-find-named-block blockname))
    (org-babel-expand-src-block)))

(defcustom org-babel-command:racket "/usr/bin/racket"
  "The path to the Racket interpreter executable."
  :group 'org-babel
  :type 'string)

;; -- Babel Functions --

(defun org-babel-expand-body:racket (body params)
  "Expands the body of a Racket code block."
  (let* ((lines-of-body (split-string body "\n"))
         (first-line (car lines-of-body))
         (output-lines-stack '())
         (print-length nil)) ;; Used by the format function which is invoked inside string formatting
    (cl-flet* ((out (l) (push l output-lines-stack))
               (out-headers ()
                            (when-let* ((requires (assoc 'requires params)))
                              (out (mapconcat 'ob-racket--format-require
                                              (cadr requires)
                                              "\n")))
                            (when-let* ((vars (cadr (assoc 'vars params))))
                              (out (mapconcat (lambda (var) (ob-racket--format-var (car var) (cdr var)))
                                              vars
                                              "\n")))))
      ;; actually not sure this next part is necessary
      (cond ((string-match "#lang" first-line)
             (out first-line)
             (out-headers))
            (t
             (out "#lang racket")
             (out-headers)
             (out first-line)))
      (dolist (l (cdr lines-of-body))
        (out l))
      (string-join (reverse output-lines-stack) "\n"))))


(defun org-babel-execute:racket (body unparsed-params)
  "Executes a Racket code block."
  (let* ((params (ob-racket--parse-params unparsed-params))
         (main-src (org-babel-expand-body:racket body params))
         (temporary-file-directory (file-name-as-directory (make-temp-file "ob-racket-" 'make-directory-only)))
         (main-filename (concat temporary-file-directory (make-temp-name "ob-racket") ".rkt")))
    (dolist (blockname (cadr (assoc 'adjacent-files params)))
      (let ((filename (concat temporary-file-directory blockname))
            (src (ob-racket--expand-named-src-block blockname)))
        (with-temp-file filename
          (insert src))))
    (with-temp-file main-filename
      (insert main-src))
    ;; Run script with Racket interpreter, delete temp file, and return output
    (with-temp-buffer
      (prog2
          (let ((default-directory temporary-file-directory))
            (call-process org-babel-command:racket nil (current-buffer) nil main-filename))
          (buffer-string)
        (delete-directory temporary-file-directory 'recursive)))))

(defun org-babel-prep-session:racket (session params)
  (error "Racket does not currently support sessions."))

;; -- Parameter Parsing --

(defun ob-racket--parse-params (params)
  "Processes and parses parameters for an Org Babel code block. The results are
returned as a list."
  (let ((processed-params (org-babel-process-params params))
        (result-type nil)
        (requires nil)
        (adjacent-files nil)
        (vars nil))
    (dolist (processed-param processed-params)
      (let ((key (car processed-param))
            (value (cdr processed-param)))
        (cond
         ((equal key :result-type) (setq result-type value))
         ((equal key :var) (push value vars))
         ((equal key :adjacent-file) (setq adjacent-files (split-string value "\s+")))
         ((equal key :require) (push value requires)))))
    `((result-type ,result-type)
      (vars ,vars)
      (adjacent-files ,adjacent-files)
      (requires ,requires))))
