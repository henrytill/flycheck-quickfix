;;; flycheck-quickfix.el --- Flycheck Checker for parsing errors from Quickfix files  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Henry Till

;; Author: Henry Till <henrytill@gmail.com>
;; Keywords: tools
;; Pacakge-Requires: ((dash "2.13.0") (flycheck "30") (s "1.11.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:
;;   (require 'flycheck-quickfix)
;;   (add-hook 'scala-mode-hook 'flycheck-quickfix-setup)
;;   (add-hook 'scala-mode-hook 'flycheck-mode)

;;; Code:

(require 'dash)
(require 'flycheck)
(require 'rx)
(require 's)

(defvar flycheck-quickfix-modes '(scala-mode))

(defvar-local flycheck-quickfix-filename "errors.err")

(defconst flycheck-quickfix--regex
  (rx bol
      (group (+ (not (any ":"))))
      ":"
      (group (+ num))
      ":"
      (* space)
      (group (+ nonl))))

(defun flycheck-quickfix--read-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun flycheck-quickfix--get-working-directory (checker)
  (locate-dominating-file default-directory flycheck-quickfix-filename))

(defun flycheck-quickfix--file ()
  (let* ((wd (flycheck-quickfix--get-working-directory nil))
         (f  (expand-file-name flycheck-quickfix-filename wd)))
    (and (file-exists-p f) f)))

(defun flycheck-quickfix--parse-line (checker line)
  (-when-let ((_ filename line msg) (s-match flycheck-quickfix--regex line))
    (flycheck-error-new-at (string-to-number line) nil 'error msg
                           :filename filename
                           :checker checker)))

(defun flycheck-quickfix--parse-file (checker file)
  (delq nil (mapcar (lambda (line) (flycheck-quickfix--parse-line checker line)) (s-lines file))))

(defun flycheck-quickfix--start (checker callback)
  (-if-let (file (flycheck-quickfix--file))
      (let ((errors (flycheck-quickfix--parse-file checker (flycheck-quickfix--read-file file))))
        (funcall callback 'finished errors))
    (funcall callback 'suspicious (format "could not find %s" flycheck-quickfix-filename))))

(defun flycheck-quickfix--verify (name)
  (let ((file (flycheck-quickfix--file)))
    (list (flycheck-verification-result-new
           :label   "quickfix file"
           :message (or file "not found")
           :face    (if file 'success 'warning)))))

(defun flycheck-quickfix-setup ()
  (interactive)
  (flycheck-define-generic-checker 'quickfix
    "A Flycheck Checker for parsing errors from Quickfix files"
    :predicate         #'flycheck-quickfix--file
    :working-directory #'flycheck-quickfix--get-working-directory
    :verify            #'flycheck-quickfix--verify
    :start             #'flycheck-quickfix--start
    :modes             flycheck-quickfix-modes)
  (add-to-list 'flycheck-checkers 'quickfix))

(provide 'flycheck-quickfix)
;;; flycheck-quickfix.el ends here
