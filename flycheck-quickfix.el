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

(defun flycheck-quickfix--file-find ()
  (expand-file-name flycheck-quickfix-filename (flycheck-quickfix--get-working-directory nil)))

(defun flycheck-quickfix--parse-line (checker line)
  (-when-let ((_ filename line msg) (s-match flycheck-quickfix--regex line))
    (flycheck-error-new-at (string-to-number line) nil 'error msg
                           :filename filename
                           :checker checker)))

(defun flycheck-quickfix--parse-file (checker file)
  (delq nil (mapcar (lambda (line) (flycheck-quickfix--parse-line checker line)) (s-lines file))))

(defun flycheck-quickfix--check (checker callback file)
  (if (file-exists-p file)
      (let ((errors (flycheck-quickfix--parse-file checker (flycheck-quickfix--read-file file))))
        (funcall callback 'finished errors))
    (funcall callback 'suspicious (format "No quickfix file found at: %s" file))))

(defun flycheck-quickfix--start (checker callback)
  (flycheck-quickfix--check checker callback (flycheck-quickfix--file-find)))

(defun flycheck-quickfix--verify (name)
  (let ((quickfix-file (flycheck-quickfix--file-find)))
    (list
     (if (file-exists-p quickfix-file)
         (flycheck-verification-result-new
          :label "Quickfix"
          :message (message "Quickfix file exists at: %s" quickfix-file)
          :face 'success)
       (flycheck-verification-result-new
        :label "Quickfix"
        :message "Quickfix file doesn't exist"
        :face '(bold warning))))))

(flycheck-define-generic-checker 'quickfix-file
  "A Flycheck Checker for parsing errors from Quickfix files"
  :start             #'flycheck-quickfix--start
  :verify            #'flycheck-quickfix--verify
  :working-directory #'flycheck-quickfix--get-working-directory
  :modes             '(scala-mode))

(defun flycheck-quickfix-setup ()
  (interactive)
  (add-to-list 'flycheck-checkers 'quickfix-file))

(provide 'flycheck-quickfix)
;;; flycheck-quickfix.el ends here
