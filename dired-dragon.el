;;; dired-dragon.el --- add dragon bindings to dired so that drag and drop becomes a thing  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jeetaditya Chatterjee

;; Author: Jeetaditya Chatterjee <jeetelongname@gmail.com>
;; Keywords: convenience, files, extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; provies dragon bindings so that you can select files and drag them into whatever you want. here be dragons

;;; Code:


(defgroup dired-dragon ()
  "Dired dragon customise group."
  :group 'convenience)

(defvar dired-dragon-location (executable-find "dragon"))

(defun dired-dragon--make-string (l)
  "Make a string from the selected files. takes `L' as an argument."
  (message l  "this is a test")
  (if (> (length l) 1)
      (mapconcat 'identity l " ") (car l)))

;;;###autoload
(defun dired-dragon ()
  "The Default."
  (interactive)
  (message "hell")
  (start-process-shell-command
   "dragon" "*dragon*"
   (concat dired-dragon-location (dired-dragon--make-string  (list (dired-get-marked-files))))))

;; (defun dired-dragon-stay ())

;; (defun dired-dragon-individual ())

;; (defun dired-dragon-take-in ())
(provide 'dired-dragon)
;;; dired-dragon.el ends here

;; (defun dired-do-delete (&optional arg)
;;   "Delete all marked (or next ARG) files.
;; `dired-recursive-deletes' controls whether deletion of
;; non-empty directories is allowed."
;;   ;; This is more consistent with the file marking feature than
;;   ;; dired-do-flagged-delete.
;;   (interactive "P")
;;   (dired-internal-do-deletions
;;    (nreverse
;;     ;; this may move point if ARG is an integer
;;     (dired-map-over-marks (cons (dired-get-filename) (point))
;; 			  arg))
;;    arg t))
