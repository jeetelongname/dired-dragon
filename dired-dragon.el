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

(defvar dired-dragon-location (executable-find "dragon")
  "The location of dragon. may need changing depending on what dragon is called.")

(defvar dired-dragon--buffer "*dragon*")


(defun dired-dragon--strip-parens (s)
  "Strip parens from a string using regex find and replace.
takes argument S. Its a bit crude but it works"
  (replace-regexp-in-string "(" "" (replace-regexp-in-string ")" "" s)))

(defun dired-dragon--core (name &optional flags)
  "This is most of the core logic for dired-dragon. Takes the arg NAME and the optional arg of FLAGS."
  (start-process-shell-command
   name
   dired-dragon--buffer
   (concat dired-dragon-location
          (dired-dragon--strip-parens
           (format " %s"  (dired-get-marked-files))) flags)))

;;;###autoload
(defun dired-dragon ()
  "The Default. will drag all items selected and exit once done.
its my biggest uscase"
  (interactive)
  (dired-dragon--core "dragon" " -x -a"))

;;;###autoload
(defun dired-dragon-stay ()
  "Drag multiple files to the same source but don't exit after the first drop."
  (interactive)
  (dired-dragon--core "dragon-stay" " -a"))

;;;###autoload
(defun dired-dragon-individual ()
  "Mark multiple files and drag them individually."
  (interactive)
  (dired-dragon--core "dragon-individual"))

(provide 'dired-dragon)
;;; dired-dragon.el ends here
