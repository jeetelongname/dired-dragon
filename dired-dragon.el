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

(defun dired-dragon--core ()
  "This is most of the core logic for dired-dragon."
  (concat dired-dragon-location
          (dired-dragon--strip-parens
           (format " %s"  (dired-get-marked-files)))))

(defun dired-dragon--strip-parens (s)
  "Strip parens from a string using regex find and replace.
takes argument S. Its a bit crude but it works"
  (replace-regexp-in-string "(" "" (replace-regexp-in-string ")" "" s)))

(defun dired-dragon ()
  "The Default. will drag all items selected and exit once done.
its my biggest uscase"
  (interactive)
  (start-process-shell-command
   "dragon" "*dragon*" (concat (dired-dragon--core) " -x -a")))

;; TODO
(defun dired-dragon-stay ()
  "If you have a lot of dragging and dropping to do.
it will stick around but will still drop all of them"
  (start-process-shell-command
   "dragon-stay" "*dragon*" (concat (dired-dragon--core) " -a")))
;; TODO
;; (defun dired-dragon-individual ())
;; TODO
;; (defun dired-dragon-take-in ())

(provide 'dired-dragon)
;;; dired-dragon.el ends here
