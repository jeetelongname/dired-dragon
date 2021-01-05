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

;; 1 Commands
;; ==========

;; 1.1 Dired commands
;; ~~~~~~~~~~~~~~~~~~

;;   you mark the file like normal (using `m') and call one of these
;;   commands (flags are what I use for dragon) it will also work if you
;;   are sort of on the file
;;    Command                    Use                                           flags
;;   ----------------------------------------------------------------------------------
;;    `dired-dragon'             dumps all the files and exits once done       `-x -a'
;;    `dired-dragon-stay'        sticks around but dumps all the files         `-a'
;;    `dired-dragon-individual'  you can individually drag and drop the files

;;   I recommend binding them unless you are a `M-x' wizard
;;   ,----
;;   | (define-key dired-mode-map (kbd "C-d d") #'dired-dragon)
;;   | (define-key dired-mode-map (kbd "C-d s") #'dired-dragon-stay)
;;   | (define-key dired-mode-map (kbd "C-d i") #'dired-dragon-individual)
;;   `----


;; 1.2 In file commands
;; ~~~~~~~~~~~~~~~~~~~~

;;   these commands are the original work of @ymarco. Thanks! They open
;;   up a dired instance for the current file unless its an org or tex
;;   file. then it will check for a coresponding .pdf file and drag that
;;   instead
;;    Commands                            use                                                     flag  requires
;;   ------------------------------------------------------------------------------------------------------------
;;    `:drag'                             an evil ex command to drag the current buffer and quit  `-x'  *evil*
;;    TODO `dired-dragon-current-buffer'  a vanilla version of the `:drag' command                `-x'  Nothing


;; 3 Configuring
;; =============

;; 3.1 Variables
;; ~~~~~~~~~~~~~

;;   if you are on a distro that names dragon differently then you can
;;   change the variable `dired-dragon-location' to the name and or
;;   location of your choice
;;   ,----
;;   | (setq dired-dragon-location (executable-find "YOUR-NAME-HERE"))
;;   | ;; using executable find is a little better as it does not hard code the location
;;   | ;; but this is optional, just provide it with a full path (eg /usr/bin/dragon)
;;   `----
;;; Code:

(defgroup dired-dragon ()
  "Dired dragon customise group."
  :group 'convenience)

(defcustom dired-dragon-location (executable-find "dragon")
  "The location of dragon. may need changing depending on what dragon is called."
  :type 'string
  :group 'dired-dragon)

(defcustom dired-dragon-buffer "*dragon*"
  "Buffer that dired dragon will output too."
  :type 'string
  :group 'dired-dragon)


(defun dired-dragon--strip-parens (s)
  "Strip parens from a string using regex find and replace.
takes argument S. Its a bit crude but it works"
  (replace-regexp-in-string "(" "" (replace-regexp-in-string ")" "" s)))

(defun dired-dragon--core (name &optional flags)
  "This is most of the core logic for dired-dragon. Takes the arg NAME and the optional arg of FLAGS."
  (start-process-shell-command
   name
   dired-dragon-buffer
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

;;; non dired commands
;;;###autoload
(when (require 'evil nil 'noerror)
  (evil-define-command dragon-drag-file (file)
    "Open a drag window with dragon for the file opened in the current buffer.
With FILE, use that file instead. If FILE not specified and the
buffer is org/tex and a corresponding pdf exists, drag that pdf."
    (interactive "<f>")
    (start-process "dragon-current-file" dired-dragon-buffer
                   "dragon"
                   (or file
                       (and (eq major-mode 'dired-mode)
                            (dired-get-filename))
                       (let ((file (file-name-extension (buffer-file-name))))
                         (and (or (eq major-mode 'org-mode)
                                  (eq major-mode 'latex-mode))
                              (file-exists-p file)
                              file))
                       (buffer-file-name))
                   "-x"))
  (evil-ex-define-cmd "drag" #'dragon-drag-file))

;; TODO needs some work to port to vanilla
(defun dired-dragon-current-buffer ()
  "Open dragon for the current file."
  (interactive)
  (start-process "dragon-evil" dired-dragon-buffer
                   "dragon"
                   (or file (and (eq major-mode 'dired-mode) (dired-get-filename))
                       (let ((file (file-name-extension (buffer-file-name))))
                         (and (or (eq major-mode 'org-mode)
                                  (eq major-mode 'latex-mode))
                              (file-exists-p file)
                              file))
                       (buffer-file-name))
                   "-x"))
(provide 'dired-dragon)
;;; dired-dragon.el ends here
