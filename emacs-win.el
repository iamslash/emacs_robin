;;; emacs-win.el --- emacs script for windows-nt     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  

;; Author:  <iamslash@IAMSLASH-PC>
;; Keywords: 

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

;; 

;;; Code:

(provide 'emacs-win)
;;; emacs-win.el ends here

;;;; shell on win
;; 
(defun win-launch-terminal ()
  "Run cmd on the directory of the current buffer"
  (interactive)
  ;; "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\Common7\Tools\VsDevCmd.bat"
  (shell-command
   (let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe")))
     (set-process-query-on-exit-flag proc nil))))
  
;; launch explore
(defun win-launch-explorer ()
  "Run explorer on the directory of the current buffer"
  (interactive)
  (shell-command
   (concat "start explorer /e,/select,\""
           (replace-regexp-in-string "/" "\\\\"
                                     (buffer-file-name)) "\"")))
;;;; shortcut
(global-set-key [(f5)] 'win-launch-terminal)
(global-set-key [(M-f5)] 'win-launch-explorer)

;;;; markdown-mode
(custom-set-variables
 '(markdown-command "C:\\Program Files (x86)\\Pandoc\\pandoc.exe"))
