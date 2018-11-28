;; install wanted packages.
;; TODO: why error has occurred...
;; (let ((wanted '(exec-path-from-shell)))
;;   (dolist (package wanted)
;;     (unless (require package nil t)
;;       (package-install package))))
;; (unless (locate-library "exec-path-from-shell")
;;   (package-install 'exec-path-from-shell))

;;; korean key input.
(setq default-input-method "korean-hangul")
(define-key global-map (kbd "S-SPC")  'toggle-input-method)

;;; HOME, END.
(define-key global-map (kbd "<home>") 'beginning-of-line)
(define-key global-map (kbd "<end>")  'end-of-line)

;;; use Option key for Meta on terminal
(unless window-system
 (setq mac-option-modifier 'meta))

;;; .
(when window-system
 ;; (set-frame-font "monaco-15")
 (add-to-list 'default-frame-alist
              ;; '(font . "DejaVu Sans Mono-15")))
              '(font . "monaco-15")))

;; get env from .bash_profile
;; https://github.com/purcell/exec-path-from-shell
;; (when (locate-library "exec-path-from-shell")
(when (load "~/.emacs_robin/exec-path-from-shell.el")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; for LANG setting
(set-locale-environment "ko_KR.UTF-8")

;;; mdfind is spotlight command
(setq locate-command "mdfind")

;;;; shell on macosx
;; https://www.emacswiki.org/emacs/MacOSTweaks#toc22
(defun mac-launch-terminal ()
   (interactive)
   (let ((dir ""))
     (cond
      ((and (local-variable-p 'dired-directory) dired-directory)
       (setq dir dired-directory))
      ((stringp (buffer-file-name))
       (setq dir (file-name-directory (buffer-file-name))))
      ((stringp default-directory)
       (setq dir default-directory))
      )
     (do-applescript
      (format "
 tell application \"Terminal\"
   activate
   try
     do script with command \"cd %s\"
   on error
     beep
   end try
 end tell" dir))
     ))

;; launch finder
(defun mac-launch-finder ()
  (interactive)
  (start-process "finder" nil "open" "."))

;;;; shortcut
(global-set-key [(f5)] 'mac-launch-terminal)
(global-set-key [(M-f5)] 'mac-launch-finder)
