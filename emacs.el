;; -*- coding: utf-8 -*-
;; -*- mode: Emacs-Lisp; outline-regexp: "^;;;; .*";  -*-

;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Global
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;; defconst for platform, machine 
(defconst win32p (eq system-type 'windows-nt) "true if window")
(defconst linuxp (eq system-type (or 'gnu/linux 'berkeley-unix)) "true if linux")
(defconst macosxp (eq system-type 'darwin))
(defconst homep (string-equal system-name "SALSHHOMEONE") "true if home")
(defconst officep (string-equal system-name "DO-PC") "true if office")

;;;;;;;;  
(global-font-lock-mode 1)               ; syntanx highlight
(transient-mark-mode 1)                 ; marking highlight
(show-paren-mode t)                     ; 
(setq ring-bell-function 'ignore)       ; turn off alarms completely
(line-number-mode 1)                    ; 
(column-number-mode 1)                  ; 
(setq scroll-step 1)                    ; like windows
(setq scroll-conservatively 4096)    
(delete-selection-mode 1)               ; like windows
(setq-default truncate-lines t)         ; see also toggle-truncate-lines 
(dynamic-completion-mode)               ; 
;; Set the text for titlebar and icons, %f=filename, %b=buffername
(setq frame-title-format (list "GNU Emacs " emacs-version "@" system-name " - " '(buffer-file-name "%f" "%b")))
(setq icon-title-format frame-title-format)
(which-function-mode 1)                 ; it is better than semantic-stickyfunc-mode
(if (functionp 'tool-bar-mode) (tool-bar-mode 0))
(if (functionp 'scroll-bar-mode) (scroll-bar-mode 0))
(auto-compression-mode 1)               ; 
(setq-default indent-tabs-mode nil)     ; 
;; https://www.emacswiki.org/emacs/IncrementalSearch
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)
;;;; windows key
(when win32p
  (setq w32-pass-lwindow-to-system nil
        w32-pass-rwindow-to-system nil
        w32-pass-apps-to-system    nil
        w32-lwindow-modifier       'super   ;; Left Windows
        w32-rwindow-modifier       'super   ;; Rigth Windows
        w32-apps-modifier          'hyper) ;; App-Menu (right to Right Windows)
  (global-set-key [(super g)] 'goto-line))

(global-set-key (kbd "C-x C-b") 'buffer-menu) ; buffer switching
;; (when macosxp
;;   (global-set-key (kbd "S-SPC") 'toggle-korean-input-method))

;;;;;;;; Short Cut 
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-_") 'text-scale-decrease)
(global-set-key [(f7)] 'compile)   ; visual studio 
;; (global-set-key [(f5)] 'gdb)       ; visual studio 
(global-set-key [(f4)] 'next-error)   ; useful for M-x grep 
(global-set-key [(shift f4)] 'previous-error)


;;;;;;;; CODEC 
(when (or (and enable-multibyte-characters macosxp)
          (and enable-multibyte-characters win32p))

  (set-language-environment "Korean")
  (setq-default coding-system 'utf-8)
  (setq file-coding-system 'utf-8)
  (setq display-coding-system 'utf-8)
  (setq-default buffer-coding-system 'utf-8)
  (setq sendmail-coding-system 'utf-8)
  (setq keyboard-coding-system 'utf-8)
  (setq terminal-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
   
  ;; (setq-default file-name-coding-system 'euc-kr)
  (setq input-method-verbose-flag nil
        input-method-highlight-flag nil)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  ;; Hangul Mail setting
  (setq-default sendmail-coding-system 'utf-8))

;;;;;;;; Font
(when win32p
  (set-face-font 'default "-outline-Courier New-normal-normal-normal-mono-16-*-*-*-c-*-iso8859-1")
  ;;   (set-face-font 'default "fixed")  
  )

;;;;;;;; Auto Mode Alist
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)) 
(add-to-list 'auto-mode-alist '("\\.asm\\'" . asm-mode))
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("Makefile\\.[[:alpha:]]*" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\TODO\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\README\\'" . org-mode))

;;;;;;;; Open a file in emacs from eshell
;; From http://www.emacswiki.org/cgi-bin/wiki.pl?EshellFunctions
(defun eshell/emacs (&rest args)
  "Open a file in emacs. Some habits die hard."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else weird stuff happens
    ;; when you try to open a bunch of different files in wildly
    ;; different places in the filesystem.
    (mapc #'find-file (mapcar #'expand-file-name args))))

;;;;;;;; For querying before overwriting or removing files add to your .emacs:
;; https://www.emacswiki.org/emacs/EshellInteractivelyRemove
(setq eshell-cp-interactive-query t
      eshell-ln-interactive-query t
      eshell-mv-interactive-query t
      eshell-rm-interactive-query t
      eshell-mv-overwrite-files nil)

;; http://www.emacswiki.org/cgi-bin/wiki.pl/EshellWThirtyTwo
(when win32p
  (defun eshell/start (FILE)
    "Invoke (w32-shell-execute \"Open\" FILE) and substitute slashes for backslashes"
    (w32-shell-execute "Open"
                       (subst-char-in-string ?\\ ?/ (expand-file-name FILE))) nil))

;;;;;;;; eshell scrolling
;; http://www.emacswiki.org/cgi-bin/wiki/EshellScrolling
(setq eshell-scroll-show-maximum-output t
      eshell-scroll-to-bottom-on-output nil)

;;;;;;;; Backup
;; see also backup-by-coping 
(setq backup-directory-alist '(("." . "~/.emacs_backup")))
(setq
 version-control t                      ; Control use of version numbers for backup files.
 kept-old-versions 2                    ; 
 kept-new-versions 3                    ; 
 delete-old-versions t)                 ; If t, delete excess backup versions silently.

;;;;;;;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;;;;;;;; Parenthis matching
;; http://www.emacswiki.org/cgi-bin/wiki/MatchParenthesis
(defun match-paren () 
  "% command of vi" 
  (interactive) 
  (let ((char (char-after (point)))) 
    (cond ((memq char '(?\( ?\{ ?\[)) 
           (forward-sexp 1) 
           (backward-char 1)) 
          ((memq char '(?\) ?\} ?\])) 
           (forward-char 1) 
           (backward-sexp 1)) 
          (t (call-interactively 'self-insert-command)))))

;; (defun match-paren (arg)
;;   "Go to the matching paren if on a paren; otherwise insert %."
;;   (interactive "p")
;;   (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
;;         ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;;         (t (self-insert-command (or arg 1)))))

;; what about C-q % 
(global-set-key (kbd "%") 'match-paren)

;;;;;;;; timestamp
(defun my-insert-timestamp ()
  (interactive)
  (insert (format-time-string
           (cond ((and (featurep 'howm) howm-mode) "[%Y-%m-%d]")
                 (t "%Y/%m/%d %H:%M:%S")))))
(global-set-key (kbd "ESC ESC tm") 'my-insert-timestamp) ; tm stands for time


;;;;;;;; emacs-lisp-mode 
(add-hook 'emacs-lisp-mode-hook 
          (lambda ()
            (progn
              (eldoc-mode 1)
              (outline-minor-mode 1)
              (local-set-key (kbd "RET") 'newline-and-indent)
              (setq indent-tabs-mode nil))))

;;;;;;;;; gud 
(setq gdb-many-windows t)               ; 
(global-set-key [f9] 'gud-break)
(global-set-key [(shift f9)] 'gdb-delete-breakpoint)   ; like visual studio 
(global-set-key [f10] 'gud-next)
(global-set-key [f11] 'gud-step)
(global-set-key [(shift f11)] 'gud-finish)
(global-set-key [(shift f10)] '(lambda ()
                                 (interactive)
                                 (call-interactively 'gud-tbreak)
                                 (call-interactively 'gud-cont)))
(global-set-key (kbd "C-x C-a C-c") 'gud-cont)
(global-set-key (kbd "C-x C-a C-w") 'gud-watch)


;;;;;;;; move cursor multiple lines
(global-set-key [(M-down)] '(lambda () (interactive) (next-line 5)))
(global-set-key [(M-up)] '(lambda () (interactive) (previous-line 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; M-x list-packages
(require 'package)
(setq package-archives '(("ELPA"      . "http://tromey.com/elpa/")
                        ("gnu"       . "http://elpa.gnu.org/packages/")
                        ("marmalade" . "http://marmalade-repo.org/packages/")
                        ("melpa"     . "http://melpa.milkbox.net/packages/")))
(package-initialize)
;; install wanted packages.
(let ((wanted '(gtags solarized-theme auto-complete magit js3-mode
                      nyan-mode)))
  (dolist (package wanted)
    (unless (require package nil t)
      (package-install package))))

;; 이상하게 wanted에 입력해 놓으면 설치 안되는 녀석들은 list-packges를
;; 통해서 수동으로 설치하자.
;;
;; iedit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nyan-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (locate-library "nyan-mode")
  (require 'nyan-mode)
  (setq-default nyan-wavy-trail t)
  (nyan-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; iedit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fix iedit bug in Mac
(when (locate-library "iedit")
  (define-key global-map (kbd "C-c ;") 'iedit-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; csharp-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ggtags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (locate-library "gtags")
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x G") 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto complete mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired-x
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired-omit-files 
(require 'dired-x)
(defun add-to-dired-omit-files (pattern)
  (setq dired-omit-files
        (concat dired-omit-files
                "\\|"
                pattern)))
(add-to-dired-omit-files "^\\.hg$")
(add-to-dired-omit-files "^\\.svn$")
(add-to-dired-omit-files "^\\..+$")
(add-hook 'dired-mode-hook '(lambda () (dired-omit-mode 1)))  ; M-o


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; autoinsert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/cgi-bin/wiki/AutoInsertMode
(require 'autoinsert)
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs_robin/autoinsert/")
(setq auto-insert-query nil)
(define-auto-insert "\\<Makefile\\'" "autoinsert.makefile")
(define-auto-insert "\\.py\\'" "autoinsert.py")
(define-auto-insert "\\.pyw\\'" "autoinsert.py")

(setq auto-insert-alist
      (append `(
                (("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header")
                 "" (my-auto-insert-cpp-header))
                (("\\.\\([Cc]\\|cc\\|cpp\\)\\'" . "C / C++ body")
                 "" (my-auto-insert-cpp-body)))
              auto-insert-alist))

(defun my-auto-insert-cpp-header ()
  "__HEADER_H__ guard"
  (let ((guard (upcase
                (replace-regexp-in-string "\\." "_"
                                          (file-relative-name  (buffer-file-name))))))
    (format "#pragma once
#ifndef __%s__
#define __%s__

#endif // #ifndef __%s__" guard guard guard)))

(defun my-auto-insert-cpp-body ()
    (let*
      ((stem
        (file-name-sans-extension buffer-file-name))
       (header (cond
                ((file-exists-p (concat stem ".h"))
                 (file-name-nondirectory (concat stem ".h")))
                ((file-exists-p (concat stem ".hpp"))
                 (file-name-nondirectory (concat stem ".hpp")))
                ((file-exists-p (concat stem ".hh"))
                 (file-name-nondirectory (concat stem ".hh"))))))
      (format "#include \"%s\"

" header)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ido)
(ido-mode t)
(ido-everywhere 1)
(setq ido-use-filename-at-point 'guess
      ido-use-url-at-point t)
;;(iswitchb-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; c++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c++ indentation
(defconst my-c-style
  '((c-tab-always-indent          . t)
    (c-comment-only-line-offset   . 4)
    (c-hanging-braces-alist       . ((substatement-open after)
                                     (brace-list-open)))
    (c-hanging-colons-alist       . ((member-init-intro before)
                                     (inher-intro)
                                     (case-label after)
                                     (label after)
                                     (access-label after)))
    (c-cleanup-list               . (scope-operator
                                     empty-defun-braces
                                     defun-close-semi))
    (c-offsets-alist              . ((arglist-close . c-lineup-arglist)
                                     (substatement-open . 0)
                                     (case-label        . 0)
                                     (block-open        . 0)
                                     (namespace-open . 0)
                                     (namespace-close . 0)
                                     (innamespace . 0)
                                     (inextern-lang . 0)
                                     (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t)
    )
  "My C Programming Style")

;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  (c-add-style "PERSONAL" my-c-style t)
  (setq tab-width 4
        ;;indent-tabs-mode t
        c-basic-offset 4)
  (c-toggle-auto-state -1)              ; disable auto-newline mode
  (c-toggle-hungry-state 1)             ; enable hungry-delete mode

  ;; indentation with return
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)

  ;; C-c RET to switch between h and cpp files.
  (define-key c-mode-base-map [(control c)(return)] 'ff-find-other-file)

  ;; if enabled ecb mode, C-c C-c for ecb-goto-window-methods 
  (when (fboundp 'ecb-goto-window-methods)
    (define-key c-mode-base-map [(control c)(control c)] 'ecb-goto-window-methods))

  ;; if enabled cedet mode, META-Ret for semantic-analyze-possible-completions.
  (when (fboundp 'semantic-analyze-possible-completions)
    (define-key c-mode-base-map [(meta return)] 'semantic-ia-complete-symbol))
  
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'member-init-intro '++)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'comment-intro 0)
  (c-set-offset 'label 0)
  (c-set-offset 'arglist-intro '+)

  (hs-minor-mode 1)
  (hide-ifdef-mode 1)
  (setq hide-ifdef-lines t)             ; #if 
  (setq hide-ifdef-read-only t))         ; readonly in case of hide-ifdef-mode

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; 
(font-lock-add-keywords 'c++-mode
                        '(("\\<\\(FIXME\\):" 1 c-nonbreakable-space-face prepend)
                          ("\\<\\(TODO\\):" 1 c-nonbreakable-space-face prepend)
                          ("\\<\\(BUG\\):" 1 c-nonbreakable-space-face prepend)
                          ("\\<\\(NOTE\\):" 1 c-nonbreakable-space-face prepend)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; other files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when macosxp
  (load "~/.emacs_robin/emacs-mac"))
(when (locate-library "js3-mode")
  (load "~/.emacs_robin/emacs-js"))
;(when (eq system-type 'windows-nt)
;  (load "~/.emacs_robin/emacs-win"))

