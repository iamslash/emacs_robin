;; -*- coding: utf-8 -*-
;; -*- mode: Emacs-Lisp; outline-regexp: "^;;;; .*";  -*-

;; https://github.com/emacs-tw/awesome-emacs 
;;   A community driven list of useful Emacs packages, libraries and others.
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
(setq default-tab-width 4)
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
;; http://props.tistory.com/35
(when enable-multibyte-characters
  (set-language-environment "Korean")
  (setq file-coding-system 'utf-8)
  (setq display-coding-system 'utf-8)
  (setq-default coding-system 'utf-8)
  (setq-default buffer-coding-system 'utf-8)
  (setq-default buffer-file-coding-system 'utf-8)
  (setq keyboard-coding-system 'utf-8)
  (setq terminal-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (set-clipboard-coding-system 'utf-8)   
  (setq input-method-verbose-flag nil
        input-method-highlight-flag nil)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  ;; Hangul Mail setting
  (setq-default sendmail-coding-system 'utf-8))

;; on windows 7 should use euc-kr
(when (and enable-multibyte-characters win32p)
  (set-w32-system-coding-system 'euc-kr)
  ;; (setq-default file-name-coding-system 'euc-kr)
  (setq terminal-coding-system 'euc-kr)
  (setq keyboard-coding-system 'euc-kr)
  ;(setq shell-coding-system 'euc-kr)
  (set-selection-coding-system 'euc-kr)
  (set-clipboard-coding-system 'euc-kr))

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
;; http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/
(require 'package)
(setq package-archives '(("ELPA"      . "http://tromey.com/elpa/")
                        ("gnu"       . "http://elpa.gnu.org/packages/")
                        ("melpa"     . "https://melpa.org/packages/")))
(defvar prelude-packages
  '(csharp-mode solarized-theme auto-complete magit js3-mode
                nyan-mode iedit yasnippet 
                flymake-cursor google-c-style php-mode go-mode
                go-autocomplete markdown-mode ggtags elm-mode
                clojure-mode cider markdown-toc kotlin-mode
                swift-mode typescript-mode)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (cl-loop for p in prelude-packages
        when (not (package-installed-p p)) do (cl-return nil)
        finally (cl-return t)))
(package-initialize)
(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; typescript-mode
;;; https://github.com/emacs-typescript/typescript.el 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'typescript-mode-hook
  (lambda ()
    ;; Customize compile command to run MainApp.kt
    (if (not (string-match "ts" compile-command))
        (set (make-local-variable 'compile-command)
             "ts-node a.ts"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; kotlin-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'kotlin-mode-hook
  (lambda ()
    ;; Customize compile command to run MainApp.kt
    (if (not (string-match "kt" compile-command))
        (set (make-local-variable 'compile-command)
             "kotlinc MainApp.kt -include-runtime -d MainApp.jar"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; java-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'java-mode-hook
  (lambda ()
    (setq indent-tabs-mode f)
    (setq c-basic-offset 4)
    (setq tab-width 2)
    ;; Customize compile command to run MainApp.java
    (if (not (string-match "java" compile-command))
        (set (make-local-variable 'compile-command)
             "javac MainApp.java"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; python-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook
  (lambda ()
    (setq indent-tabs-mode f)
    (setq tab-width 4)
    (setq python-indent-offset 4)
    ;; Customize compile command to run a.py
    (if (not (string-match "py" compile-command))
        (set (make-local-variable 'compile-command)
             "python a.py"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; clojure-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (locate-library "clojure-mode")
  (add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
  (add-to-list 'load-path "~/.emacs.d/clojure-mode")
  (require 'clojure-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cider
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (locate-library "cider")
  (add-hook 'cider-mode-hook #'clj-refactor-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; go-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0. install go
;; > brew install go
;;
;; 0. setting .bash_profile
;;   export GOROOT=/usr/local/opt/go       
;;   export GOPATH=~/my/go
;;   export PATH=$PATH:$GOROOT/bin
;;   export PATH=$PATH:$GOPATH/bin
;;
;; 1. install go package
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/vet
;; go get -u golang.org/x/tools/cmd/oracle
;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u github.com/golang/lint/golint
;; go get -u github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/tleyden/checkers-bot-minimax
;;
(when (locate-library "go-mode")
  (autoload 'go-mode "go-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (defun my-go-mode-hook ()
    (setq tab-width 4)
    (setq standard-indent 4)
    (setq indent-tabs-mode nil)
    ;; (setq default-tab-width 2)
    ;; Use goimports instead of go-fmt
    (setq gofmt-command "goimports")
    ;; Call Gofmt before saving
    (add-hook 'before-save-hook 'gofmt-before-save)
    ;; Customize compile command to run go build
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go generate && go build -v && go test -v && go vet"))
    ;; ;; Go oracle
    ;; (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
    ;; Godef jump key binding
    (local-set-key (kbd "M-.") 'godef-jump))
  (add-hook 'go-mode-hook 'my-go-mode-hook)

  (defun auto-complete-for-go ()
    (auto-complete-mode 1))
  (add-hook 'go-mode-hook 'auto-complete-for-go)
  (with-eval-after-load 'go-mode
    (require 'go-autocomplete)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; docview
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fset 'doc-prev "\C-xo\C-x[\C-xo")
(fset 'doc-next "\C-xo\C-x]\C-xo")
(global-set-key (kbd "M-[") 'doc-prev)
(global-set-key (kbd "M-]") 'doc-next)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; solarized-theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; solarized-light 
(when window-system
  (load-theme 'solarized-light t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nyan-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (locate-library "nyan-mode")
  (require 'nyan-mode)
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
(when (locate-library "csharp-mode")
  (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
  (add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
  (defun my-csharp-mode-fn ()
     "function that runs when csharp-mode is initialized for a buffer."
     (turn-on-auto-revert-mode)
     (setq indent-tabs-mode nil))
  (add-hook  'csharp-mode-hook 'my-csharp-mode-fn t))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; php-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ggtags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install)
;; 0. Install Global with support for exuberant ctags
;;     https://github.com/leoliu/ggtags/wiki/Install-Global-with-support-for-exuberant-ctags
;;   > brew install --HEAD ctags
;;   > brew install global --with-exuberant-ctags
;; 1. list-packages ggtags
;; 2. set 
;; Usage) https://github.com/leoliu/ggtags
;;
;; How to make tags
;; > 
(when (locate-library "ggtags")
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
(define-auto-insert "\\.cpp\\'" "autoinsert.cpp")
(define-auto-insert "\\.cs\\'" "autoinsert.cs")
(define-auto-insert "\\.go\\'" "autoinsert.go")
(define-auto-insert "\\.java\\'" "autoinsert.java")
(define-auto-insert "\\.js\\'" "autoinsert.js")
(define-auto-insert "\\README.md\\'" "README.md")
(define-auto-insert "\\.kt\\'" "autoinsert.kt")
(define-auto-insert "\\.ts\\'" "autoinsert.ts")

;; (setq auto-insert-alist
;;       (append `(
;;                 (("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header")
;;                  "" (my-auto-insert-cpp-header))
;;                 (("\\.\\([Cc]\\|cc\\|cpp\\)\\'" . "C / C++ body")
;;                  "" (my-auto-insert-cpp-body)))
;;               auto-insert-alist))

;; (defun my-auto-insert-cpp-header ()
;;   "__HEADER_H__ guard"
;;   (let ((guard (upcase
;;                 (replace-regexp-in-string "\\." "_"
;;                                           (file-relative-name  (buffer-file-name))))))
;;     (format "#pragma once
;; #ifndef __%s__
;; #define __%s__

;; #endif // #ifndef __%s__" guard guard guard)))

;; (defun my-auto-insert-cpp-body ()
;;     (let*
;;       ((stem
;;         (file-name-sans-extension buffer-file-name))
;;        (header (cond
;;                 ((file-exists-p (concat stem ".h"))
;;                  (file-name-nondirectory (concat stem ".h")))
;;                 ((file-exists-p (concat stem ".hpp"))
;;                  (file-name-nondirectory (concat stem ".hpp")))
;;                 ((file-exists-p (concat stem ".hh"))
;;                  (file-name-nondirectory (concat stem ".hh"))))))
;;       (format "#include \"%s\"

;; " header)))



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

;; yasnippet
; start yasnippet with emacs
(when (locate-library "yasnippet")
  (require 'yasnippet)
  (yas-global-mode 1))
;; auto-complete-c-headers
(when (locate-library "auto-complete-c-headers")
  ; let's define a function which initializes auto-complete-c-headers and gets called for c/c++ hooks
  (defun my:ac-c-header-init ()
    (require 'auto-complete-c-headers)
    (add-to-list 'ac-sources 'ac-source-c-headers)
    (add-to-list 'achead:include-directories '"/usr/include/c++/4.2.1"))
  ; now let's call this function from c/c++ hooks
  (add-hook 'c++-mode-hook 'my:ac-c-header-init)
  (add-hook 'c-mode-hook 'my:ac-c-header-init))
;; flymake-google-cpplint
; 0. list-package flymake-google-cpplint
; 1. sudo pip install cpplint
; 2. cpplint path should be added to PATH
(defun my:flymake-google-init ()
  (require 'flymake-google-cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command "/usr/local/bin/cpplint"))
  (flymake-google-cpplint-load))

(when (locate-library "flymake-google-cpplint")
  (add-hook 'c-mode-hook 'my:flymake-google-init)
  (add-hook 'c++-mode-hook 'my:flymake-google-init)
  ; start google-c-style with emacs
  (when (locate-library "google-c-style")
    (require 'google-c-style)
    (add-hook 'c-mode-common-hook 'google-set-c-style)
    (add-hook 'c-mode-common-hook 'google-make-newline-indent)))

;; Customize compile command to run a.cpp
(add-hook 'c++-mode-hook
  (lambda ()
    (if (not (string-match "cpp" compile-command))
    (set (make-local-variable 'compile-command)
         "g++ -std=c++11 -o a.out a.cpp"))))

;; NOTICE: intellicense does not work and too slow...
;; ;; semantic-mode
;; ; turn on Semantic
;; (semantic-mode 1)
;; ; let's define a function which adds semantic as a suggestion backend to auto complete
;; ; and hook this function to c-mode-common-hook
;; (defun my:add-semantic-to-autocomplete() 
;;   (add-to-list 'ac-sources 'ac-source-semantic))
;; (add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)

;; ;; c++ indentation
;; (defconst my-c-style
;;   '((c-tab-always-indent          . t)
;;     (c-comment-only-line-offset   . 4)
;;     (c-hanging-braces-alist       . ((substatement-open after)
;;                                      (brace-list-open)))
;;     (c-hanging-colons-alist       . ((member-init-intro before)
;;                                      (inher-intro)
;;                                      (case-label after)
;;                                      (label after)
;;                                      (access-label after)))
;;     (c-cleanup-list               . (scope-operator
;;                                      empty-defun-braces
;;                                      defun-close-semi))
;;     (c-offsets-alist              . ((arglist-close . c-lineup-arglist)
;;                                      (substatement-open . 0)
;;                                      (case-label        . 0)
;;                                      (block-open        . 0)
;;                                      (namespace-open . 0)
;;                                      (namespace-close . 0)
;;                                      (innamespace . 0)
;;                                      (inextern-lang . 0)
;;                                      (knr-argdecl-intro . -)))
;;     (c-echo-syntactic-information-p . t)
;;     )
;;   "My C Programming Style")

;; ;; Customizations for all modes in CC Mode.
;; (defun my-c-mode-common-hook ()
;;   (c-add-style "PERSONAL" my-c-style t)
;;   (setq tab-width 4
;;         ;;indent-tabs-mode t
;;         c-basic-offset 4)
;;   (c-toggle-auto-state -1)              ; disable auto-newline mode
;;   (c-toggle-hungry-state 1)             ; enable hungry-delete mode

;;   ;; indentation with return
;;   (define-key c-mode-base-map "\C-m" 'newline-and-indent)

;;   ;; C-c RET to switch between h and cpp files.
;;   (define-key c-mode-base-map [(control c)(return)] 'ff-find-other-file)

;;   ;; if enabled ecb mode, C-c C-c for ecb-goto-window-methods 
;;   (when (fboundp 'ecb-goto-window-methods)
;;     (define-key c-mode-base-map [(control c)(control c)] 'ecb-goto-window-methods))

;;   ;; if enabled cedet mode, META-Ret for semantic-analyze-possible-completions.
;;   (when (fboundp 'semantic-analyze-possible-completions)
;;     (define-key c-mode-base-map [(meta return)] 'semantic-ia-complete-symbol))
  
;;   (c-set-offset 'substatement-open 0)
;;   (c-set-offset 'member-init-intro '++)
;;   (c-set-offset 'inline-open 0)
;;   (c-set-offset 'comment-intro 0)
;;   (c-set-offset 'label 0)
;;   (c-set-offset 'arglist-intro '+)

;;   (hs-minor-mode 1)
;;   (hide-ifdef-mode 1)
;;   (setq hide-ifdef-lines t)             ; #if 
;;   (setq hide-ifdef-read-only t))         ; readonly in case of hide-ifdef-mode

;; (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; 
(font-lock-add-keywords 'c++-mode
                        '(("\\<\\(FIXME\\):" 1 c-nonbreakable-space-face prepend)
                          ("\\<\\(TODO\\):" 1 c-nonbreakable-space-face prepend)
                          ("\\<\\(BUG\\):" 1 c-nonbreakable-space-face prepend)
                          ("\\<\\(NOTE\\):" 1 c-nonbreakable-space-face prepend)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ansi-color
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; other files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when macosxp
  (load "~/.emacs_robin/emacs-mac"))
(when (locate-library "js3-mode")
  (load "~/.emacs_robin/emacs-js"))
(when (eq system-type 'windows-nt)
  (load "~/.emacs_robin/emacs-win"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zscaler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; don't work why???
;; (add-to-list 'gnutls-trustfiles "~/.zscaler/zscaler.pem")
