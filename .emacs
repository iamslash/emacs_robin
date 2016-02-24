;; -*- coding: utf-8 -*-
;; -*- mode: Emacs-Lisp; outline-regexp: "^;;;; .*";  -*-

;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; config for platform, machine 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst win32p (eq system-type 'windows-nt) "true if window")
(defconst linuxp (eq system-type (or 'gnu/linux 'berkeley-unix)) "true if linux")
(defconst macosxp (eq system-type 'darwin))
(defconst homep (string-equal system-name "SALSHHOMEONE") "true if home")
(defconst officep (string-equal system-name "DO-PC") "true if office")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; global settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-font-lock-mode 1)               ; syntanx highlight
(transient-mark-mode 1)                 ; marking highlight
(show-paren-mode t)                     ; 
(if (functionp 'global-hi-lock-mode)    ; C-x w h 등으로 특정 단어들을 빛내준다
    (global-hi-lock-mode 1)
    (hi-lock-mode 1))
;;(global-hl-line-mode 1)               ; 현재줄을 빛내준다. 이거 좀 불편해서 뺐다.
(setq ring-bell-function (lambda () nil)) ; bell 무시
(line-number-mode 1)                    ; mode line 에 라인수를 표시한다
(column-number-mode 1)                  ; mode line 에 컬럼을 표시한다(기본이 아니더라)
(setq scroll-step 1)                    ; 윈도스런 스크롤을 위해서..
(setq scroll-conservatively 4096)    
(delete-selection-mode 1)               ; 윈도우처럼, 선택된 regeion 을 DEL 로 지우거나, 다른 글자를 타이핑 할때 즉시 지운다.
(setq-default truncate-lines t)         ; 화면을 벗어나는 긴 줄처리 toggle-truncate-lines 참고
;;(dynamic-completion-mode)             ; 음 이게 뭐드라? M-/ 던가 M-RET 던가
;; Set the text for titlebar and icons, %f=filename, %b=buffername
(setq frame-title-format (list "GNU Emacs " emacs-version "@" system-name " - " '(buffer-file-name "%f" "%b")))
(setq icon-title-format frame-title-format)
(which-function-mode 1) ; 어떤 함수를 수정중인지 표현 semantic-stickyfunc-mode 보다는 이게 보기 좋다
(tool-bar-mode -1)                      ; toolbar 는 거의 안쓰니 꺼버린다
(auto-compression-mode 1)               ; 가끔 필요해서..
(setq-default indent-tabs-mode nil)     ; TAB 보다 space 가 더 좋아졌다
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1)) ; 스크롤바눈에거슬린다
;; http://emacswiki.org/cgi-bin/wiki?InteractiveSearch
;; isearch 중 붙여넣기를 하려면 C-y 가 아닌 M-y 를 해야 하는데
;; 이건 뭔가 아니다. C-y 로 바꾸자
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)
;;;; X 에서 휠마우스 버튼 쓰기
;;(global-set-key [(mouse-4)] 'scroll-down)
;;(global-set-key [(mouse-5)] 'scroll-up)
;;;; 윈도에서는 윈도키를 잘 써먹자
(when win32p
  (setq w32-pass-lwindow-to-system nil
        w32-pass-rwindow-to-system nil
        w32-pass-apps-to-system    nil
        w32-lwindow-modifier       'super   ;; Left Windows
        w32-rwindow-modifier       'super   ;; Rigth Windows
        w32-apps-modifier          'hyper) ;; App-Menu (right to Right Windows)
  (global-set-key [(super g)] 'goto-line))
;;;; 버퍼 전환 설정(http://www.emacswiki.org/cgi-bin/wiki/CategoryBufferSwitching)
;; from http://www.emacswiki.org/cgi-bin/wiki/BufferMenu
(global-set-key (kbd "C-x C-b") 'buffer-menu)
;; (when macosxp
;;   (global-set-key (kbd "S-SPC") 'toggle-korean-input-method))

;; 단축키설정
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-_") 'text-scale-decrease)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CODEC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Font
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when win32p
  (set-face-font 'default "-outline-Courier New-normal-normal-normal-mono-16-*-*-*-c-*-iso8859-1")
  ;;   (set-face-font 'default "fixed")  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; M-x list-packages
(require 'package)
(setq package-archives '(("ELPA"      . "http://tromey.com/elpa/")
                        ("gnu"       . "http://elpa.gnu.org/packages/")
                        ("marmalade" . "http://marmalade-repo.org/packages/")
                        ("melpa"     . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; install wanted packages.
(let ((wanted '(gtags etags-select deft ace-jump-mode find-file-in-repository
                      smex iedit flycheck go-mode go-eldoc key-chord
                      highlight-symbol wgrep hideshowvis ido-ubiquitous
                      flx-ido projectile auto-complete flymake-cursor)))
 (dolist (package wanted)
   (unless (require package nil t)
     (package-install package))))
