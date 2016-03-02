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
              '(font . "DejaVu Sans Mono-15")))


;;; solarized-light 
;; (when window-system
;;  (load-theme 'solarized-light t))

;; ;; get env from .bash_profile
;; ;; (setenv "PATH" (concat "~/opt/bin:"
;; ;;                        "/usr/local/bin:"
;; ;;                        "/usr/local/sbin:"
;; ;;                        (getenv "PATH")))
;; ;; (setq exec-path
;; ;;       (append `(,(expand-file-name "~/opt/bin")
;; ;;                 "/usr/local/bin"
;; ;;                 "/usr/local/sbin")
;; ;;               exec-path))
;; ;; better choice
;; ;;; https://github.com/purcell/exec-path-from-shell
;; (require 'exec-path-from-shell)
;; (exec-path-from-shell-initialize)
;; (exec-path-from-shell-copy-env "GOPATH")

;; for LANG setting
(set-locale-environment "ko_KR.UTF-8")

;;; mdfind is spotlight command
(setq locate-command "mdfind")
