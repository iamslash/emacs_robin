;;; mac 으로 옮기면서 가장 당황했던것중 하나.
;;; 일단 한글입력을 손에 익은 키로 바꿨다.
(setq default-input-method "korean-hangul")
(define-key global-map (kbd "S-SPC")  'toggle-input-method)


;;; HOME 하고 END 가 돌아가는 방식도 다르더라. 빡침.
(define-key global-map (kbd "<home>") 'beginning-of-line)
(define-key global-map (kbd "<end>")  'end-of-line)

;;; iterm 에서는 alt 가 alt 가 아니더라. 땜질해줌ㅋ
(unless window-system
 (setq mac-option-modifier 'meta))

;;; 폰트조절좀 하자. 일단 크기만 좀 키웠음.ㅋ
;;; macos 한글폰트 그닥 맘에 안드는게 일단 참아보자.
(when window-system
 ;; (set-frame-font "monaco-15")
 (add-to-list 'default-frame-alist
              '(font . "DejaVu Sans Mono-15")))


;;; solarized-light 맘에 들었음. 계속 쓴다.
;; (when window-system
;;  (load-theme 'solarized-light t))


;;; F5 를 누를때마다 새로운 터미널이 뜨는게 내 작업 취향.
;;; macos 에선 open 이란놈이 상당히 다양한 일을 해주는것 같다.
;; (defun yoonkn-run-cmd ()
;;   (interactive)
;;   (start-process "terminal" nil "open" "-n" "-a" "Terminal.app"))
;;; 위의 방식으론 좀 어색하게 돌아가더라.
;;; http://www.emacswiki.org/emacs/MacOSTweaks
;;; 에서 함수 가져왔다.
(defun mac-open-terminal ()
  (interactive)
  (let ((dir ""))
    (cond
     ((and (local-variable-p 'dired-directory) dired-directory)
      (setq dir dired-directory))
     ((stringp (buffer-file-name))
      (setq dir (file-name-directory (buffer-file-name))))
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
end tell" dir))))
(defun yoonkn-run-cmd ()
 (interactive)
 (mac-open-terminal))

;;; meta-F5 를 누르면 탐색기가 뜨는게 내 작업 취향.
;;; macos 에선 파인더라고 불리는 모양임?
(defun yoonkn-run-file-browser ()
 (interactive)
 (start-process "finder" nil "open" "."))
;;; 터미널 여는게 위의 내 방식대



;;; 띄우고 보니 .bash_profile 의 세팅을 가져오지 않더라.
;;; 직접 넣어줌.. 헐..
;; (setenv "PATH" (concat "~/opt/bin:"
;;                        "/usr/local/bin:"
;;                        "/usr/local/sbin:"
;;                        (getenv "PATH")))
;; (setq exec-path
;;       (append `(,(expand-file-name "~/opt/bin")
;;                 "/usr/local/bin"
;;                 "/usr/local/sbin")
;;               exec-path))
;;; 추가.. 더 좋은게 있더라
;;; https://github.com/purcell/exec-path-from-shell
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "GOPATH")

;;; emacs 를 app 형태로 실행하면 LANG 이 C 로 박힌 상태로
;;; 실행되더라. .bash_profile 못읽는거랑 유사한 문제 같은데..
;;; 일단 직접 박아줌.
(set-locale-environment "ko_KR.UTF-8")

;;; locate 대신 mdfind 를 쓴다. 이게 spotlight 의 커맨드라인 버전이라네
(setq locate-command "mdfind")
