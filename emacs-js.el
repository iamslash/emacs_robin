;;; emacs-js.el --- js mode                          -*- lexical-binding: t; -*-

;; Copyright (C) 2016  iamslash

;; Author: iamslash <iamslash@ncrewui-MacBook-Pro.local>
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



(provide 'emacs-js)
;;; emacs-js.el ends here

;Associate files with js mode
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;js2-mode - A better js mode for Emacs
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js3-mode-hook 'ac-js3-mode)

(setq js2-highlight-level 3)

;; ;Using paredit with javascript
;; (define-key js-mode-map "{" 'paredit-open-curly)
;; (define-key js-mode-map "}" 'paredit-close-curly-and-newline)

;;
(add-hook 'js-mode-hook
  (lambda ()
    (setq indent-tabs-mode t)
    (setq tab-width 2)
    ;; Customize compile command to run a.js
    (if (not (string-match "js" compile-command))
        (set (make-local-variable 'compile-command)
             "node a.py"))))
