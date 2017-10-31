(require 'cl)

(setq load-prefer-newer t)

(menu-bar-mode -1)

;; (global-linum-mode t)
(delete-selection-mode t)
(show-paren-mode t)
(blink-cursor-mode t)
(winner-mode t)
(column-number-mode t)
(setq shift-select-mode t)
(setq-default indent-tabs-mode nil)

(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(cua-mode t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(cursor-color . "#F74874"))

(setq use-dialog-box nil)
(setq sgml-basic-offset 2)
(setq debug-on-error t)
(setq whitespace-line-column 800)
(setq ns-function-modifier 'hyper)
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq show-paren-style 'parenthesis)

(setq enable-recursive-minibuffers t)
(setq confirm-nonexistent-file-or-buffer nil)
(setq initial-major-mode (quote text-mode))
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix
      temporary-file-directory)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default tab-width 2)
