(setq inhibit-startup-screen t)(require 'package) ;; You might already have this line(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))                    (not (gnutls-available-p))))       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))  (add-to-list 'package-archives (cons "melpa" url) t))(when (< emacs-major-version 24)  ;; For important compatibility libraries like cl-lib  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))(package-initialize) ;; You might already have this line(defvar required-packages '(use-package))(eval-when-compile (require 'use-package))(load (expand-file-name "functions.el" user-emacs-directory))(load (expand-file-name "settings.el" user-emacs-directory))(load (expand-file-name "keys.el" user-emacs-directory))(load (expand-file-name "packages.el" user-emacs-directory))(load (expand-file-name "git.el" user-emacs-directory))(load (expand-file-name "rg.el" user-emacs-directory))(load (expand-file-name "fzf.el" user-emacs-directory))(load (expand-file-name "chords.el" user-emacs-directory))(load (expand-file-name "hydra.el" user-emacs-directory))(load (expand-file-name "org.el" user-emacs-directory))(load (expand-file-name "complete.el" user-emacs-directory))(load (expand-file-name "prog.el" user-emacs-directory))(load (expand-file-name "search.el" user-emacs-directory))(load (expand-file-name "dired.el" user-emacs-directory))(load (expand-file-name "modeline.el" user-emacs-directory))(custom-set-variables ;; custom-set-variables was added by Custom. ;; If you edit it by hand, you could mess it up, so be careful. ;; Your init file should contain only one such instance. ;; If there is more than one, they won't work right. '(Man-notify-method (quote pushy)) '(company-idle-delay 0.1) '(custom-safe-themes   (quote    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))) '(display-buffer-base-action (quote (display-buffer-reuse-window (reusable-frames . t)))) '(package-selected-packages   (quote    (vue-mode alchemist projectile-rails hydra gitignore-mode git-timemachine slim-mode dired-toggle-sudo lua-mode multi-term fish-mode magithub regex-tool ruby-electric coffee-mode rjsx-mode company-tern emamux emacs-emamux command-log-mode xterm-color company-web exec-path-from-shell indium cyrillic-ukrainian markdown-mode sx key-chord nodejs-repl tern yaml-mode scss-mode emmet-mode web-mode robe inf-ruby rubocop restclient flycheck evil-nerd-commenter company yasnippet git-gutter+ magit org-bullets fzf smartparens multiple-cursors aggressive-indent window-numbering circe auto-compile shackle f s esup counsel-dash counsel smex swiper ivy rainbow-delimiters rainbow-delimiter undo-tree rainbow-mode))))(custom-set-faces ;; custom-set-faces was added by Custom. ;; If you edit it by hand, you could mess it up, so be careful. ;; Your init file should contain only one such instance. ;; If there is more than one, they won't work right. '(default ((t (:inherit nil :stipple nil :background "lemon chiffon" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 125 :width normal :foundry "SRC" :family "Hack")))) '(mode-line ((t (:background "#F097AD" :foreground "black")))) '(mode-line-inactive ((t (:inherit mode-line :background "grey90" :foreground "grey20" :weight light)))) '(region ((t (:background "pink1" :distant-foreground "gtk_selection_fg_color")))))(put 'scroll-left 'disabled nil)