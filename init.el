(setq inhibit-startup-screen t)


(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(defvar required-packages '(use-package))
(eval-when-compile (require 'use-package))


(load (expand-file-name "settings.el" user-emacs-directory))
(load (expand-file-name "functions.el" user-emacs-directory))
(load (expand-file-name "keys.el" user-emacs-directory))
(load (expand-file-name "packages.el" user-emacs-directory))
(load (expand-file-name "chords.el" user-emacs-directory))
(load (expand-file-name "hydra.el" user-emacs-directory))
(load (expand-file-name "nlinum.el" user-emacs-directory))
(load (expand-file-name "fzf.el" user-emacs-directory))
(load (expand-file-name "header.el" user-emacs-directory))
(load (expand-file-name "ibuffer.el" user-emacs-directory))
(load (expand-file-name "org.el" user-emacs-directory))
(load (expand-file-name "git.el" user-emacs-directory))
(load (expand-file-name "complete.el" user-emacs-directory))
(load (expand-file-name "prog.el" user-emacs-directory))
(load (expand-file-name "search.el" user-emacs-directory))
(load (expand-file-name "dired.el" user-emacs-directory))
(load (expand-file-name "modeline.el" user-emacs-directory))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.1)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(dired-hide-details-hide-information-lines nil)
 '(diredp-hide-details-initially-flag nil)
 '(package-selected-packages
   (quote
    (key-chord dired-narrow dired+ nodejs-repl tern yaml-mode scss-mode emmet-mode web-mode robe inf-ruby rubocop restclient flycheck evil-nerd-commenter company yasnippet git-gutter+ magit org-bullets fzf zygospore smartparens multiple-cursors expand-region aggressive-indent window-numbering highlight-symbol circe auto-compile shackle f s simpleclip esup counsel-dash wgrep counsel smex swiper ivy rainbow-delimiters rainbow-delimiter undo-tree rainbow-mode solarized-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-keyword-face ((t (:foreground "blue" :weight bold))))
 '(mode-line ((t (:background "#F097AD" :foreground "black"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey90" :foreground "grey20" :weight light)))))


(find-file "~/.emacs.d/init.el")
