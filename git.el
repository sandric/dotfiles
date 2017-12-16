(use-package magit
  :ensure t
  :commands (magit-toplevel)
  :config (progn
            (setq ediff-window-setup-function 'ediff-setup-windows-plain)
            (setq ediff-window-setup-function 'ediff-setup-windows-plain)
            (setq ediff-diff-options "-w")
            (setq ediff-split-window-function 'split-window-horizontally))

  :bind (:map with-editor-mode-map
              ("<C-f4> r" . with-editor-finish)
              ("<C-f4> a" . with-editor-cancel)

              :map magit-status-mode-map
              ("p" . magit-push-popup)
              ("M-n" . nil)
              ("<C-f2> r" . magit-refresh)

              :map magit-log-mode-map
              ("M-n" . nil)

              :map magit-revision-mode-map
              ("M-n" . nil)

              :map git-commit-mode-map
              ("M-n" . nil)

              :map magit-blame-mode-map
              ("C-g" . magit-blame-quit)))

(use-package magithub
  :ensure t
  :config (progn
            (setq ghub-token (shell-command-to-string "git config --get github.oauth-token"))))

(use-package github-clone
  :ensure t)

(use-package git-timemachine
  :ensure t
  :defer t
  :commands (git-timemachine)
  :config (progn
            (require 'git-timemachine)
            (custom-set-faces
             '(git-timemachine-minibuffer-detail-face
               ((t (:foreground "selectedMenuItemColor")))))

            (define-key git-timemachine-mode-map (kbd "C-g")
              'git-timemachine-quit)
            (define-key git-timemachine-mode-map (kbd "C")
              'git-timemachine-kill-abbreviated-revision)
            (define-key git-timemachine-mode-map (kbd "L")
              'git-timemachine-kill-revision)
            (define-key git-timemachine-mode-map (kbd "N")
              'git-timemachine-show-previous-revision)
            (define-key git-timemachine-mode-map (kbd "I")
              'git-timemachine-show-next-revision)))

(use-package gitignore-mode
  :ensure t
  :defer t)
