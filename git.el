(defun sandric/magit-kill-buffers ()
  "Kill all magit buffers."
  (interactive)
  (mapc (lambda (buffer)
          (with-current-buffer buffer (magit-blame-quit))
          (when (s-starts-with? "magit" (format "%s" (buffer-local-value 'major-mode buffer)))
            (kill-buffer buffer)))
        (buffer-list)))

(defun sandric/magit-blame-in-right-pane ()
  "Magit blame in right pane"
  (interactive)
  (sandric/open-buffer-in-right-pane (current-buffer))
  (sandric/eval-in-right-pane '(lambda ()
                                 (interactive)
                                 (call-interactively 'magit-blame))))

(defun sandric/git-timemachine-in-right-pane ()
  "Git-timemachine in right pane"
  (interactive)
  (sandric/open-buffer-in-right-pane (current-buffer))
  (sandric/eval-in-right-pane '(lambda ()
                                 (interactive)
                                 (call-interactively 'git-timemachine))))



(use-package magit
  :ensure t
  :commands (magit-toplevel)
  :config (progn
            (setq ediff-window-setup-function 'ediff-setup-windows-plain)
            (setq ediff-window-setup-function 'ediff-setup-windows-plain)
            (setq ediff-diff-options "-w")
            (setq ediff-split-window-function 'split-window-horizontally)

            (define-key magit-mode-map (kbd "C-H-f") 'sandric/magit-kill-buffers))

  :bind (:map with-editor-mode-map
              ("C-H-S-r" . with-editor-finish)
              ("C-H-S-a" . with-editor-cancel)

              :map magit-status-mode-map
              ("p" . magit-push-popup)
              ("M-n" . nil)

              :map magit-log-mode-map
              ("M-n" . nil)

              :map magit-revision-mode-map
              ("M-n" . nil)

              :map git-commit-mode-map
              ("M-n" . nil)

              :map magit-blame-mode-map
              ("C-g" . magit-blame-quit))
  )

(use-package git-gutter+
  :ensure t
  :config (progn
            (setq git-gutter+-modified-sign "    ")
            (setq git-gutter+-added-sign "++++")
            (setq git-gutter+-deleted-sign "----")

            (set-face-background 'git-gutter+-modified "yellow")

            (set-face-background 'git-gutter+-added "green")
            (set-face-background 'git-gutter+-deleted "red")

            (global-git-gutter+-mode)))

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
