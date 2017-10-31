(defun sandric/swiper-or-region (beg end)
  "Swiper region or 'empty string' if none highlighted."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (and beg end)
      (progn
        (deactivate-mark)
        (swiper (buffer-substring-no-properties beg end)))
    (swiper)))


(defun sandric/ag-or-region (beg end)
  "AG region or 'empty string' if none highlighted."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (and beg end)
      (progn
        (deactivate-mark)
        (counsel-ag (buffer-substring-no-properties beg end)))
    (counsel-ag)))

(defun sandric/pt-or-region (beg end)
  "PT region or 'empty string' if none highlighted."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (and beg end)
      (progn
        (deactivate-mark)
        (setq counsel-current-region (buffer-substring-no-properties beg end))
        (run-at-time 0.5 nil (lambda ()
                               (sandric/select-minibuffer)
                               (insert counsel-current-region)
                               (backward-char)))
        (counsel-pt))
    (counsel-pt)))

(defun sandric/rg-or-region (beg end)
  "RG region or 'empty string' if none highlighted."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (and beg end)
      (progn
        (deactivate-mark)
        (setq counsel-current-region (buffer-substring-no-properties beg end))
        (run-at-time 0.5 nil (lambda ()
                               (sandric/select-minibuffer)
                               (insert counsel-current-region)
                               (backward-char)))
        (counsel-rg))
    (counsel-rg)))


(defun sandric/ivy-symbol ()
  "Ivy mode-map symbol command."
  (interactive)
  (with-ivy-window
    (setq current-ivy-symbol (thing-at-point 'symbol t)))
  (insert current-ivy-symbol))

(defun sandric/ivy-replace ()
  "Ivy replace with mc selction."
  (interactive)
  (run-at-time nil nil (lambda ()
                         (print "here text:")
                         (print ivy-text)
                         (ivy-wgrep-change-to-wgrep-mode)
                         (goto-line 5)
                         (mc/mark-all-in-region (point) (point-max) ivy-text)))
  (ivy-occur))

(defun sandric/wgrep-discard-and-kill-buffer ()
  "Discard wgrep changes and kill buffer."
  (interactive)
  (wgrep-abort-changes))


(defun sandric/ag-home ()
  "AG in home directory."
  (interactive)
  (setq default-directory "/home/sandric/")
  (sandric/ag-or-region))

(defun sandric/ag-emacs ()
  "AG in Emacs directory."
  (interactive)
  (setq default-directory "/home/sandric/.emacs.d/")
  (sandric/ag-or-region))

(defun sandric/ag-projects ()
  "AG in projects directory."
  (interactive)
  (setq default-directory "/home/sandric/projects/")
  (sandric/ag-or-region))

(defun sandric/ag-rbenv ()
  "AG in rbenv directory."
  (interactive)
  (setq default-directory (sandric/rbenv-gems-directory))
  (sandric/ag-or-region))

(defun sandric/ag-npm ()
  "AG in npm directory."
  (interactive)
  (setq default-directory "/home/sandric/.node_modules/")
  (sandric/ag-or-region))

(defun sandric/ag-git ()
  "AG in git root."
  (interactive)
  (setq default-directory "/home/sandric/")
  (sandric/ag-or-region))


(defun sandric/pt-home ()
  "PT in home directory."
  (interactive)
  (setq default-directory "/home/sandric/")
  (sandric/pt-or-region))

(defun sandric/pt-emacs ()
  "PT in Emacs directory."
  (interactive)
  (setq default-directory "/home/sandric/.emacs.d/")
  (sandric/pt-or-region))

(defun sandric/pt-projects ()
  "PT in projects directory."
  (interactive)
  (setq default-directory "/home/sandric/projects/")
  (sandric/pt-or-region))

(defun sandric/pt-rbenv ()
  "PT in rbenv directory."
  (interactive)
  (setq default-directory (sandric/rbenv-gems-directory))
  (sandric/pt-or-region))

(defun sandric/pt-npm ()
  "PT in npm directory."
  (interactive)
  (setq default-directory "/home/sandric/.node_modules/")
  (sandric/pt-or-region))

(defun sandric/pt-git ()
  "PT in git root."
  (interactive)
  (setq default-directory "/home/sandric/")
  (sandric/pt-or-region))


(defun sandric/rg-home ()
  "RG in home directory."
  (interactive)
  (setq default-directory "/home/sandric/")
  (sandric/rg-or-region))

(defun sandric/rg-emacs ()
  "RG in Emacs directory."
  (interactive)
  (setq default-directory "/home/sandric/.emacs.d/")
  (sandric/rg-or-region))

(defun sandric/rg-projects ()
  "RG in projects directory."
  (interactive)
  (setq default-directory "/home/sandric/projects/")
  (sandric/rg-or-region))

(defun sandric/rg-rbenv ()
  "RG in rbenv directory."
  (interactive)
  (setq default-directory (sandric/rbenv-gems-directory))
  (sandric/rg-or-region))

(defun sandric/rg-npm ()
  "RG in npm directory."
  (interactive)
  (setq default-directory "/home/sandric/.node_modules")
  (sandric/rg-or-region))

(defun sandric/rg-git ()
  "RG in git root."
  (interactive)
  (setq default-directory "/home/sandric/")
  (sandric/rg-or-region))


(defun sandric/counsel-dash-at-point ()
  "Find documentation at point."
  (interactive)
  (counsel-dash (thing-at-point 'symbol)))

(defun browse-dash-split-chrome (url &rest args)
  "Browse URL in chrome split for dash."
  (browse-url url args)
  (sandric/hammerspoon-split-chrome))



(use-package ivy
  :ensure t
  :defer t
  :config (progn
            (defun ivy--buffer-list (str &optional virtual predicate)
              "Return the buffers that match STR.
            When VIRTUAL is non-nil, add virtual buffers."
              (delete-dups
               (append
                (mapcar
                 (lambda (x)
                   (if (with-current-buffer x
                         (and default-directory
                              (file-remote-p
                               (abbreviate-file-name default-directory))))
                       (propertize x 'face 'ivy-remote)
                     (let ((face (with-current-buffer x
                                   (cdr (assoc major-mode
                                               ivy-switch-buffer-faces-alist)))))
                       (if face
                           (propertize x 'face face)
                         x))))
                 (all-completions str 'internal-complete-buffer predicate))
                (and virtual
                     (ivy--virtual-buffers)))))

            (setq ivy-switch-buffer-faces-alist
                  '((emacs-lisp-mode . swiper-match-face-1)
                    (dired-mode . ivy-subdir)
                    (org-mode . org-level-4)))

            (use-package smex
              :ensure t
              :config (progn
                        (require 'smex)

                        (setq smex-save-file
                              (expand-file-name "/home/sandric/.smex-history"))

                        (smex-initialize)))

            (use-package counsel
              :ensure t
              :config (progn
                        (setq ivy-initial-inputs-alist '()))))
  :bind (("" . ivy-switch-buffer)
         ("" . counsel-M-x)

         :map ivy-minibuffer-map
         ("C-H-c" . sandric/open-ibuffer)
         ("s-f" . sandric/ivy-symbol)
         ("C-H-p" . sandric/ivy-symbol)
         ("s-x" . sandric/ivy-replace)))

(use-package swiper
  :ensure t
  :defer t
  :config (progn
            (use-package wgrep
              :ensure t))
  :bind (("C-f" . sandric/swiper-or-region)
         :map ivy-occur-grep-mode-map
         ("M-n" . nil)
         ("C-H-w" . ivy-wgrep-change-to-wgrep-mode)

         :map wgrep-mode-map
         ("S-C-H-r" . wgrep-finish-edit)))


(use-package counsel-dash
  :ensure t
  :config (progn
            (setq counsel-dash-common-docsets '("Emacs Lisp"))

            (setq counsel-dash-browser-func 'browse-dash-split-chrome)
            (add-hook 'emacs-lisp-mode-hook
                      (lambda () (setq-local counsel-dash-docsets '("Emacs Lisp"))))
            (add-hook 'ruby-mode-hook
                      (lambda () (setq-local counsel-dash-docsets '("Ruby" "Ruby on Rails"))))
            (add-hook 'js-mode-hook
                      (lambda () (setq-local counsel-dash-docsets '("JavaScript"))))
            (add-hook 'coffee-mode-hook
                      (lambda () (setq-local counsel-dash-docsets '("JavaScript"))))
            (add-hook 'css-mode-hook
                      (lambda () (setq-local counsel-dash-docsets '("Css"))))
            (add-hook 'typescript-mode-hook
                      (lambda () (setq-local counsel-dash-docsets '("TypeScript"))))))
