(use-package esup
  :ensure t
  :defer t)

(use-package simpleclip
  :ensure t
  :defer t
  :config (progn
            (simpleclip-mode 1))
  :bind (("M-6" . simpleclip-copy)
         ("M-8" . simpleclip-cut)
         ("M-7" . simpleclip-paste)))

(use-package s
  :ensure t
  :defer t)

(use-package f
  :ensure t
  :defer t)

(use-package async
  :ensure t
  :defer t)

(use-package shackle
  :ensure t
  :config (progn
            (setq shackle-rules
                  '(
                    (help-mode :custom sandric/shackle-in-right-pane)
                    ("\\`\\*Customize.*?\\*\\'" :regexp t :custom sandric/shackle-in-right-pane)
                    (magit-status-mode :custom sandric/shackle-in-right-pane)
                    ("*Ibuffer*" :align below :ratio 0.3)
                    (jade-repl-mode :custom sandric/shackle-in-right-pane)
                    (nodejs-repl-mode :custom sandric/shackle-in-right-pane)
                    (inf-ruby-mode :custom sandric/shackle-in-right-pane)
                    (comint-mode :custom sandric/shackle-in-right-pane)
                    ))
            (shackle-mode t)))

(use-package auto-compile
  :ensure t
  :config (progn
            (require 'auto-compile)
            (auto-compile-on-load-mode)
            (auto-compile-on-save-mode)))

(use-package circe
  :ensure t
  :defer t)

(use-package rainbow-delimiters
  :ensure t
  :commands (rainbow-delimiters-mode)
  :init (progn
          (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package rainbow-mode
  :ensure t
  :defer t)

(use-package highlight-symbol
  :ensure t
  :config (progn
            (setq highlight-symbol-idle-delay 0.2)
            (add-hook 'prog-mode-hook 'highlight-symbol-mode)))

(use-package window-numbering
  :ensure t
  :config (progn
            (setq window-numbering-assign-func
                  (lambda () (when (equal (buffer-name) "*fzf*") 3)))

            (setq window-numbering-mode-line-position 2)
            (window-numbering-mode t)))

(use-package aggressive-indent
  :ensure t
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
          (add-hook 'ruby-mode-hook 'aggressive-indent-mode)
          (add-hook 'js-mode-hook 'aggressive-indent-mode)
          (add-hook 'css-mode-hook 'aggressive-indent-mode)
          (add-hook 'sass-mode-hook 'aggressive-indent-mode)
          (add-hook 'html-mode-hook 'aggressive-indent-mode)
          (add-hook 'slim-mode-hook 'aggressive-indent-mode)))

(use-package undo-tree
  :ensure t
  :config (progn
            (setq undo-tree-mode-lighter " Undo-Tree")
            (global-undo-tree-mode))
  :bind (("C-z" . undo-tree-undo)
         ("" . undo-tree-visualize)
         ("C-y" . undo-tree-redo)))

(use-package expand-region
  :ensure t
  :bind (("C-S-s-t" . er/expand-region)
         ("s-Y" . er/contract-region)))

(use-package multiple-cursors
  :ensure t
  :config (progn
            (setq mc/always-run-for-all t)
            )
  :bind (("C-H-e" . mc/mark-next-like-this)
         ("C-H-u" . mc/unmark-next-like-this)))

(use-package smartparens
  :ensure t
  :config (progn (smartparens-global-mode)))

(use-package zygospore
  :ensure t)
