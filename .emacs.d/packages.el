(use-package regex-tool
  :ensure t
  :bind (:map regex-tool-mode-map
              ("<C-f4> a" . regex-tool-quit)))

(use-package emamux
  :ensure t
  :defer t)

(use-package command-log-mode
  :ensure t
  :defer t)

(use-package xterm-color
  :ensure t
  :defer t)

(use-package esup
  :ensure t
  :defer t)

(use-package exec-path-from-shell
  :ensure t)

(use-package s
  :ensure t
  :defer t)

(use-package f
  :ensure t
  :defer t)

(use-package async
  :ensure t
  :defer t)

(use-package auto-compile
  :ensure t
  :config (progn
            (require 'auto-compile)
            (auto-compile-on-load-mode)
            (auto-compile-on-save-mode)))

(use-package rainbow-delimiters
  :ensure t
  :commands (rainbow-delimiters-mode)
  :init (progn
          (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package rainbow-mode
  :ensure t
  :defer t)

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
  :config (setq
           (progn undo-tree-mode-lighter " Undo-Tree")
           (global-undo-tree-mode))
  :bind (("C-z" . undo-tree-undo)
         ("C-y" . undo-tree-redo)))

(use-package multiple-cursors
  :ensure t
  :config (progn
            (setq mc/always-run-for-all 1)
            (setq mc/always-repeat-command t))
  
  :bind (("<C-f10>" . mc/mark-next-like-this)
         ("<C-f11>" . mc/unmark-next-like-this)))

(use-package smartparens
  :ensure t
  :config (progn (smartparens-global-mode)))

(use-package markdown-mode
  :ensure t)
