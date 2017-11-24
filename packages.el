(use-package regex-tool
  :ensure t
  :bind (:map regex-tool-mode-map
              ("<C-f4> a" . regex-tool-quit)))

(use-package w3m
  :ensure t
  :defer t
  :bind (:map w3m-mode-map
              ("<left>" . nil)
              ("<right>" . nil)
              ("<up>" . nil)
              ("<down>" . nil)
              ("<C-f4> a" . sandric/w3m-force-quit)))

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

(use-package nlinum
  :ensure t
  :config (progn))

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
         ("<C-f3> w" . undo-tree-visualize)
         ("C-y" . undo-tree-redo)))

(use-package multiple-cursors
  :ensure t
  :config (progn
            (setq mc/always-run-for-all t)
            )
  :bind (("<C-f2> s" . mc/mark-next-like-this)
         ("<C-f2> f" . mc/unmark-next-like-this)
         ("<C-f2> a" . mc/mark-all-like-this)
         :map mc/keymap
         ("ESC ESC ESC" . mc/keyboard-quit)))

(use-package smartparens
  :ensure t
  :config (progn (smartparens-global-mode)))

(use-package zygospore
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package sx
  :ensure t
  :config (progn
            (add-hook 'sx-question-list-mode
                      (lambda () (setq header-line-format nil))))
  :bind (:map sx-question-mode-map
              ("<left>" . nil)
              ("<right>" . nil)
              ("<up>" . previous-line)
              ("<down>" . next-line)))
