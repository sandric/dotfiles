(defun sandric/eval-last-or-region-ruby (arg)
  "Evaluates last sexp or region if active in ruby mode."
  (interactive "P")
  (if (use-region-p)
      (ruby-send-region (region-beginning) (region-end))
    (ruby-send-last-sexp arg)))

(defun sandric/eval-last-or-region-js (beg end)
  "Evaluates last sexp or region if active js mode."
  (interactive "r")
  (if (use-region-p)
      (skewer-eval (buffer-substring beg end) #'skewer-post-minibuffer)
    (skewer-eval-last-expression beg)))

(use-package projectile-rails
  :ensure t)

(use-package fish-mode
  :ensure t)

(use-package evil-nerd-commenter
  :ensure t
  :bind (("M-c" . evilnc-comment-or-uncomment-lines)))

(use-package flycheck
  :ensure t
  :defer t
  :commands (flycheck-mode)
  :config (progn
            (setq-default flycheck-temp-prefix ".flycheck")
            (setq-default flycheck-disabled-checkers
                          (append flycheck-disabled-checkers
                                  '(json-jsonlist))))
  :init (progn
          ;; (global-flycheck-mode t)
          ))

(use-package restclient
  :ensure t
  :defer t
  :config (progn
            (define-key restclient-mode-map (kbd "C-x C-e")
              'restclient-http-send-current)))

(use-package emmet-mode
  :ensure t
  :defer t
  :init (progn (require 'emmet-mode)
               (add-hook 'sgml-mode-hook 'emmet-mode)
               (add-hook 'html-mode-hook 'emmet-mode)
               (add-hook 'css-mode-hook  'emmet-mode)

               (define-key emmet-mode-keymap (kbd "TAB") 'emmet-expand-line)))

(use-package rubocop
  :ensure t)

(use-package inf-ruby
  :ensure t
  :defer t
  :commands (inf-ruby-minor-mode)
  :init (progn
          (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
          (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)))

(use-package robe
  :ensure t
  ;; :defer t
  ;; :mode "\\.rb\\'"
  ;; :commands (inf-ruby-minor-mode)
  :config (progn
            (add-hook 'ruby-mode-hook 'robe-mode)))

(use-package ruby-electric
  :ensure t)

(use-package web-mode
  :ensure t
  :defer t
  :config (progn
            (require 'web-mode)

            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (setq web-mode-attr-indent-offset 2)

            (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))))

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :init (progn
          (setq exec-path (cons
                           (expand-file-name
                            "/home/sandric/.rbenv/shims/sass")
                           exec-path))))

(use-package yaml-mode
  :ensure t)

(use-package coffee-mode
  :ensure t)

(use-package indium
  :ensure t)

(use-package tern
  :ensure t
  :defer t
  :init (progn (add-hook 'js-mode-hook 'tern-mode))
  :commands (tern-mode))

(use-package alchemist
  :ensure t
  :config (progn
            (key-chord-define alchemist-mode-map "NN" 'sandric/alchemist-struct)
            (key-chord-define alchemist-mode-map "TT" 'sandric/alchemist-pipe)

            (setq alchemist-goto-elixir-source-dir "/home/sandric/elixir"))
  :bind (:map alchemist-mode-map
              ("M-r" . alchemist-iex-send-region-and-go)))

(use-package vue-mode
  :ensure t)

(use-package lua-mode
  :ensure t
  :config (progn
            (setq lua-indent-level 2)))
