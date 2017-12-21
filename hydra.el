(defun sandric/hydra-interact-description (arg)
  "Hydra interact description."
  (interactive "P")
  (cond ((string= major-mode "emacs-lisp-mode")
         (describe-symbol (or (symbol-at-point) (error "No symbol-at-point"))))
        ((string= major-mode "ruby-mode")
         (call-interactively 'robe-doc arg))
        ((string= major-mode "js-mode")
         (call-interactively 'sandric/counsel-dash-at-point arg))
        ((string= major-mode "term-mode")
         (call-interactively 'sandric/term-man arg))
        ((string= major-mode "elixir-mode")
         (call-interactively 'alchemist-help-search-at-point arg))))

(defun sandric/hydra-interact-definition (arg)
  "Hydra interact definition."
  (interactive "P")
  (cond ((string= major-mode "emacs-lisp-mode")
         (xref-find-definitions (thing-at-point 'symbol)))
        ((string= major-mode "ruby-mode")
         (call-interactively 'robe-jump arg))
        ((string= major-mode "js-mode")
         (call-interactively 'tern-find-definition arg))
        ((string= major-mode "term-mode")
         (call-interactively 'sandric/term-which arg))
        ((string= major-mode "elixir-mode")
         (call-interactively 'alchemist-goto-definition-at-point arg))))

(defun sandric/hydra-interact-scratch (arg)
  "Hydra interact scratch."
  (interactive "P")
  (cond ((string= major-mode "emacs-lisp-mode")
         (find-file "/home/sandric/scratch/scratch.el"))
        ((string= major-mode "ruby-mode")
         (find-file "/home/sandric/scratch/scratch.rb"))
        ((string= major-mode "js-mode")
         (find-file "/home/sandric/scratch/scratch.js"))
        ((string= major-mode "html-mode")
         (find-file "/home/sandric/scratch/scratch.html"))))

(defun sandric/hydra-interact-repl (arg)
  "Hydra interact repl."
  (interactive "P")
  (cond ((string= major-mode "emacs-lisp-mode")
         (ielm))
        ((string= major-mode "ruby-mode")
         (ruby-switch-to-inf arg))
        ((string= major-mode "js-mode")
         (find-file "/home/sandric/scratch/scratch.js"))))


(use-package hydra
  :ensure t
  :config (progn
            (setq hydra-lv nil)
            
            (defhydra hydra-interact (:exit t)
              "Interact"
              ("M-N" sandric/hydra-interact-definition "definition")
              ("M-E" sandric/hydra-interact-description "docs")
              ("M-R" sandric/hydra-interact-scratch "scratch")
              ("M-T" sandric/hydra-interact-repl "repl")
              ("M-S" flycheck-list-errors "lint"))

            (defhydra hydra-rg (:exit t)
              "PT"
              ("M-F" sandric/rg-or-region "current")
              ("M-A" sandric/rg-home "home")
              ("M-R" sandric/rg-emacs "emacs")
              ("M-S" sandric/rg-projects "projects")
              ("M-N" sandric/rg-host "host"))

            (defhydra hydra-fzf (:exit t)
              "FZF"
              ("M-T" sandric/fzf "current")
              ("M-A" sandric/fzf-home "home")
              ("M-R" sandric/fzf-emacs "emacs")
              ("M-S" sandric/fzf-projects "projects")
              ("M-N" sandric/fzf-host "host"))

            (defhydra hydra-dired (:exit t)
              "Dired"
              ("M-V" dired-jump "current")
              ("M-A" sandric/dired-jump-home "home")
              ("M-R" sandric/dired-jump-emacs "emacs")
              ("M-S" sandric/dired-jump-projects "projects")
              ("M-N" sandric/dired-jump-host "host"))


            (defhydra hydra-git (:exit t)
              "Git"
              ("M-W" magit-status "status")
              ("M-T" git-timemachine "timemachine")
              ("M-F" magit-log-buffer-file "log-file")
              ("M-P" magit-log-all "log")
              ("M-A" magit-blame "annotate"))


            (global-set-key (kbd "M-N") 'hydra-interact/body)
            (global-set-key (kbd "M-F") 'hydra-rg/body)
            (global-set-key (kbd "M-T") 'hydra-fzf/body)
            (global-set-key (kbd "M-V") 'hydra-dired/body)
            (global-set-key (kbd "M-W") 'hydra-git/body)
            ))
