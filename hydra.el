(use-package hydra
  :ensure t
  :config (progn

            (defhydra hydra-help (:exit t :hint nil)
              "Help"
              ("N" nil)
              ("T" nil)

              ("S" nil)
              ("A" sandric/counsel-dash-at-point)
              ("V" counsel-describe-variable)
              ("F" counsel-describe-function)
              ("M" describe-mode)
              ("K" describe-key))

            (setq hydra-help/hint
                  '(cond ((or (string= major-mode "js2-mode")
                              (string= major-mode "coffee-mode"))
                          (prog1 (eval
                                  (hydra--format nil '(:exit t)
                                                 (s-concat "\nF1: "
                                                           "[_N_]: type, "
                                                           "[_T_]: docs, "
                                                           "[_S_]: definition, "
                                                           "[_A_]: dash, "
                                                           "[_V_]: describe variable, "
                                                           "[_F_]: describe function, "
                                                           "[_M_]: describe mode, "
                                                           "[_K_]: describe key.")
                                                 hydra-help/heads))
                            (define-key hydra-help/keymap "N" 'tern-get-type)
                            (define-key hydra-help/keymap "T" 'tern-get-docs)
                            (define-key hydra-help/keymap "S" 'tern-find-definition)
                            (define-key hydra-help/keymap "A" 'sandric/counsel-dash-at-point)
                            (define-key hydra-help/keymap "V" 'counsel-describe-variable)
                            (define-key hydra-help/keymap "F" 'counsel-describe-function)
                            (define-key hydra-help/keymap "M" 'describe-mode)
                            (define-key hydra-help/keymap "K" 'describe-key)))
                         ((string= major-mode "ruby-mode")
                          (prog1 (eval
                                  (hydra--format nil '(:exit t)
                                                 (s-concat "\nF1: "
                                                           "[_T_]: docs, "
                                                           "[_S_]: definition, "
                                                           "[_A_]: dash, "
                                                           "[_M_]: describe mode, "
                                                           "[_K_]: describe key.")
                                                 hydra-help/heads))
                            (define-key hydra-help/keymap "N" nil)
                            (define-key hydra-help/keymap "T" 'robe-doc)
                            (define-key hydra-help/keymap "S" 'robe-jump)
                            (define-key hydra-help/keymap "A" 'sandric/counsel-dash-at-point)
                            (define-key hydra-help/keymap "V" nil)
                            (define-key hydra-help/keymap "F" nil)
                            (define-key hydra-help/keymap "M" 'describe-mode)
                            (define-key hydra-help/keymap "K" 'describe-key)))
                         (t (prog1 (eval
                                    (hydra--format nil '(:exit t)
                                                   (s-concat "\nF1: "
                                                             "[_S_]: find definition, "
                                                             "[_A_]: dash, "
                                                             "[_V_]: describe variable, "
                                                             "[_F_]: describe function, "
                                                             "[_M_]: describe mode, "
                                                             "[_K_]: describe key. ")
                                                   hydra-help/heads))
                              (define-key hydra-help/keymap "N" nil)
                              (define-key hydra-help/keymap "T" nil)
                              (define-key hydra-help/keymap "S" 'xref-find-definitions-other-window)
                              (define-key hydra-help/keymap "A" 'sandric/counsel-dash-at-point)
                              (define-key hydra-help/keymap "V" 'counsel-describe-variable)
                              (define-key hydra-help/keymap "F" 'counsel-describe-function)
                              (define-key hydra-help/keymap "M" 'describe-mode)
                              (define-key hydra-help/keymap "K" 'describe-key)))))

            (defhydra hydra-interact (:exit t :hint nil)
              "Interact"
              ("C" nil)
              ("T" nil)
              ("N" nil)
              ("R" nil)
              ("D" nil)
              ("L" flycheck-list-errors)
              ("S" nil))

            (setq hydra-interact/hint
                  '(cond ((string= major-mode "js2-mode")
                          (prog1 (eval
                                  (hydra--format nil '(:exit t)
                                                 (s-concat "\nInteract: "
                                                           "[_C_]: Connect to browser, "
                                                           "[_R_]: Repl, "
                                                           "[_D_]: Debugger, "
                                                           "[_L_]: Lint.")
                                                 hydra-interact/heads))
                            (define-key hydra-interact/keymap "C" 'jade-connect-to-chrome)
                            (define-key hydra-interact/keymap "R" (lambda ()
                                                                    (interactive)
                                                                    (sandric/switch-buffers-by-name-right-pane-regexp "JS REPL")))
                            (define-key hydra-interact/keymap "D" 'realgud:trepanjs)
                            (define-key hydra-interact/keymap "D" (lambda ()
                                                                    (interactive)
                                                                    (insert "\ndebugger")))
                            (define-key hydra-interact/keymap "L" 'flycheck-list-errors)
                            (define-key hydra-interact/keymap "S" (lambda ()
                                                                    (interactive)
                                                                    (sandric/select-right-pane)
                                                                    (find-file "~/test.js")))))
                         ((string= major-mode "coffee-mode")
                          (prog1 (eval
                                  (hydra--format nil '(:exit t)
                                                 (s-concat "\nInteract: "
                                                           "[_C_]: Compile, "
                                                           "[_R_]: Repl, "
                                                           "[_L_]: Lint.")
                                                 hydra-interact/heads))
                            (define-key hydra-interact/keymap "C" 'coffee-compile-buffer)
                            (define-key hydra-interact/keymap "T" nil)
                            (define-key hydra-interact/keymap "N" nil)
                            (define-key hydra-interact/keymap "R" 'coffee-repl)
                            (define-key hydra-interact/keymap "D" 'nil)
                            (define-key hydra-interact/keymap "L" 'flycheck-list-errors)))
                         ((string= major-mode "scss-mode")
                          (prog1 (eval
                                  (hydra--format nil '(:exit t)
                                                 (s-concat "\nInteract: "
                                                           "[_C_]: Compile, "
                                                           "[_L_]: Lint.")
                                                 hydra-interact/heads))
                            (define-key hydra-interact/keymap "C" 'scss-compile)
                            (define-key hydra-interact/keymap "T" nil)
                            (define-key hydra-interact/keymap "N" nil)
                            (define-key hydra-interact/keymap "R" nil)
                            (define-key hydra-interact/keymap "D" 'nil)
                            (define-key hydra-interact/keymap "L" 'flycheck-list-errors)))
                         ((string= major-mode "ruby-mode")
                          (prog1 (eval
                                  (hydra--format nil '(:exit t)
                                                 (s-concat "\nInteract: "
                                                           "[_T_]: Rubocop fix, "
                                                           "[_N_]: Rubycritic, "
                                                           "[_C_]: Intrarepl, "
                                                           "[_L_]: Lint.")
                                                 hydra-interact/heads))
                            (define-key hydra-interact/keymap "C" 'seeing-is-believing-run)
                            (define-key hydra-interact/keymap "T" 'rubocop-autocorrect-current-file)
                            (define-key hydra-interact/keymap "N" 'sandric/open-rubycritic)
                            (define-key hydra-interact/keymap "R" nil)
                            (define-key hydra-interact/keymap "D" 'nil)
                            (define-key hydra-interact/keymap "L" 'flycheck-list-errors)))
                         (t (prog1 (eval
                                    (hydra--format nil '(:exit t)
                                                   (s-concat "\nInteract: "
                                                             "[_R_]: Repl, "
                                                             "[_L_]: Lint.")
                                                   hydra-interact/heads))
                              (define-key hydra-interact/keymap "C" nil)
                              (define-key hydra-interact/keymap "T" nil)
                              (define-key hydra-interact/keymap "N" nil)
                              (define-key hydra-interact/keymap "R" 'ielm)
                              (define-key hydra-interact/keymap "D" 'nil)
                              (define-key hydra-interact/keymap "L" 'flycheck-list-errors)))))

            (defhydra hydra-scratch (:exit t)
              "Scratch"
              ("E" sandric/open-scratch-elisp "elisp")
              ("R" sandric/open-scratch-ruby "ruby")
              ("J" sandric/open-scratch-js "js")
              ("C" sandric/open-scratch-coffee "coffee")
              ("H" sandric/open-scratch-html "html"))

            (defhydra hydra-rg-deps (:exit t)
              "PT deps"
              ("C-H-S-t" sandric/pt-rbenv "rbenv")
              ("C-H-S-u" sandric/pt-npm "npm"))

            (defhydra hydra-fzf-deps (:exit t)
              "FZF deps"
              ("C-H-S-t" sandric/fzf-rbenv "rbenv")
              ("C-H-S-u" sandric/fzf-npm "npm"))

            (defhydra hydra-dired-deps (:exit t)
              "Dired deps"
              ("C-H-S-t" sandric/dired-jump-rbenv "rbenv")
              ("C-H-S-u" sandric/dired-jump-npm "npm"))


            (defhydra hydra-rg (:exit t)
              "PT"
              ("C-H-p" sandric/rg-or-region "current")
              ("C-H-S-r" sandric/rg-home "home")
              ("C-H-S-e" sandric/rg-emacs "emacs")
              ("C-H-S-u" sandric/rg-projects "projects")
              ("C-H-S-a" sandric/rg-git "git")
              ("C-H-S-s" hydra-rg-deps/body "deps"))

            (defhydra hydra-ag (:exit t)
              "AG"
              ("C-H-p" sandric/ag-or-region "current")
              ("C-H-S-r" sandric/ag-home "home")
              ("C-H-S-e" sandric/ag-emacs "emacs")
              ("C-H-S-u" sandric/ag-projects "projects")
              ("C-H-S-a" sandric/ag-git "git")
              ("C-H-S-s" hydra-pt-deps/body "deps"))

            (defhydra hydra-fzf (:exit t)
              "FZF"
              ("C-H-t" fzf "current")
              ("C-H-S-r" sandric/fzf-home "home")
              ("C-H-S-e" sandric/fzf-emacs "emacs")
              ("C-H-S-u" sandric/fzf-projects "projects")
              ("C-H-S-a" sandric/fzf-git "git")
              ("C-H-S-s" hydra-fzf-deps/body "deps"))

            (defhydra hydra-dired (:exit t)
              "Dired"
              ("C-H-S-v" dired-jump "current")
              ("C-H-S-r" sandric/dired-jump-home "home")
              ("C-H-S-e" sandric/dired-jump-emacs "emacs")
              ("C-H-S-u" sandric/dired-jump-projects "projects")
              ("C-H-S-a" sandric/dired-jump-git "git")
              ("C-H-S-s" hydra-dired-deps/body "deps"))


            (defhydra hydra-git (:exit t)
              "Git"
              ("C-H-f" magit-status "status")
              ("C-H-t" sandric/git-timemachine-in-right-pane "timemachine")
              ("C-H-S-q" magit-log-buffer-file "log")
              ("C-H-S-a" magit-blame "annotate"))



            (key-chord-define-global "ON" 'hydra-help/body)

            (key-chord-define-global "TA" 'hydra-interact/body)

            (key-chord-define-global "SS" 'hydra-scratch/body)

            (global-set-key (kbd "C-H-p") 'hydra-ag/body)
            (global-set-key (kbd "C-H-t") 'hydra-fzf/body)
            (global-set-key (kbd "S-C-H-v") 'hydra-dired/body)
            (global-set-key (kbd "C-H-f") 'hydra-git/body)
            ))
