(use-package key-chord
  :ensure t
  :config (progn
            (setq key-chord-in-macros t)

            (key-chord-define-global "WW" 'sandric/transpose-windows)
            (key-chord-define-global "EE" 'rectangle-mark-mode)
            (key-chord-define-global "OO" 'sandric/eval-last-or-region)

            (key-chord-define-global "GG" (lambda ()
                                            (interactive)
                                            (print major-mode)
                                            ))
            (key-chord-mode t)))
