(defun sandric/rg-or-region (beg end &optional directory)
  "RG region or 'empty string' if none highlighted."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let ((default-directory (or directory default-directory)))
    (if (and beg end)
        (progn
          (deactivate-mark)
          (setq selection (buffer-substring-no-properties beg end))
          (counsel-rg selection))
      (counsel-rg))))



(defun sandric/rg-home (beg end)
  "RG in home directory."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (sandric/rg-or-region beg end "/home/sandric"))

(defun sandric/rg-emacs (beg end)
  "RG in Emacs directory."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (sandric/rg-or-region beg end "/home/sandric/.emacs.d"))

(defun sandric/rg-projects (beg end)
  "RG in projects directory."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (sandric/rg-or-region beg end "/home/sandric/projects"))

(defun sandric/rg-rbenv (beg end)
  "RG in rbenv directory."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (sandric/rg-or-region beg end (sandric/rbenv-gems-directory)))

(defun sandric/rg-npm (beg end)
  "RG in npm directory."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (sandric/rg-or-region beg end "/home/sandric/.node_modules"))

(defun sandric/rg-git (beg end)
  "RG in git root."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (sandric/rg-or-region beg end (magit-toplevel)))

(defun sandric/rg-host (beg end)
  "RG in host directory."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (sandric/rg-or-region beg end "/home/sandric/host"))
