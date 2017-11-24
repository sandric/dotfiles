(defun sandric/fzf (&optional directory)
  "Starts a fzf session in directory."
  (interactive)
  (let ((default-directory (or directory (magit-toplevel) default-directory)))
    (counsel-fzf)))

(defun sandric/fzf-home ()
  "Starts a fzf session in home directory."
  (interactive)
  (sandric/fzf "/home/sandric"))

(defun sandric/fzf-emacs ()
  "Starts a fzf session in emacs directory."
  (interactive)
  (sandric/fzf "/home/sandric/.emacs.d"))

(defun sandric/fzf-projects ()
  "Starts a fzf session in projects directory."
  (interactive)
  (sandric/fzf "/home/sandric/projects"))

(defun sandric/docset-dir ()
  (interactive)
  (counsel-fzf "/home/sandric/.docsets"))

(defun sandric/fzf-git ()
  "Starts a fzf session in git directory."
  (interactive)
  (sandric/fzf (magit-toplevel)))

(defun sandric/fzf-host ()
  "Starts a fzf session in host directory."
  (interactive)
  (sandric/fzf "/home/sandric/host"))
