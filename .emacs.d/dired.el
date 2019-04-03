(use-package dired-toggle-sudo
  :ensure t)

(defun sandric/dired-jump-home ()
  "Dired jump home directory."
  (interactive)
  (dired "/home/sandric"))

(defun sandric/dired-jump-emacs ()
  "Dired jump emacs directory."
  (interactive)
  (dired "/home/sandric/.emacs.d"))

(defun sandric/dired-jump-projects ()
  "Dired jump projects directory."
  (interactive)
  (dired "/home/sandric/projects"))

(defun sandric/dired-jump-host ()
  "Dired jump host directory."
  (interactive)
  (dired "/home/sandric/host"))

(defun sandric/dired-jump-rbenv ()
  "Dired jump rbenv directory."
  (interactive)
  (dired (sandric/rbenv-gems-directory)))

(defun sandric/dired-jump-npm ()
  "Dired jump npm directory."
  (interactive)
  (dired "/home/sandric/.node_modules"))

(defun sandric/dired-jump-git ()
  "Dired jump git root directory."
  (interactive)
  (dired (magit-toplevel)))


(defun sandric/dired-file-markedp ()
  "Return true if current file is marked."
  (interactive)
  (let ((beginning (line-beginning-position)))
    (eq (char-after beginning) dired-marker-char)))

(defun sandric/dired-toggle-mark ()
  "Set mark if not set, remove mark otherwise."
  (interactive)
  (let ((current-file (dired-get-filename)))
    (if (sandric/dired-file-markedp)
        (dired-unmark current-file)
      (dired-mark current-file))))

(defun sandric/dired-kill-buffers ()
  "Kill all dired buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun sandric/dired-is-directory ()
  "Check if thing under cursor is directory."
  (interactive)
  (let ((find-file-run-dired t)
        (file (dired-get-file-for-visit)))
    (file-directory-p file)))

(defun sandric/dired-find-directory ()
  "A `dired-find-file' which only works on directories."
  (interactive)
  (let ((find-file-run-dired t)
        (file (dired-get-file-for-visit)))
    (when (file-directory-p file)
      (find-file file))))

(defun sandric/dired-find-file-or-directory ()
  "A `dired-find-file' which only works directories or files."
  (interactive)
  (let ((find-file-run-dired t)
        (file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (find-file file)
      (progn
        (dired-find-file-other-window)
        (sandric/dired-kill-buffers)))))

(defun sandric/dired-create-file (&optional file)
  "Create a file called FILE."
  (interactive
   (list (read-file-name "Create file: " (dired-current-directory))))
  (let* ((expanded (expand-file-name file))
         (try expanded)
         (dir (directory-file-name (file-name-directory expanded)))
         new)
    (if (file-exists-p expanded)
        (error "Cannot create file %s: file exists" expanded))
    (while (and try (not (file-exists-p try)) (not (equal new try)))
      (setq new try
            try (directory-file-name (file-name-directory try))))
    (when (not (file-exists-p dir))
      (make-directory dir t))
    (write-region "" nil expanded t)
    (when new
      (dired-add-file new)
      (dired-move-to-filename))))

(defun sandric/dired-setup ()
  (define-key dired-mode-map (kbd "<left>") 'dired-jump)
  (define-key dired-mode-map (kbd "<right>") 'sandric/dired-find-directory)

  (define-key dired-mode-map (kbd "M-A") 'sandric/dired-kill-buffers)

  (define-key dired-mode-map (kbd "<SPC>") 'sandric/dired-toggle-mark)
  (define-key dired-mode-map (kbd "A") '(lambda ()
                                          (interactive)
                                          (dired-mark-files-regexp "")))
  (define-key dired-mode-map (kbd "U") 'dired-unmark-all-marks)
  (define-key dired-mode-map (kbd "M") 'dired-do-move-recursive)
  (define-key dired-mode-map (kbd "N") 'sandric/dired-create-file)
  (define-key dired-mode-map (kbd "T") 'dired-create-directory)
  (define-key dired-mode-map (kbd "D") 'dired-do-delete)
  (define-key dired-mode-map (kbd "C") 'dired-do-copy)


  (require 'wdired)
  (setq wdired-allow-to-change-permissions t)
  (define-key dired-mode-map (kbd "M-W") 'wdired-change-to-wdired-mode)
  (define-key wdired-mode-map (kbd "M-R") 'wdired-finish-edit)
  (define-key wdired-mode-map (kbd "M-A") 'wdired-abort-changes)
  
  (hl-line-mode t))


(require 'dired-toggle-sudo)
(define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)
(eval-after-load 'tramp
  '(progn
     ;; Allow to use: /sudo:user@host:/path/to/file
     (add-to-list 'tramp-default-proxies-alist
                  '(".*" "\\`.+\\'" "/ssh:%h:"))))


(add-hook 'dired-mode-hook 'sandric/dired-setup)
