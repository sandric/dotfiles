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
    ;; Find the topmost nonexistent parent dir (variable `new')
    (while (and try (not (file-exists-p try)) (not (equal new try)))
      (setq new try
            try (directory-file-name (file-name-directory try))))
    (when (not (file-exists-p dir))
      (make-directory dir t))
    (write-region "" nil expanded t)
    (when new
      (dired-add-file new)
      (dired-move-to-filename))))


(defun sandric/dired-transfer-to-narrow-symbol ()
  "Transfer inserted symbol in dired mode to dired-narrow mode."
  (interactive)
  (unwind-protect
      (minibuffer-keyboard-quit)
    (run-at-time nil nil (lambda ()
                           (self-insert-command 1)
                           (dired-narrow--live-update)))
    (dired-narrow)))

(defun sandric/dired-transfer-to-narrow-latin-symbols ()
  (loop for c from ?a to ?z
        do (define-key dired-mode-map (kbd (string c))
             'sandric/dired-transfer-to-narrow-symbol)))

(defun sandric/dired-transfer-to-narrow-numbers ()
  (loop for c from ?0 to ?9
        do (define-key dired-mode-map (kbd (string c))
             'sandric/dired-transfer-to-narrow-symbol)))

(defun sandric/dired-narrow-execute-function (func &rest arg)
  "Execute function in dired buffer from narrow."
  (interactive)
  (with-current-buffer dired-narrow-buffer
    (select-window (get-buffer-window (current-buffer)))
    (let ((result (apply func arg)))
      (sandric/select-minibuffer)
      result)))



(defun sandric/ediff-setup ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map (kbd "<down>") 'ediff-next-difference)
  (define-key ediff-mode-map (kbd "<up>") 'ediff-previous-difference))

(defun sandric/dired-setup ()
  (hl-line-mode t))


(use-package dired
  :defer t
  :init (progn
          (custom-set-variables
           '(dired-hide-details-hide-information-lines nil)
           '(diredp-hide-details-initially-flag nil)))

  :config (progn

            (use-package dired+
              :ensure t
              :config (progn
                        (require 'wdired)
                        (setq wdired-allow-to-change-permissions t)

                        (define-key dired-mode-map (kbd "C-H-w") 'wdired-change-to-wdired-mode)
                        (define-key wdired-mode-map (kbd "S-C-H-r") 'wdired-finish-edit)
                        (define-key wdired-mode-map (kbd "S-C-H-a") 'wdired-abort-changes)))

            (use-package dired-narrow
              :ensure t
              :config (progn
                        (require 'dired-narrow)

                        (advice-add 'dired :after #'revert-buffer)

                        (defun dired-narrow--internal (filter-function)
                          "My narrow internal substitution."
                          (let ((dired-narrow-buffer (current-buffer))
                                (dired-narrow-filter-function filter-function)
                                (disable-narrow nil))
                            (unwind-protect
                                (progn
                                  (dired-narrow-mode 1)
                                  (add-to-invisibility-spec :dired-narrow)
                                  (setq disable-narrow (read-from-minibuffer "Filter: " nil dired-narrow-map))
                                  (unless (dired-utils-goto-line dired-narrow--current-file)
                                    (setq dired-narrow--current-file nil)))
                              (dired-narrow-mode -1))))

                        (defun dired-narrow--live-update ()
                          "My narrow update substitution."
                          (when dired-narrow-buffer
                            (let ((current-filter (minibuffer-contents-no-properties)))
                              (with-current-buffer dired-narrow-buffer
                                (unless (equal current-filter dired-narrow--minibuffer-content)
                                  (dired-narrow--update current-filter))
                                (setq dired-narrow--minibuffer-content current-filter)
                                (setq dired-narrow--current-file (dired-utils-get-filename))
                                (set-window-point (get-buffer-window (current-buffer)) (point))

                                (when (bound-and-true-p hl-line-mode)
                                  (hl-line-highlight))))))))



            (define-key dired-mode-map "A" '(lambda ()
                                              (interactive)
                                              (dired-mark-files-regexp "")))


            (define-key dired-narrow-map (kbd "A")
              '(lambda ()
                 (interactive)
                 (sandric/dired-narrow-execute-function
                  'dired-mark-files-regexp "")))

            (define-key dired-narrow-map (kbd "U")
              '(lambda ()
                 (interactive)
                 (sandric/dired-narrow-execute-function
                  'dired-unmark-all-marks)))

            (define-key dired-narrow-map (kbd "M")
              '(lambda ()
                 (interactive)
                 (sandric/dired-narrow-execute-function
                  'sandric/dired-toggle-mark)))

            (define-key dired-narrow-map (kbd "<RET>")
              '(lambda ()
                 (interactive)
                 (sandric/minibuffer-quit-and-run
                  (call-interactively
                   'sandric/dired-find-file-or-directory))))

            (define-key dired-narrow-map (kbd "L")
              '(lambda ()
                 (interactive)
                 (sandric/minibuffer-quit-and-run
                  (call-interactively
                   'dired-find-file-other-window))))

            (define-key dired-narrow-map (kbd "Y")
              '(lambda ()
                 (interactive)
                 (sandric/minibuffer-quit-and-run
                  (call-interactively
                   'dired-find-file))))

            (define-key dired-narrow-map (kbd "N")
              '(lambda ()
                 (interactive)
                 (sandric/minibuffer-quit-and-run
                  (call-interactively
                   'sandric/dired-create-file))))

            (define-key dired-narrow-map (kbd "T")
              '(lambda ()
                 (interactive)
                 (sandric/minibuffer-quit-and-run
                  (call-interactively
                   'sandric/switch-to-existing-term-or-create))))


            (define-key dired-narrow-map (kbd "S")
              '(lambda ()
                 (interactive)
                 (sandric/minibuffer-quit-and-run
                  (call-interactively
                   'dired-create-directory))))

            (define-key dired-narrow-map (kbd "D")
              '(lambda ()
                 (interactive)
                 (sandric/minibuffer-quit-and-run
                  (dired-do-delete))))

            (define-key dired-narrow-map (kbd "C")
              '(lambda ()
                 (interactive)
                 (sandric/minibuffer-quit-and-run
                  (dired-do-copy))))

            (define-key dired-narrow-map (kbd "C-H-w")
              '(lambda ()
                 (interactive)
                 (sandric/minibuffer-quit-and-run
                  (revert-buffer)
                  (wdired-change-to-wdired-mode))))

            (define-key dired-narrow-map (kbd "S-C-H-v")
              '(lambda ()
                 (interactive)
                 (sandric/minibuffer-quit-and-run
                  (sandric/dired-kill-buffers))))

            (define-key dired-narrow-map (kbd "<S-left>")
              '(lambda ()
                 (interactive)
                 (sandric/minibuffer-quit-and-run
                  (dired-jump))))

            (define-key dired-narrow-map (kbd "<S-right>")
              '(lambda ()
                 (interactive)
                 (when (sandric/dired-narrow-execute-function
                        'sandric/dired-is-directory)
                   (sandric/minibuffer-quit-and-run
                    (call-interactively
                     'sandric/dired-find-directory)))))


            (sandric/dired-transfer-to-narrow-latin-symbols)
            (sandric/dired-transfer-to-narrow-numbers)
            (define-key dired-mode-map (kbd ".")
              'sandric/dired-transfer-to-narrow-symbol))

  :bind (:map dired-mode-map
              ("N" . dired-narrow)
              ("M" . sandric/dired-toggle-mark)
              ("<RET>" . sandric/dired-find-file-or-directory)
              ("L" . dired-find-file-other-window)
              ("Y" . dired-find-file)
              ("T" . sandric/dired-create-file)
              ("S" . dired-create-directory)
              ("D" . dired-do-delete)
              ("C" . dired-do-copy)
              ("C-g" . revert-buffer)
              ("s-f" . swiper)
              ("S-C-H-v" . sandric/dired-kill-buffers)
              ("<S-left>" . dired-jump)
              ("<S-right>" . sandric/dired-find-directory)
              ("M-i" . nil)))



(add-hook 'ediff-after-quit-hook-internal 'winner-undo)
(add-hook 'ediff-mode-hook 'sandric/ediff-setup)

(add-hook 'dired-mode-hook 'sandric/dired-setup)

(advice-add 'dired :before 'sandric/select-right-pane)
