(setq fzf/executable-fzf "/home/sandric/fzf/fzf.sh")
(setq fzf/executable-fzd "/home/sandric/fzf/fzd.sh")

(setq fzf/selection-file "/home/sandric/fzf/.fzf_selection")

(setq fzf/window-height 15)

(setq fzf/current-directory "/home/sandric/")



(defun fzf/read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun fzf/fullpath (path)
  (concat fzf/current-directory path))



(defun fzf/stop-fzf (process-name msg)
  (let* ((selected-paths (fzf/read-lines fzf/selection-file))
         (selected-paths-length (list-length selected-paths)))
    (kill-buffer "*fzf*")
    (jump-to-register :fzf-windows)
    (when (eq selected-paths-length 1)
      (find-file  (fzf/fullpath (s-concat "/" (car selected-paths)))))
    (when (> selected-paths-length 1)
      (dired
       (cons
        "FZF results"
        (mapcar 'fzf/fullpath selected-paths)))))
  (advice-remove 'term-handle-exit #'fzf/stop-fzf))

(defun fzf/stop-fzd (process-name msg)
  (let* ((selected-paths (fzf/read-lines fzf/selection-file))
         (selected-paths-length (list-length selected-paths)))
    (kill-buffer "*fzf*")
    (jump-to-register :fzf-windows)

    (when (eq selected-paths-length 1)
      (dired (fzf/fullpath (car selected-paths))))
    (when (> selected-paths-length 1)
      (dired
       (cons
        "FZF results"
        (mapcar 'fzf/fullpath selected-paths)))))
  (advice-remove 'term-handle-exit #'fzf/stop-fzd))



(defun fzf/start-fzf (directory)
  (let ((default-directory directory))
    (require 'term)
    (setq fzf/current-directory default-directory)
    (window-configuration-to-register :fzf-windows)
    (advice-add 'term-handle-exit :after #'fzf/stop-fzf)
    (let ((buf (get-buffer-create "*fzf*")))
      (split-window-vertically fzf/window-height)
      (make-term "fzf" fzf/executable-fzf)
      (switch-to-buffer buf)

      (setq-local scroll-margin 0)
      (setq-local scroll-conservatively 0)
      (setq-local term-suppress-hard-newline t)
      (face-remap-add-relative 'mode-line '(:box nil))

      (term-char-mode)
      (setq mode-line-format (format "   FZF  %s" directory)))))

(defun fzf/start-fzd (directory)
  (let ((default-directory directory))
    (require 'term)
    (setq fzf/current-directory default-directory)
    (window-configuration-to-register :fzf-windows)
    (advice-add 'term-handle-exit :after #'fzf/stop-fzd)
    (let ((buf (get-buffer-create "*fzf*")))
      (split-window-vertically fzf/window-height)
      (make-term "fzf" fzf/executable-fzd)
      (switch-to-buffer buf)

      (setq-local scroll-margin 0)
      (setq-local scroll-conservatively 0)
      (setq-local term-suppress-hard-newline t)
      (face-remap-add-relative 'mode-line '(:box nil))

      (term-char-mode)
      (setq mode-line-format (format "   FZF  %s" directory)))))



;;;###autoload
(defun fzf ()
  "Starts a fzf session."
  (interactive)
  (if (fboundp #'projectile-project-root)
      (fzf/start-fzf (condition-case err
                         (projectile-project-root)
                       (error
                        default-directory)))
    (fzf/start-fzf default-directory)))

;;;###autoload
(defun sandric/fzf-home ()
  "Starts a fzf session at the home directory."
  (interactive)
  (fzf/start-fzf "/home/sandric"))

;;;###autoload
(defun sandric/fzf-emacs ()
  "Starts a fzf session at the emacs directory."
  (interactive)
  (fzf/start-fzf "/home/sandric/.emacs.d"))

;;;###autoload
(defun sandric/fzf-projects ()
  "Starts a fzf session at the projects directory."
  (interactive)
  (fzf/start-fzf "/home/sandric/projects"))

;;;###autoload
(defun sandric/fzf-rbenv ()
  "Starts a fzf session at the rbenv directory."
  (interactive)
  (fzf/start-fzf (sandric/rbenv-gems-directory)))

;;;###autoload
(defun sandric/fzf-npm ()
  "Starts a fzf session at the npm directory."
  (interactive)
  (fzf/start-fzf "/home/sandric/.node_modules"))

;;;###autoload
(defun sandric/fzf-git ()
  "Starts a fzf session at the git directory."
  (interactive)
  (fzf/start-fzf (magit-toplevel)))




;;;###autoload
(defun fzf-directory (directory)
  "Starts a fzf session at the specified directory."
  (interactive "D")
  (fzf/start-fzf directory))



;;;###autoload
(defun fzd ()
  "Starts a fzd session."
  (interactive)
  (if (fboundp #'projectile-project-root)
      (fzf/start-fzd (condition-case err
                         (projectile-project-root)
                       (error
                        default-directory)))
    (fzf/start-fzd default-directory)))

;;;###autoload
(defun fzd-home ()
  "Starts a fzd session at the home directory."
  (interactive)
  (fzf/start-fzd "/home/sandric/"))

;;;###autoload
(defun fzd-directory (directory)
  "Starts a fzd session at the specified directory."
  (interactive "D")
  (fzf/start-fzd directory))


(provide 'fzf)
