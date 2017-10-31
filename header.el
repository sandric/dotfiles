(defface header/directory '((t :inherit mode-line)) "" :group 'header-faces)
(defface header/name '((t :inherit mode-line :weight bold :foreground "blue")) "" :group 'header-faces)
(defface header/git '((t :inherit mode-line :foreground "blue")) "" :group 'header-faces)


(defun sandric/header-is-file-under-git ()
  "Check if current file under is git"
  (let ((file (buffer-file-name)))
    (if file
        (progn
          (require 'vc-git)
          (if (vc-git-responsible-p (buffer-file-name))
              ":G "
            " "))
      " ")))

(defun sandric/header-line-directory-path (window)
  "Trim directory if too long."
  (require 's)
  (let* ((window-length (window-total-width window))
         (buffer-length (length (buffer-name)))
         (directory-available-length (- window-length buffer-length 4)))
    (s-right directory-available-length (abbreviate-file-name default-directory))))

(defun sandric/header-line-whitespace (path-lenght window)
  "Calculate number of whitespace to place in header."
  (- (window-total-width window) path-lenght (length (buffer-name)) 4))

(defun sandric/header-line-format (window active)
  "Format for header line."
  (if (or (string= (buffer-name) "*fzf*")
          (string= (buffer-name) "*LV*"))
      (setq header-line-format nil)
    (let* ((directory-path (sandric/header-line-directory-path window))
           (directory-face (if active 'header/directory 'mode-line-inactive))
           (name-face (if active 'header/name 'mode-line-inactive)))
      (setq header-line-format (list (propertize (concat " "
                                                         directory-path)
                                                 'face directory-face)
                                     (propertize (sandric/header-is-file-under-git)
                                                 'face name-face)
                                     (propertize (apply 'concat
                                                        (make-list
                                                         (sandric/header-line-whitespace
                                                          (length directory-path)
                                                          window) " "))
                                                 'face name-face)
                                     (propertize (concat (buffer-name) " ")
                                                 'face name-face)))
      )))

(defun sandric/update-header ()
  "Update headers of all visible windows."
  (mapc
   (lambda (window)
     (with-current-buffer (window-buffer window)
       (sandric/header-line-format window nil)))
   (window-list)))

(defadvice select-window (after focus-header activate)
  (sandric/update-header)
  (sandric/header-line-format (selected-window) t))
