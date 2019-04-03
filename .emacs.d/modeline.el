(defface mode-line-modified
  '((t :weight bold :foreground "white" :background "red"))
  ""
  :group 'modeline-faces)

(defface mode-line-not-modified
  '((t :weight bold :foreground "blue"))
  ""
  :group 'modeline-faces)

(defface mode-line-rocket
  '((t :weight bold :foreground "blue"))
  ""
  :group 'modeline-faces)

(defface mode-line-term-active
  '((t :weight bold :foreground "white" :background "blue"))
  ""
  :group 'modeline-faces)

(defface mode-line-term-inactive
  '((t :foreground "black" :background "white"))
  ""
  :group 'modeline-faces)

(defface mode-line-term-active-line-mode
  '((t :weight bold :foreground "white" :background "red"))
  ""
  :group 'modeline-faces)


(custom-set-faces
 '(mode-line ((t (:background "#F097AD":foreground "black"))))
 '(mode-line-inactive ((t
                        (:inherit mode-line
                                  :background "grey90"
                                  :foreground "grey20"
                                  :weight light)))))


(defun sandric/modeline-render (left center right &optional lpad rpad)
  "Return a string the width of the current window with
LEFT, CENTER, and RIGHT spaced out accordingly, LPAD and RPAD,
can be used to add a number of spaces to the front and back of the string."
  (condition-case err
      (let* ((left (if lpad (concat (make-string lpad ?\s) left) left))
             (right (if rpad (concat right (make-string rpad ?\s)) right))
             (width (apply '+ (window-width) (let ((m (window-margins))) (list (or (car m) 0) (or (cdr m) 0)))))
             (total-length (+ (length left) (length center) (length right) 2)))
        (when (> total-length width) (setq left "" right ""))
        (let* ((left-space (/ (- width (length center)) 2))
               (right-space (- width left-space (length center)))
               (lspaces (max (- left-space (length left)) 1))
               (rspaces (max (- right-space (length right)) 1 0)))
          (concat left (make-string lspaces  ?\s)
                  center
                  (make-string rspaces ?\s)
                  right)))
    (error (format "[%s]: (%s) (%s) (%s)" err left center right))))

(defun sandric/flycheck-mode-line-status-text (&optional status)
  "Customizations to flycheck mode line status."
  (pcase (or status flycheck-last-status-change)
    (`not-checked "")
    (`no-checker "-")
    (`running "*")
    (`errored "!")
    (`finished
     (let-alist (flycheck-count-errors flycheck-current-errors)
       (if (or .error .warning)
           (format "%s/%s" (or .error 0) (or .warning 0))
         "")))
    (`interrupted "-")
    (`suspicious "?")))


(defun sandric/term-list-names ()
  "."
  (interactive)
  (mapcar (lambda (buffer)
            (if (string= (buffer-name) (buffer-name buffer))
                (if (term-in-line-mode)
                    (propertize (s-wrap (sandric/term-format-name (buffer-name buffer)) " ")
                                'face
                                'mode-line-term-active-line-mode)
                  (propertize (s-wrap (sandric/term-format-name (buffer-name buffer)) " ")
                              'face
                              'mode-line-term-active))
              (propertize (s-wrap (sandric/term-format-name (buffer-name buffer)) " ")
                          'face
                          'mode-line-term-inactive)))
          multi-term-buffer-list))

(defun sandric/mode-line-left ()
  "Mode line format for left margin"
  (format-mode-line
   (list
    " "
    '(:eval (if (buffer-modified-p)
                (propertize "M"
                            'face
                            'mode-line-modified)
              (propertize "S "
                          'face
                          'mode-line-not-modified)))
    " "
    (propertize "%l" 'face 'italic)
    ":"
    (propertize "%c" 'face 'normal)
    " "
    (format "%3d%%" (/ (window-end) 0.01 (point-max))))))

(defun sandric/mode-line-center ()
  "Mode line format for center margin"
  (format-mode-line
   (list
    (propertize "* " 'face 'mode-line-rocket)

    '(:eval (if (string= "term-mode" major-mode)
                (sandric/term-list-names)
              (propertize "%b")))
    
    (propertize " *" 'face 'mode-line-rocket))))

(defun sandric/mode-line-right ()
  "Mode line format for right margin"
  (format-mode-line
   (list
    (propertize "* " 'face 'mode-line-rocket)
    (propertize "%m")
    (propertize " *" 'face 'mode-line-rocket)
    minor-mode-alist
    (propertize " *" 'face 'mode-line-rocket))))

(setq-default mode-line-format
              '((:eval (sandric/modeline-render
                        (sandric/mode-line-left)
                        (sandric/mode-line-center)
                        (sandric/mode-line-right)
                        1 1))))

(defvar mode-line-cleaner-alist
  `((smartparens-mode . "")
    (global-undo-tree-mode . "")
    (undo-tree-mode . "")
    (undo-tree-minor-mode . "")
    (whitespace-mode . "")
    (flycheck-mode . " f")
    (eldoc-mode . "")
    (hi-lock-mode . "")
    (highlight-symbol-mode . "")
    (auto-revert-mode . "")

    (tern-mode . " t")
    (emmet-mode . " e")
    (company-mode . " c")
    (yas-minor-mode . " γσ")
    (aggressive-indent-mode . " αι")
    (robe-mode . " r")

    (lisp-interaction-mode . "λ")
    (python-mode . "Py")
    (emacs-lisp-mode . "ελ")
    (ruby-mode . "R")
    (java-script-mode . "JS")
    (js-mode . "JS")
    (nxhtml-mode . "nx")
    (term-mode . "T")))

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner))
                  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)
