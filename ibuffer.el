(defun ibuffer-mark-forward (start end arg)
  "Mark the buffers in the region, or ARG buffers.
If point is on a group name, this function operates on that group."
  (interactive (ibuffer-get-region-and-prefix))
  (ibuffer-mark-region-or-n-with-char start end arg ibuffer-marked-char))

(defun ibuffer-unmark-forward (start end arg)
  "Unmark the buffers in the region, or ARG buffers.
If point is on a group name, this function operates on that group."
  (interactive (ibuffer-get-region-and-prefix))
  (ibuffer-mark-region-or-n-with-char start end arg ?\s))

(defun sandric/ibuffer-toggle-mark-forward (start end arg)
  "Unmark the buffers in the region, or ARG buffers.
If point is on a group name, this function operates on that group."
  (interactive (ibuffer-get-region-and-prefix))
  (let ((mark (ibuffer-current-mark)))
    (if (eq mark ibuffer-marked-char)
        (ibuffer-mark-region-or-n-with-char start end arg ?\s)
      (ibuffer-mark-region-or-n-with-char start end arg ibuffer-marked-char))))

(defun sandric/ibuffer-unmark-all ()
  (interactive)
  (ibuffer-map-lines
   (lambda (_buf mark)
     (when (not (char-equal mark ?\s))
       (ibuffer-set-mark-1 ?\s))
     t))
  (ibuffer-redisplay t))

(defun sandric/ibuffer-mark-all ()
  (interactive)
  (sandric/ibuffer-unmark-all)
  (ibuffer-toggle-marks))

(defun sandric/open-ibuffer ()
  "Open ibuffer from ido-switch-buffer"
  (interactive)
  (sandric/minibuffer-quit-and-run (ibuffer-other-window)))


(defun sandric/ibuffer-mode-setup ()
  "Open ibuffer from ido-switch-buffer"
  (interactive)
  (define-key ibuffer-mode-map (kbd "C-g") 'quit-window)
  (define-key ibuffer-mode-map (kbd "M") 'sandric/ibuffer-toggle-mark-forward)
  (define-key ibuffer-mode-map (kbd "U") 'sandric/my-ibuffer-unmark-all)
  (define-key ibuffer-mode-map (kbd "A") 'sandric/my-ibuffer-mark-all))


;; (require 'ibuffer)

(add-hook 'ibuffer-mode-hook 'sandric/ibuffer-mode-setup)
