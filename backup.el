;; Ivy pass
(use-package ivy-pass)

;; (defun yequake-org-capture (&optional goto keys)
;;   "Call `org-capture' in a Yequake frame.
;; Adds a function to `org-capture-after-finalize-hook' that closes
;; the recently toggled Yequake frame and removes itself from the
;; hook.
;; Note: if another Yequake frame is toggled before the capture is
;; finalized, when the capture is finalized, the wrong Yequake frame
;; will be toggled."
;;   (let* ((remove-hook-fn (lambda ()
;;                            (remove-hook 'org-capture-after-finalize-hook #'yequake-retoggle))))
;;     (add-hook 'org-capture-after-finalize-hook remove-hook-fn)
;;     (add-hook 'org-capture-after-finalize-hook #'yequake-retoggle)
;;     ;; MAYBE: Propose an `org-capture-switch-buffer-fn' variable that could be rebound here.

;;     ;; NOTE: We override `org-switch-to-buffer-other-window' because
;;     ;; it always uses `switch-to-buffer-other-window', and we want to
;;     ;; display the template menu and capture buffer in the existing
;;     ;; window rather than splitting the frame.
;;     (cl-letf* (((symbol-function #'org-switch-to-buffer-other-window)
;;                 (symbol-function #'switch-to-buffer)))
;;       (condition-case nil
;;           (progn
;;             (org-capture goto keys)
;;             ;; Be sure to return the "CAPTURE-" buffer, which is the current
;;             ;; buffer at this point.
;;             (current-buffer))
;;         ((error quit)
;;          ;; Capture aborted: remove the hook and hide the capture frame.
;;          (remove-hook 'org-capture-after-finalize-hook #'yequake-retoggle)
;;          (yequake-retoggle))))))

;; ;; Call emacs functionality from anywhere
;; (use-package yequake
;;   :custom
;;   (yequake-frames
;;    '(("org-capture"
;;       (buffer-fns . (yequake-org-capture))
;;       (width . 0.75)
;;       (height . 0.5)
;;       (alpha . 0.95)
;;       (frame-parameters . ((undecorated . t)
;;                            (skip-taskbar . t)
;;                            (sticky . t))))))
;;   :after
;;   (org))


(use-package elpy
  :init
  (elpy-enable))

(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)
              ("<S-iso-lefttab>" . dired-subtree-remove)))
;; DEPRECATED


;; treemacs
(use-package treemacs
  :disabled
  :bind
  (("C-c t" . treemacs-toggle)))
