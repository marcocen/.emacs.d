
;; Set separate custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Set theme
(load-theme 'tango-dark)

;; Load Sensible Defaults
(load-file "local/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

;; Personal Info
(setq user-full-name "Marco Centurión Virdó"
      user-mail-address "mcenturion@fing.edu.uy"
      calendar-location-name "Montevideo, UY")

;; Various interface tweaks
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(display-time-mode 1)
(setq display-time-default-load-average nil)
(column-number-mode t)
(show-paren-mode 1)
(electric-pair-mode 1)

;; Use graphic clipboard to copy/paste
(use-package xclip
  :config
  (xclip-mode 1))
;; nlinum is more performant than linum
(use-package nlinum
  :config
  (global-nlinum-mode t)
  :init
  (add-hook 'doc-view-mode-hook (lambda () (nlinum-mode 0)))
  (add-hook 'term-mode-hook (lambda () (nlinum-mode 0))))



;; magit config
(use-package magit
  :bind
  (("C-c m" . magit)))

;; yasnippets
(use-package yasnippet
  :init
  (yas-global-mode))

(use-package yasnippet-snippets
  :after
  (yasnippet))

;; Puppet
(use-package puppet-mode)

;; Ivy pass
(use-package ivy-pass)

;; org
(use-package org
  :custom
  (org-capture-templates '(("t" "Todo" entry (file+headline "~/Documents/TODO/todo.org" "Tasks")
			    "* TODO %?\n")
			   ("j" "Journal" entry (file+datetree "~/org/journal.org")
			    "* %?\nEntered on %U\n  %i"))))
;; org-mode
(use-package ox-hugo
  :after
  (org))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode)
  :after
  (org))



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

;; Use ivy
(use-package ivy
  :init
  (ivy-mode 1)
  :bind
  (:map ivy-minibuffer-map
	("C-m" . ivy-alt-done)
	("C-j" . ivy-alt-done)))
(use-package counsel
  :after
  (ivy))

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
