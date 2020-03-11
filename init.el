(setq url-proxy-services
      '(("http"     . "proxy.fing.edu.uy:3128")
	("https"    . "proxy.fing.edu.uy:3128")
	("ftp"      . "proxy.fing.edu.uy:3128")
	("no_proxy" . "^.*fing.edu.uy")))
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Ensure that use-package is installed.
;;
;; If use-package isn't already installed, it's extremely likely that this is a
;; fresh installation! So we'll want to update the package repository and
;; install use-package before loading the literate configuration.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

;; Set separate custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Set theme
(load-theme 'tango-dark)

;; Load Sensible Defaults
(load-file "~/.emacs.d/sensible-defaults.el")
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

;; treemacs
(use-package treemacs
  :bind
  (("C-c t" . treemacs-toggle)))

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

;; org-mode
(use-package ox-hugo
  :after
  (org))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode)
  :after
  (org))

(use-package org
  :custom
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "~/Documents/TODO/todo.org" "Tasks")
	   "* TODO %?\n")
	  ("j" "Journal" entry (file+datetree "~/org/journal.org")
	   "* %?\nEntered on %U\n  %i"))))

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
