(if (string-suffix-p ".fing.edu.uy" (system-name))
  (setq url-proxy-services
      '(("http"     . "proxy.fing.edu.uy:3128")
	("https"    . "proxy.fing.edu.uy:3128")
	("ftp"      . "proxy.fing.edu.uy:3128")
	("no_proxy" . "^.*fing.edu.uy"))))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(straight-use-package '(use-package :type git :host github :repo
 "jwiegley/use-package"))

(add-to-list 'load-path "~/.emacs.d/local")

(use-package auto-compile
  :straight t
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

(require 'org)
(org-babel-load-file (expand-file-name "~/.emacs.d/emacs-init.org"))
