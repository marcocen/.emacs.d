(if (string-suffix-p ".fing.edu.uy" (system-name))
  (setq url-proxy-services
      '(("http"     . "proxy.fing.edu.uy:3128")
	("https"    . "proxy.fing.edu.uy:3128")
	("ftp"      . "proxy.fing.edu.uy:3128")
	("no_proxy" . "^.*fing.edu.uy"))))
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/local")
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

(require 'org)
(org-babel-load-file (expand-file-name "~/.emacs.d/emacs-init.org"))
