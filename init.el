;;; package --- Summary

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; don't get trojan horse'd
(setq enable-local-variables 'query)

;; disable startup screen
(setq inhibit-startup-message t)

;; line numbers
(global-linum-mode t)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; remove menu bar and tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; paren mode
(show-paren-mode t)

;; Highlight current line.
(global-hl-line-mode t)

;;cursor type bar
(setq-default cursor-type 'bar)

;; remove scroll bar
(scroll-bar-mode -1)

;; make vertical line separating windows thin
(window-divider-mode 0)

;; dont make backup files
(setq make-backup-files nil)

;; start as fullscreen
(toggle-frame-fullscreen)

;;default directory
(setq default-directory "~/")

;; turn off the goddamn bell
(setq ring-bell-function 'ignore)

(load-theme 'spacemacs-dark t)

;; always open read-only buffers in view-mode
(setq-default view-read-only t)

;; Move custom variables to a different file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages related setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

;; Add melpa package source when using package list
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                     ("melpa" . "https://melpa.org/packages/")))


;; fetch the list of packages available (not using now as no need to refresh unless some new package is to be installed)
;;(unless package-archive-content (package-refresh-contents))

;; define list of packages to install
(defvar yuvraj/myPackages
  '(magit
    spacemacs-theme
    use-package
    flycheck
    company
    treemacs
    haskell-mode
    elpy
    highlight-indent-guides
    ))

;; check if any new package needs to be installed
;; if yes, refresh the archive
(defun yuvraj/has-newpack (packlist)
   (if (null packlist)
    nil
     (let ((first (car packlist))
	    (rest (cdr packlist)))
       (if (not (package-installed-p first))
	 t
	 (yuvraj/has-newpack rest)
	 )
       )
     )
   )

(when (yuvraj/has-newpack yuvraj/myPackages)
  (package-refresh-contents)
  )

;; install all packages in list
(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      yuvraj/myPackages)

;; Python setup with elpy

(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-rpc-python-command "python3")
  ;; Enable flycheck and disable flymake with elpy
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

;; Highlight-indent-guides config

;; Enable highlight indent guides mode with prog mode
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; hig method set to character
(setq highlight-indent-guides-method 'character)

(provide 'init)
;;; init.el ends here
