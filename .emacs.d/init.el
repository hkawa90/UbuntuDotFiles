;;; init.el --- emacs initial setting  -*- lexical-binding: t -*-
;;; Commentary:



;; Global Settings
;; Code:

;;(setq visible-bell t) ;; beep音の代わりに画面フラッシュ
(column-number-mode t) ;;モードラインに列番号表示


;; シンボリックリンクの読み込みを許可
;;(setq vc-follow-symlinks t)

;; Cursor Shape

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))



;; Speed up startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(defvar default-gc-cons-threshold gc-cons-threshold)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold (* 1024 1024 100))
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold default-gc-cons-threshold)))

;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  ;; force to load latest org-mode
  ;; TODO
  ;;  (unless package-archive-contents    ;; Refresh the packages descriptions
  ;;    (message "package-archive-contents")
  ;;    (package-refresh-contents))
  ;;  (setq package-load-list '(all))     ;; List of packages to load
  ;;  (unless (package-installed-p 'org)  ;; Make sure the Org package is
  ;;    (package-install 'org))           ;; installed, install it if not

  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))
  (unless (package-installed-p 'org)
    (package-refresh-contents)
    (package-install 'org))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>

;; Now you can use leaf!
(leaf leaf-tree :ensure t)
(leaf leaf-convert :ensure t)
(leaf transient-dwim
  :ensure t
  :bind (("M-=" . transient-dwim-dispatch)))

;; You can also configure builtin package via leaf!
(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :custom ((user-full-name . "Hideo Kawamura")
           (user-mail-address . "hideo.90.kawamura@gmail.com")
           (user-login-name . "kawa90")
           (truncate-lines . t)
           (menu-bar-mode . nil) ; Disable the menu bar
           (tool-bar-mode . nil) ; Disable the toolbar
           (scroll-bar-mode . nil) ; Disable visible scrollbar
           (visible-bell . t) ;; beep音の代わりに画面フラッシュ
           (vc-follow-symlinks . t)
           (indent-tabs-mode . nil)))

(leaf startup
  :doc "process Emacs shell arguments"
  :tag "builtin" "internal"
  :custom ((inhibit-splash-screen . t)
           (inhibit-startup-message . t)
           ))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 0.1))
  :global-minor-mode global-auto-revert-mode)


(leaf init-loader
  :doc "Loader for configuration files"
  :added "2020-12-21"
  :url "https://github.com/emacs-jp/init-loader/"
  :ensure t
  :config
  (init-loader-load (expand-file-name "inits" user-emacs-directory))
  )




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-interval 0.1)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 1)
 '(company-transformers (quote (company-sort-by-occurrence)))
 '(counsel-find-file-ignore-regexp "\\(?:\\.\\(?:\\.?/\\)\\)")
 '(counsel-yank-pop-separator "
----------
")
 '(custom-set-variables nil t)
 '(doom-themes-enable-bold nil)
 '(doom-themes-enable-italic nil)
 '(eww-history-limit 100 t)
 '(eww-search-prefix "https://www.google.co.jp/search?&q=" t)
 '(flycheck-emacs-lisp-initialize-packages t t)
 '(indent-tabs-mode nil)
 '(inhibit-splash-screen t)
 '(inhibit-startup-message t)
 '(inhibit-startup-screen t)
 '(ivy-initial-inputs-alist nil)
 '(ivy-prescient-retain-classic-highlighting t)
 '(ivy-use-selectable-prompt t)
 '(ivy-use-virtual-buffers t)
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("~/Documents/org")))
 '(package-archives
   (quote
    (("org" . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (dashboard transient-dwim selected open-junk-file neotree mozc-cand-posframe markdown-toc magit leaf-tree leaf-convert ivy-prescient hydra howm google-this git-timemachine flycheck-package flycheck-elsa el-get doom-themes dockerfile-mode diff-hl counsel company-c-headers blackout bind-key aggressive-indent)))
 '(prescient-aggressive-file-save t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(user-full-name "Hideo Kawamura")
 '(user-login-name "kawa90" t)
 '(user-mail-address "hideo.90.kawamura@gmail.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
