;;; init.el --- emacs initial setting  -*- lexical-binding: t -*-
;;; Commentary:



;; Global Settings
;; Code:

(let ((default-directory (locate-user-emacs-file "./lisp")))
  (add-to-list 'load-path default-directory))

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

;;https://qiita.com/conao3/items/347d7e472afd0c58fbd7
;;問題はcustomがinit.elに次のようなダンプを出力する点です。このダンプにより、leafの :custom で管理している場合、2箇所を修正する必要が生じます。
(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf startup
  :doc "process Emacs shell arguments"
  :tag "builtin" "internal"
  :custom ((inhibit-splash-screen . nil)
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

