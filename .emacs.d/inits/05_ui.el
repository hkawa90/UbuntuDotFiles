;;; 05_ui.el --- personal configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;; personal configurations

;;; Code:

;;; テーマの設定
;;; doom-one-light
;;;
(leaf doom-themes
  :ensure t treemacs
  :custom
  (doom-themes-enable-italic . t)
  (doom-themes-enable-bold . t)
  :config
  (load-theme 'doom-one-light t)
  (doom-themes-visual-bell-config)
  ;;(doom-themes-treemacs-config)
  ;;(doom-themes-neotree-config)
  (doom-themes-org-config)
  )


;; font install
;; M-x all-the-icons-install-fonts
;; fc-cache -f -v
(leaf all-the-icons
  :doc "A library for inserting Developer icons"
  :req "emacs-24.3" "memoize-1.0.1"
  :tag "lisp" "convenient" "emacs>=24.3"
  :added "2020-12-19"
  :url "https://github.com/domtronn/all-the-icons.el"
  :emacs>= 24.3
  :ensure t
  :after memoize)

;; 各ウィンドウの左右の端に、狭いフリンジを
(set-fringe-mode 10)
;; カーソル形状
(setq-default cursor-type 'bar)
;; カーソル行ハイライト
(setq hl-line-face 'underline) ; 下線
(global-hl-line-mode)
