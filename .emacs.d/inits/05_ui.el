;;; 05_ui.el --- personal configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;; personal configurations

;;; Code:

;;; テーマの設定
;;; doom-one-light
;;;
;;(leaf doom-themes
;;  :ensure t treemacs
;;  :custom
;;  (doom-themes-enable-italic . t)
;;  (doom-themes-enable-bold . t)
;;  :config
;;  (load-theme 'doom-one-light t)
;;(load-theme 'doom-one t)
;;  (doom-themes-visual-bell-config)
;;(doom-themes-treemacs-config)
;;(doom-themes-neotree-config)
;;  (doom-themes-org-config)
;;  )

;;; Moody(Tabs and ribbons for the mode line)

(leaf moody
  :doc "Tabs and ribbons for the mode line"
  :req "emacs-25.3"
  :tag "emacs>=25.3"
  :added "2021-01-03"
  :url "https://github.com/tarsius/moody"
  :emacs>= 25.3
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;;; Modus Themes (Modus Operandi and Modus Vivendi)
;;; - high contrast theme
;;; https://protesilaos.com/modus-themes/

(leaf modus-themes
  :doc "Highly accessible themes (WCAG AAA)"
  :req "emacs-26.1"
  :tag "accessibility" "theme" "faces" "emacs>=26.1"
  :added "2021-01-03"
  :url "https://gitlab.com/protesilaos/modus-themes"
  :emacs>= 26.1
  :ensure t
  :bind ("<f2>" . modus-themes-toggle)
  :init
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil
        modus-themes-mode-line 'moody)
  :config
  ;; Load the theme of your choice
  (modus-themes-load-operandi)
  ;; ;; OR
  ;; (load-theme 'modus-operandi t)
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
