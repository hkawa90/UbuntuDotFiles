;;; 05_ui.el --- personal configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;; personal configurations

;;; Code:
;;(if window-system ;; window-system is nil on Emacs27 daemon mode
(progn
  ;; maximized screen
  ;;(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  ;;(add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;;(toggle-frame-maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (message "default-frame-alist %s" default-frame-alist)
  (message "initial-frame-alist %s" initial-frame-alist)
  ;; full screen
  ;;(add-to-list 'default-frame-alist '(fullscreen . fullboth))
  ;; font
  (defvar my-font-scale (/ (x-display-pixel-width) 100)
    "font scaling parameter")
  (create-fontset-from-ascii-font
   "Cica:style=regular:spacing=0" nil "Cica")

  (dolist (charset
           '(unicode
             japanese-jisx0208
             japanese-jisx0208-1978
             japanese-jisx0212
             japanese-jisx0213-1
             japanese-jisx0213-2
             japanese-jisx0213-a
             japanese-jisx0213.2004-1
             katakana-jisx0201))
    (set-fontset-font "fontset-Cica"
                      charset
                      (font-spec :family "Cica" :size my-font-scale)
                      nil 'prepend))

  (setq default-frame-alist
        (append (list
                 '(font . "fontset-Cica")
                 '(alpha . 100) ;; transparent background
                 )
                default-frame-alist))
  (set-face-attribute 'fixed-pitch nil :family "Cica")
  ;; 各ウィンドウの左右の端に、狭いフリンジを
  (set-fringe-mode 10)
  ;; カーソル形状
  (setq-default cursor-type 'bar)
  ;; カーソル行ハイライト
  (defface hlline-face
    '((((class color)
        (background dark))
       (:background "gray20"))
      (((class color)
        (background light))
       (:background "ForestGreen"))
      (t
       ()))
    "*Face used by hl-line.")
  (setq hl-line-face 'hlline-face)
  ;; (setq hl-line-face 'underline) ; 下線
  (global-hl-line-mode)

  )
;;)


;;;
;;; テーマの設定
;;; Doom Tomorrow Night
;;;
(leaf doom-themes
  :ensure t neotree 
  :custom
  (doom-themes-enable-italic . nil)
  (doom-themes-enable-bold . nil)
  :config
  (load-theme  'doom-dark+ t)
                                        ;  (load-theme 'doom-tomorrow-night t)
  
  (doom-themes-neotree-config)
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
