;;; 03_ui.el --- personal configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;; personal configurations

;;; Code:

;; full screen
(if window-system
    (progn
      ;; full screen
      (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
      (add-to-list 'default-frame-alist '(fullscreen . maximized))
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

      ))

(leaf which-key
  :doc "Display available keybindings in popup"
  :req "emacs-24.4"
  :tag "emacs>=24.4"
  :added "2020-12-21"
  :url "https://github.com/justbur/emacs-which-key"
  :emacs>= 24.4
  :ensure t
  :hook (after-init-hook . which-key-mode)
  :config
  (which-key-setup-minibuffer)
  (setq which-key-idle-secondary-delay 0)
  )
