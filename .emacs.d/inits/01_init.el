;;; 01_init.el --- localize configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;; initialization configurations

;;; Code:

;; daemon modeでdashboardを表示させる
(if (daemonp)
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

;; Save the file specified code with basic utf-8 if it exists
(prefer-coding-system 'utf-8)

;; dired modeで文字化け抑止
(add-hook 'dired-mode-hook
          (lambda ()
            (make-local-variable 'coding-system-for-read)
            (setq coding-system-for-read 'utf-8)))

;;(add-hook 'shell-mode-hook
;;          (lambda()
;;            (set-buffer-process-coding-system 'utf-8 'utf-8)
;;            ))

;; maximized screen
;;(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;(toggle-frame-maximized)
;; See D.2 Table of X Resources for Emacs
;; - Equal to running emacs with '-mm' option.
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;;(message "default-frame-alist %s" default-frame-alist)
;;(message "initial-frame-alist %s" initial-frame-alist)
;; full screen
;;(add-to-list 'default-frame-alist '(fullscreen . fullboth))
;; doom-themes
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme  'doom-dracula t)
(doom-themes-visual-bell-config)
;;  (doom-themes-treemacs-config)
(doom-themes-org-config)
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
