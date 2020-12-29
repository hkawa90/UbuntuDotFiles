;;; Linux-fonts.el --- localize configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;; initialization configurations

;;; Code:

;; Fonts
;;; fixed pitch font
(let* ((size 15)
       (asciifont "Cica")
       (jpfont "Cica")
       (h (* size 10))
       (fontspec (font-spec :family asciifont))
       (jp-fontspec (font-spec :family jpfont)))
  (create-fontset-from-ascii-font "Fira Code-16" nil "FixedCica")
  ;;(set-face-attribute 'default nil :family asciifont :height h)
  (set-fontset-font "fontset-FixedCica" 'japanese-jisx0208 jp-fontspec nil 'append)
  (set-fontset-font "fontset-FixedCica" 'japanese-jisx0213.2004-1 jp-fontspec nil 'append)
  (set-fontset-font "fontset-FixedCica" 'japanese-jisx0213-2 jp-fontspec nil 'append)
  (set-fontset-font "fontset-FixedCica" 'katakana-jisx0201 jp-fontspec nil 'append)
  (set-fontset-font "fontset-FixedCica" '(#x0080 . #x024F) fontspec nil 'append)
  (set-fontset-font "fontset-FixedCica" '(#x0370 . #x03FF) fontspec nil 'append))
(add-to-list 'default-frame-alist '(font . "fontset-FixedCica"))
(set-face-attribute 'fixed-pitch nil :fontset "fontset-FixedCica")

;;; variable pitch font
(let* ((size 15)
       (asciifont "Noto Serif CJK JP")
       (jpfont "Noto Serif CJK JP")
       (h (* size 10))
       (fontspec (font-spec :family asciifont))
       (jp-fontspec (font-spec :family jpfont)))
  (create-fontset-from-ascii-font "Noto Serif CJK JP-16" nil "VariableNoto")
  ;;(set-face-attribute 'default nil :family asciifont :height h)
  (set-fontset-font "fontset-VariableNoto" 'japanese-jisx0208 jp-fontspec nil 'append)
  (set-fontset-font "fontset-VariableNoto" 'japanese-jisx0213.2004-1 jp-fontspec nil 'append)
  (set-fontset-font "fontset-VariableNoto" 'japanese-jisx0213-2 jp-fontspec nil 'append)
  (set-fontset-font "fontset-VariableNoto" 'katakana-jisx0201 jp-fontspec nil 'append)
  (set-fontset-font "fontset-VariableNoto" '(#x0080 . #x024F) fontspec nil 'append)
  (set-fontset-font "fontset-VariableNoto" '(#x0370 . #x03FF) fontspec nil 'append))

(set-face-attribute 'variable-pitch nil :fontset "fontset-VariableNoto")


(setq face-font-rescale-alist '((".*Cica.*" . 1.0)
                                (".*Noto Serif CJK JP.*" . 1.0)
                                (".*Fira Code.*" . 0.9)
                                ("-cdac$" . 1.3)))
