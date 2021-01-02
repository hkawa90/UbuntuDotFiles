;;; Linux-fonts.el --- localize configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;; initialization configurations

;;; Code:

;; Fonts
;;; fixed pitch font
(let* ((asciifont "Fira Code")
       (jpfont "Cica")
       (h 128)
       (fontspec (font-spec :family asciifont :height h))
       (jp-fontspec (font-spec :family jpfont :height h)))
  (create-fontset-from-ascii-font asciifont nil "FixedCica")
  (set-fontset-font "fontset-FixedCica" 'japanese-jisx0208 jp-fontspec nil 'append)
  (set-fontset-font "fontset-FixedCica" 'japanese-jisx0213.2004-1 jp-fontspec nil 'append)
  (set-fontset-font "fontset-FixedCica" 'japanese-jisx0213-2 jp-fontspec nil 'append)
  (set-fontset-font "fontset-FixedCica" 'katakana-jisx0201 jp-fontspec nil 'append)
  (set-fontset-font "fontset-FixedCica" '(#x0080 . #x024F) fontspec nil 'append)
  (set-fontset-font "fontset-FixedCica" '(#x0370 . #x03FF) fontspec nil 'append)

  (set-face-attribute 'fixed-pitch nil :family nil :fontset "fontset-FixedCica" :height h)
  (set-face-font 'fixed-pitch "fontset-FixedCica")
  (set-face-attribute 'default nil :family nil :fontset "fontset-FixedCica" :height h :width 'normal)
  (set-face-font 'default "fontset-FixedCica"))

;;; variable pitch font
(let* ((asciifont "Century Schoolbook L")
       (jpfont "Noto Serif CJK JP")
       (h 128)
       (fontspec (font-spec :family asciifont :height h))
       (jp-fontspec (font-spec :family jpfont :height h)))
  (create-fontset-from-ascii-font asciifont nil "VariableNoto")
  (set-fontset-font "fontset-VariableNoto" 'japanese-jisx0208 jp-fontspec nil 'append)
  (set-fontset-font "fontset-VariableNoto" 'japanese-jisx0213.2004-1 jp-fontspec nil 'append)
  (set-fontset-font "fontset-VariableNoto" 'japanese-jisx0213-2 jp-fontspec nil 'append)
  (set-fontset-font "fontset-VariableNoto" 'katakana-jisx0201 jp-fontspec nil 'append)
  (set-fontset-font "fontset-VariableNoto" '(#x0080 . #x024F) fontspec nil 'append)
  (set-fontset-font "fontset-VariableNoto" '(#x0370 . #x03FF) fontspec nil 'append)

  (set-face-attribute 'variable-pitch nil :family nil :fontset "fontset-VariableNoto" :height h)
  (set-face-font 'variable-pitch "fontset-VariableNoto"))


(setq face-font-rescale-alist '((".*Cica.*" . 1.08)
                                (".*Noto Serif CJK JP.*" . 1.08)
                                (".*Fira Code.*" . 1.0)
                                (".*Century Schoolbook L.*" . 1.0)
                                ("-cdac$" . 1.3)))

