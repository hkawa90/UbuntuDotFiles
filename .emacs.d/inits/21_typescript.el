;;; package config for typescript
;; Commentary
;; Typescript
(leaf typescript-mode
  :doc "Major mode for editing typescript"
  :req "emacs-24.3"
  :tag "languages" "typescript" "emacs>=24.3"
  :added "2020-12-17"
  :url "http://github.com/ananthakumaran/typescript.el"
  :emacs>= 24.3
  :ensure t
  :setq (typescript-indent-level . 2)
  :require t
  )
