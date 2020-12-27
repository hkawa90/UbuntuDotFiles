;;; 05_ui.el --- personal configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;; personal configurations

;;; Code:

;(defun my:setup-ui (frame)

;  )

;;(if window-system ;; window-system is nil on Emacs27 daemon mode
;;(if (or (daemonp) window-system)
;;;    (progn
;;;      (message "setup ui")
;;;      (add-hook 'after-make-frame-functions #'my:setup-ui)))

;(if (or (daemonp) window-system)
;    (my:setup-ui nil))

;;;    (if (or (daemonp) window-system)
;;;        (add-hook 'after-make-frame-functions 'my:setup-ui))

;;;
;;; テーマの設定
;;; Doom Tomorrow Night
;;;
(leaf doom-themes
  :ensure t treemacs
  :custom
  ;;(doom-themes-enable-italic . t)
  ;;(doom-themes-enable-bold . t)
  :config
  ;;(load-theme  'doom-dracula t)
  ;;(load-theme 'doom-tomorrow-night t)
  ;;(doom-themes-treemacs-config)
  ;;(doom-themes-neotree-config)
  ;;(doom-themes-org-config)
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
