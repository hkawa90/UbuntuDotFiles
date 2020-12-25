;;; init.el --- 01_devlop.el  -*- lexical-binding: t; -*-
;;; Commentary:

;; development configurations

;;; Code:

(leaf lsp-mode
  :doc "LSP mode"
  :req "emacs-26.1" "dash-2.14.1" "dash-functional-2.14.1" "f-0.20.0" "ht-2.0" "spinner-1.7.3" "markdown-mode-2.3" "lv-0"
  :tag "languages" "emacs>=26.1"
  :added "2020-12-19"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :emacs>= 26.1
  :ensure t
  :after spinner markdown-mode lv
  :hook (typescript-mode-hook . lsp)
  :custom ((lsp-auto-guess-root . t)
           (lsp-restart . 'ignore)
           (lsp-prefer-flymake . nil)
           (lsp-print-io . t)
           (lsp-prefer-capf . t)
           ;; ローカル環境にのみ保存
           (lsp-session-file . "~/.emacs.lsp-session")))

(leaf lsp-ui
  :doc "UI modules for lsp-mode"
  :req "emacs-26.1" "dash-2.14" "dash-functional-1.2.0" "lsp-mode-6.0" "markdown-mode-2.3"
  :tag "tools" "languages" "emacs>=26.1"
  :added "2020-12-19"
  :url "https://github.com/emacs-lsp/lsp-ui"
  :emacs>= 26.1
  :ensure t
  :package t
  :after lsp-mode markdown-mode
  :require t)



(leaf lsp-ivy
  :doc "LSP ivy integration"
  :req "emacs-25.1" "dash-2.14.1" "lsp-mode-6.2.1" "ivy-0.13.0"
  :tag "debug" "languages" "emacs>=25.1"
  :added "2020-12-19"
  :url "https://github.com/emacs-lsp/lsp-ivy"
  :emacs>= 25.1
  :ensure t
  :after lsp-mode ivy)



(leaf lsp-treemacs
  :doc "LSP treemacs"
  :req "emacs-26.1" "dash-2.14.1" "dash-functional-2.14.1" "f-0.20.0" "ht-2.0" "treemacs-2.5" "lsp-mode-6.0"
  :tag "languages" "emacs>=26.1"
  :added "2020-12-21"
  :url "https://github.com/emacs-lsp/lsp-treemacs"
  :emacs>= 26.1
  :ensure t
  :after treemacs lsp-mode)

(leaf projectile
  :doc "Manage and navigate projects in Emacs easily"
  :req "emacs-25.1" "pkg-info-0.4"
  :tag "convenience" "project" "emacs>=25.1"
  :added "2020-12-19"
  :url "https://github.com/bbatsov/projectile"
  :emacs>= 25.1
  :ensure t
  :bind ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/gitCode")
    (setq projectile-project-search-path '("~/Projects/gitCode")))
  (setq projectile-switch-project-action #'projectile-dired)
  )

(leaf counsel-projectile
  :doc "Ivy integration for Projectile"
  :req "counsel-0.13.0" "projectile-2.0.0"
  :tag "convenience" "project"
  :added "2020-12-19"
  :url "https://github.com/ericdanan/counsel-projectile"
  :ensure t
  :after counsel projectile
  :config (counsel-projectile-mode))

(leaf treemacs-projectile
  :doc "Projectile integration for treemacs"
  :req "emacs-25.2" "projectile-0.14.0" "treemacs-0.0"
  :tag "emacs>=25.2"
  :added "2020-12-22"
  :url "https://github.com/Alexander-Miller/treemacs"
  :emacs>= 25.2
  :ensure t
  :after projectile lsp-treemacs)

;;;
;;; Indenting for C
;;;
(setq-default c-indent-tabs-mode t     ; Pressing TAB should cause indentation
              c-indent-level 4         ; A TAB is equivilent to four spaces
              c-argdecl-indent 0       ; Do not indent argument decl's extra
              c-tab-always-indent t
              backward-delete-function nil) ; DO NOT expand tabs when deleting
(c-add-style "my-c-style" '((c-continued-statement-offset 4))) ; If a statement continues on the next line, indent the continuation by 4
(defun my-c-mode-hook ()
  (c-set-style "my-c-style")
  (c-set-offset 'substatement-open '0) ; brackets should be at same indentation level as the statements they open
  (c-set-offset 'inline-open '+)
  (c-set-offset 'block-open '+)
  (c-set-offset 'brace-list-open '+)   ; all "opens" should be indented by the c-indent-level
  (c-set-offset 'case-label '+))       ; indent case labels by c-indent-level, too
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
