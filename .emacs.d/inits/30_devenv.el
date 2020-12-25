;;; 30_devenv.el --- utility configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;; utility configurations

;;; Code:


(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-25.1" "async-20200113" "dash-20200524" "git-commit-20200516" "transient-20200601" "with-editor-20200522"
  :tag "vc" "tools" "git" "emacs>=25.1"
  :added "2020-12-16"
  :emacs>= 25.1
  :ensure t
  :after git-commit with-editor)

;; Dockerfile 用の設定
(leaf dockerfile-mode
  :ensure t
  :mode (("Dockerfile" . dockerfile-mode))
  )
