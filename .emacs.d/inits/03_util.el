;;; 02_util.el --- utility configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;; utility configurations

;;; Code:


(leaf exec-path-from-shell
  :doc "Get environment variables such as $PATH from the shell"
  :req "emacs-24.1"
  :tag "environment" "unix" "emacs>=24.1"
  :added "2020-08-27"
  :url "https://github.com/purcell/exec-path-from-shell"
  :emacs>= 24.1
  :ensure t
  :config (exec-path-from-shell-initialize))
