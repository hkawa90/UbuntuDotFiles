;;; 01_init.el --- localize configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;; initialization configurations

;;; Code:

;; daemon modeでdashboardを表示させる
(if (daemonp)
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

;; Save the file specified code with basic utf-8 if it exists
;;(prefer-coding-system 'utf-8)
;;(setq default-process-coding-system 'utf-8)
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
;; See D.2 Table of X Resources for Emacs
;; - Equal to running emacs with '-mm' option.
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;値はfullboth、maximized、fullwidth、fullheightのうちどれか1つを指定でき、これらはコマンドラインオプションの‘-fs’、‘-mm’、‘-fw’、‘-fh’に相当します


