;;; 01_personal.el --- personal configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;; personal configurations

;;; Code:

;;;
;;; Org
;;; https://solist.work/blog/posts/information-organizize-method/
(leaf org
  :bind (("C-c a" . org-agenda) ("C-c c" . org-capture))
  :init
  :config
  (setq org-log-done 'time)
  (setq org-use-speed-commands t)
  (setq org-src-fontify-natively t)
  (setq org-agenda-files '("~/Documents/org"))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

  (setq org-capture-templates
        '(("t" " Task/TODO")
          ("tt" " Task" entry (file+headline "~/Documents/org/task.org" "")
           "** TODO %?\n SCHEDULED: %^t \n" :prepend t)
          ("td" " Task" entry (file+headline "~/Documents/org/todo.org" "")
           "** TODO %?\n %U %i")

          ("c" " Memo from clipboard")
          ("cm" " Memo" plain (file "~/Documents/org/memo.org")
           "** MEMO %?\n%U %i%x")
          ("cl" " linux Memo" plain (file "~/Documents/org/LinuxMemo.org")
           "** LinuxMEMO %?\n%U %i%x")
          ("cj" " JS Memo" plain (file "~/Documents/org/JSMemo.org")
           "** JSMEMO %?\n%U %i%x")
          ("ce" " Emacs Memo" plain (file "~/Documents/org/EmacsMemo.org")
           "** EmacsMEMO %?\n%U %i%x")
          ("cn" " Note" plain (file "~/Documents/org/note.org")
           "** NOTE %?\n%U %i%x")
          ("cs" " Scrap" plain (file "~/Documents/org/scrap.org")
           "** NOTE %?\n%U %i%x")
          
          ("m" " Memo")
          ("mm" " Memo" plain (file "~/Documents/org/memo.org")
           "** MEMO %?\n%U %i")
          ("ml" " linux Memo" plain (file "~/Documents/org/LinuxMemo.org")
           "** LinuxMEMO %?\n%U %i")
          ("mj" " JS Memo" plain (file "~/Documents/org/JSMemo.org")
           "** JSMEMO %?\n%U %i")
          ("me" " Emacs Memo" plain (file "~/Documents/org/EmacsMemo.org")
           "** EmacsMEMO %?\n%U %i")
          ("mn" " Note" plain (file "~/Documents/org/note.org")
           "** NOTE %?\n%U %i")))
  :init
  ;; Maximize the org-capture buffer
  (defvar my:org-capture-before-config nil
    "Window configuration before 'org-capture'.")
  (defadvice org-capture (before save-config activate)
    "Save the window configuration before 'org-capture'."
    (setq my:org-capture-before-config (current-window-configuration)))
  (add-hook 'org-capture-mode-hook 'delete-other-windows))
