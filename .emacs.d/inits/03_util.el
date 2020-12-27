;;; 03_util.el --- utility configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;; utility configurations

;;; Code:
(leaf flycheck
  :doc "On-the-fly syntax checking"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :custom ((flycheck-emacs-lisp-initialize-packages . t))
  (custom-set-variables
   '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs)))
  :hook (emacs-lisp-mode-hook lisp-interaction-mode-hook)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (leaf flycheck-package
    :doc "A Flycheck checker for elisp package authors"
    :ensure t
    :config
    (flycheck-package-setup))

  (leaf flycheck-elsa
    :doc "Flycheck for Elsa."
    :emacs>= 25
    :ensure t
    :config
    (flycheck-elsa-setup))

  ;; ...
  )


(leaf exec-path-from-shell
  :doc "Get environment variables such as $PATH from the shell"
  :req "emacs-24.1"
  :tag "environment" "unix" "emacs>=24.1"
  :added "2020-08-27"
  :url "https://github.com/purcell/exec-path-from-shell"
  :emacs>= 24.1
  :ensure t
  :config
  ;;;(when (daemonp)
  ;;;(exec-path-from-shell-initialize);)
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-arguments '("-l")) ; non-interactive shell is faster.
  (exec-path-from-shell-copy-envs '("PATH" "MANPATH" "LANG" "LC_ALL" "LC_MESSAGES"))
  )

;; ------------------------------------------------------------------------
;; EWW (Emacs Web Wowser, Web Browser)
;; ------------------------------------------------------------------------
(leaf eww
  :bind (("C-c C-e" . eww))
  :custom `((eww-search-prefix . "https://www.google.co.jp/search?&q=")
            (eww-history-limit . 100)))


(leaf which-key
  :doc "Display available keybindings in popup"
  :req "emacs-24.4"
  :tag "emacs>=24.4"
  :added "2020-12-21"
  :url "https://github.com/justbur/emacs-which-key"
  :emacs>= 24.4
  :ensure t
  :hook (after-init-hook . which-key-mode)
  :config
  (which-key-setup-minibuffer)
  (setq which-key-idle-secondary-delay 0)
  )

;;;
;;; Company
;;;
(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)

(leaf company-c-headers
  :doc "Company mode backend for C/C++ header files"
  :req "emacs-24.1" "company-0.8"
  :tag "company" "development" "emacs>=24.1"
  :added "2020-03-25"
  :emacs>= 24.1
  :ensure t
  :after company
  :defvar company-backends
  :config
  (add-to-list 'company-backends 'company-c-headers))

;;;
;;; Selected
;;;
(leaf selected
  :ensure t
  :global-minor-mode selected-global-mode
  :bind ((:selected-keymap
          (";" . comment-dwim)
          ("d" . clipboard-kill-region)
          ("f" . describe-function)
          ("v" . describe-variable)
          ("c" . clipboard-kill-ring-save)
          ("i" . iedit-mode)
          ("s" . swiper-thing-at-point)
          ("k" . my:koujien)
          ("e" . my:eijiro)
          ("w" . my:weblio)
          ("t" . google-translate-auto)
          ("g" . google-this-url)
          ("G" . my:google))))

(leaf user-function-selected
  :init
  ;; Control mozc when seleceted
  (defun my:activate-selected ()
    (selected-global-mode 1)
    (selected--on)
    (remove-hook 'activate-mark-hook #'my:activate-selected))
  (add-hook 'activate-mark-hook #'my:activate-selected)
  (defun my:ime-on ()
    (interactive)
    (when (null current-input-method) (toggle-input-method)))
  (defun my:ime-off ()
    (interactive)
    (inactivate-input-method))

  (defvar my:ime-flag nil)
  (add-hook
   'activate-mark-hook
   #'(lambda ()
       (setq my:ime-flag current-input-method) (my:ime-off)))
  (add-hook
   'deactivate-mark-hook
   #'(lambda ()
       (unless (null my:ime-flag) (my:ime-on))))

  (defun my:koujien (str)
    (interactive (list (my:get-region nil)))
    (browse-url (format "https://sakura-paris.org/dict/広辞苑/prefix/%s"
			(upcase (url-hexify-string str)))))

  (defun my:weblio (str)
    (interactive (list (my:get-region nil)))
    (browse-url (format "https://www.weblio.jp/content/%s"
			(upcase (url-hexify-string str)))))

  (defun my:eijiro (str)
    (interactive (list (my:get-region nil)))
    (browse-url (format "https://eow.alc.co.jp/%s/UTF-8/"
			(upcase (url-hexify-string str)))))
  (defun my:google (str)
    (interactive (list (my:get-region nil)))
    (browse-url (format "https://www.google.com/search?hl=ja&q=%s"
			(upcase (url-hexify-string str)))))
  (defun my:get-region (r)
    "Get search word from region."
    (buffer-substring-no-properties (region-beginning) (region-end))))

;; Dashboard
(dashboard-setup-startup-hook)
(leaf dashboard
  :doc "A startup screen extracted from Spacemacs"
  :req "emacs-25.3" "page-break-lines-0.11"
  :tag "dashboard" "tools" "screen" "startup" "emacs>=25.3"
  :added "2020-12-17"
  :url "https://github.com/emacs-dashboard/emacs-dashboard"
  :emacs>= 25.3
  :ensure t
  :after page-break-lines
  :config (dashboard-setup-startup-hook))

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 8)
                        (agenda . 5)))


(leaf aggressive-indent
  :ensure t
  :hook ((emacs-lisp-mode-hook css-mode-hook) . aggressive-indent-mode))


(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :custom ((prescient-aggressive-file-save . t))
  :global-minor-mode prescient-persist-mode)

;;;
;;; Google-this
;;;
(leaf google-this
  :doc "A set of functions and bindings to google under point."
  :req "emacs-24.1"
  :tag "hypermedia" "convenience" "emacs>=24.1"
  :added "2020-12-14"
  :url "http://github.com/Malabarba/emacs-google-this"
  :emacs>= 24.1
  :ensure t
  :config
  (setq google-this-location-suffix "co.jp")
  (defun google-this-url (str) "URL for google searches."
         (concat google-this-base-url google-this-location-suffix
                 "/search?q=%s&hl=ja&num=10&as_qdr=y5&lr=lang_ja"
                 (upcase (url-hexify-string str))))
  )

(leaf google-translate
  :doc "Emacs interface to Google Translate."
  :tag "convenience"
  :added "2020-12-27"
  :url "https://github.com/atykhonov/google-translate"
  :ensure t)

(leaf google-translate-default-ui
  :doc "default UI for Google Translate"
  :after google-translate
  :added "2020-12-27"
  :leaf-autoload nil
  :require t)




(leaf open-junk-file
  :ensure t
  :config
  (setq open-junk-file-format "~/GoogleDrive/howm/junk/%Y%m%d.")
  (setq open-junk-file-find-file-function 'find-file)
  :init
  ;; https://qiita.com/zonkyy/items/eba6bc64f66d278f0032
  (leaf em-glob	:require t
    :config
    (defvar junk-file-dir "~/GoogleDrive/howm/junk/")
    (defun open-last-junk-file ()
      "Open last created junk-file."
      (interactive)
      (find-file
       (car
        (last (eshell-extended-glob
               (concat
                (file-name-as-directory junk-file-dir)
                "*.*.*"))))))))
