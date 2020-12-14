;;; init.el --- emacs initial setting  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:


(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(set-frame-parameter nil 'fullscreen 'maximized)

;; Save the file specified code with basic utf-8 if it exists
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; theme
(load-theme 'misterioso t)
;; font
(add-to-list 'default-frame-alist '(font . "Cica-18"))
;; Cursor Shape
(setq-default cursor-type 'bar)
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "gray20"))
    (((class color)
      (background light))
     (:background "ForestGreen"))
    (t
    ()))
"*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
;; (setq hl-line-face 'underline) ; 下線
(global-hl-line-mode)


;; Speed up startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(defvar default-gc-cons-threshold gc-cons-threshold)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold (* 1024 1024 100))
(add-hook 'emacs-startup-hook
		  (lambda ()
			"Restore defalut values after startup."
			(setq file-name-handler-alist default-file-name-handler-alist)
			(setq gc-cons-threshold default-gc-cons-threshold)))

;; <leaf-install-code>
  (eval-and-compile
    (customize-set-variable
     'package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
    (package-initialize)
    (unless (package-installed-p 'leaf)
      (package-refresh-contents)
      (package-install 'leaf))

    (leaf leaf-keywords
      :ensure t
      :init
      ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
      (leaf hydra :ensure t)
      (leaf el-get :ensure t)
      (leaf blackout :ensure t)
      :config
      ;; initialize leaf-keywords.el
      (leaf-keywords-init)))
  ;; </leaf-install-code>

  ;; Now you can use leaf!
  (leaf leaf-tree :ensure t)
  (leaf leaf-convert :ensure t)
  (leaf transient-dwim
    :ensure t
    :bind (("M-=" . transient-dwim-dispatch)))

  ;; You can also configure builtin package via leaf!
  (leaf cus-start
    :doc "define customization properties of builtins"
    :tag "builtin" "internal"
    :custom ((user-full-name . "Hideo Kawamura")
             (user-mail-address . "hideo.90.kawamura@gmail.com")
             (user-login-name . "kawa90")
             (truncate-lines . t)
             (menu-bar-mode . t)
             (tool-bar-mode . nil)
             (scroll-bar-mode . nil)
             (indent-tabs-mode . nil)))

  (leaf autorevert
    :doc "revert buffers when files on disk change"
    :tag "builtin"
    :custom ((auto-revert-interval . 0.1))
    :global-minor-mode global-auto-revert-mode)

  ;; Nest package configurations
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

(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :blackout t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-use-selectable-prompt . t))
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper)))

  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :blackout t
    :bind (("C-S-s" . counsel-imenu)
           ("C-x C-r" . counsel-recentf))
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t))

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :custom ((prescient-aggressive-file-save . t))
  :global-minor-mode prescient-persist-mode)
  
(leaf ivy-prescient
  :doc "prescient.el + Ivy"
  :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :after prescient ivy
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t)

;; ------------------------------------------------------------------------
;; EWW (Emacs Web Wowser, Web Browser)
;; ------------------------------------------------------------------------
(leaf eww
  :bind (("C-c C-e" . eww))
  :custom `((eww-search-prefix . "https://www.google.co.jp/search?&q=")
            (eww-history-limit . 100)))
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

;;;
;;; テーマの設定
;;; Doom Tomorrow Night
;;;
(leaf doom-themes
  :ensure t neotree 
  :custom
  (doom-themes-enable-italic . nil)
  (doom-themes-enable-bold . nil)
  :config
  (load-theme  'doom-dark+ t)
  ;  (load-theme 'doom-tomorrow-night t)
  
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  )

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
;;; Org
;;; https://solist.work/blog/posts/information-organizize-method/
(leaf org
  :bind (("C-c a" . org-agenda) ("C-c c" . org-capture))
  :init
    (defun my:howm-create-file ()
    "Make howm create file with 'org-capture'."
    (interactive)
    (format-time-string "~/GoogleDrvie/howm/%Y/%m/%Y%m%d%H%M.md" (current-time)))
  :config
;  (bind-key "C-c a" 'org-agenda)
;  (bind-key "C-c c" 'org-capture)
  (setq org-log-done 'time)
  (setq org-use-speed-commands t)
  (setq org-src-fontify-natively t)
  (setq org-agenda-files '("~/GoogleDrive/howm/org/task.org"))
  (setq org-capture-templates
		'(("t" " Task" entry (file+headline "~/GoogleDrive/howm/org/task.org" "Task")
		   "** TODO %?\n SCHEDULED: %^t \n" :prepend t)
		  ("m" " Memo" plain (file my:howm-create-file)
		   "# memo: %?\n%U %i")
		  ("n" " Note" plain (file my:howm-create-file)
		   "# note: %?\n%U %i")
		  ("p" "★ Perl" plain (file my:howm-create-file)
		   "# Perl: %?\n%U %i\n\n>>>\n\n```perl\n%i\n```")
		  ("e" "★ Emacs" plain (file my:howm-create-file)
		   "# emacs: %?\n%U %i\n\n```emacs-lisp\n%i\n```")
		  ("l" "★ Linux" plain (file my:howm-create-file)
		   "# linux: %?\n%U %i")))
  :init
  ;; Maximize the org-capture buffer
  (defvar my:org-capture-before-config nil
    "Window configuration before 'org-capture'.")
  (defadvice org-capture (before save-config activate)
    "Save the window configuration before 'org-capture'."
    (setq my:org-capture-before-config (current-window-configuration)))
  (add-hook 'org-capture-mode-hook 'delete-other-windows))

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

(leaf mozc
  :ensure t
  :bind (("<hiragana-katakana>" . toggle-input-method)
         ("<henkan>" . toggle-input-method))
  :config
  (setq default-input-method "japanese-mozc")
  (setq mozc-helper-program-name "mozc_emacs_helper")
  (setq mozc-leim-title "♡かな")
  (set-cursor-color "#BD93F9")
  (add-hook 'input-method-activate-hook
			(lambda() (set-cursor-color "#CC3333")))
  (add-hook 'input-method-inactivate-hook
  			(lambda() (set-cursor-color "#BD93F9")))
  ;; Confirmed immediately
  (define-key mozc-mode-map "," '(lambda () (interactive) (mozc-insert-str "、")))
  (define-key mozc-mode-map "." '(lambda () (interactive) (mozc-insert-str "。")))
  (define-key mozc-mode-map "?" '(lambda () (interactive) (mozc-insert-str "？")))
  (define-key mozc-mode-map "!" '(lambda () (interactive) (mozc-insert-str "！")))
  (defun mozc-insert-str (str)
	"If punctuation marks, immediately confirm."
	(mozc-handle-event 'enter)
	(toggle-input-method)
	(insert str)
	(toggle-input-method))

  ;; Key-chord measures
  (defadvice toggle-input-method (around toggle-input-method-around activate)
	"Input method function in key-chord.el not to be nil."
	(let ((input-method-function-save input-method-function))
	  ad-do-it
	  (setq input-method-function input-method-function-save)))

  :init
  (leaf mozc-cand-posframe :ensure t
	:when window-system
	:require t
	:config
	(setq mozc-candidate-style 'posframe)))


(leaf mozc-tool-setting
  :config
  :bind (
  ("<f7>" . select-mozc-tool)
  ("<f8>" . my:mozc-word-regist))
  :init
  (defun select-mozc-tool ()
	"Select mozc tool command."
	(interactive)
	(counsel-M-x "my:mozc "))

  (defun my:mozc-config-dialog ()
	"Run the mozc-tool in the background."
	(interactive)
	(compile "/usr/lib/mozc/mozc_tool --mode=config_dialog"))

  (defun my:mozc-dictionary-tool ()
	"Run the mozc-tool in the background."
	(interactive)
	(compile "/usr/lib/mozc/mozc_tool --mode=dictionary_tool"))

  (defun my:mozc-word-regist ()
	"Run the mozc-tool in the background."
	(interactive)
	(compile "/usr/lib/mozc/mozc_tool --mode=word_register_dialog"))

  (defun my:mozc-hand-writing ()
	"Run the mozc-tool in the background."
	(interactive)
	(compile "/usr/lib/mozc/mozc_tool --mode=hand_writing")))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-interval 0.1)
 '(counsel-find-file-ignore-regexp "\\(?:\\.\\(?:\\.?/\\)\\)")
 '(counsel-yank-pop-separator "
----------
")
 '(custom-set-variables nil t)
 '(doom-themes-enable-bold nil)
 '(doom-themes-enable-italic nil)
 '(eww-history-limit 100 t)
 '(eww-search-prefix "https://www.google.co.jp/search?&q=" t)
 '(flycheck-emacs-lisp-initialize-packages t t)
 '(indent-tabs-mode nil)
 '(ivy-initial-inputs-alist nil)
 '(ivy-prescient-retain-classic-highlighting t)
 '(ivy-use-selectable-prompt t)
 '(menu-bar-mode t)
 '(package-archives
   (quote
    (("org" . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (flycheck-elsa flycheck-package flycheck transient-dwim leaf-convert leaf-tree leaf-keywords hydra el-get blackout)))
 '(prescient-aggressive-file-save t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(user-full-name "Hideo Kawamura")
 '(user-login-name "kawa90" t)
 '(user-mail-address "hideo.90.kawamura@gmail.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
