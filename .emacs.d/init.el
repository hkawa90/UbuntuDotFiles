;;; init.el --- emacs initial setting  -*- lexical-binding: t -*-
;;; Commentary:
;; Global Settings
;; Code:


(unless (>= emacs-major-version 27)
  (load (concat user-emacs-directory "early-init.el"))
  (package-initialize))

;;(package-initialize)
(require 'package)
(let ((default-directory (locate-user-emacs-file "./lisp")))
  (add-to-list 'load-path default-directory))

;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  ;; force to load latest org-mode
  ;; TODO
  ;;  (unless package-archive-contents    ;; Refresh the packages descriptions
  ;;    (message "package-archive-contents")
  ;;    (package-refresh-contents))
  ;;  (setq package-load-list '(all))     ;; List of packages to load
  ;;  (unless (package-installed-p 'org)  ;; Make sure the Org package is
  ;;    (package-install 'org))           ;; installed, install it if not

  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf)
    (message "leaf installed...")
    )
  (unless (package-installed-p 'org)
    (package-refresh-contents)
    (package-install 'org)
    (message "org installed...")
    )
  (require 'leaf)
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
           (visible-bell . t) ;; beep音の代わりに画面フラッシュ
           (vc-follow-symlinks . t)
           (indent-tabs-mode . nil)
           ;; Save the file specified code with basic utf-8 if it exists
           (prefer-coding-system . 'utf-8)))

;;https://qiita.com/conao3/items/347d7e472afd0c58fbd7
;;問題はcustomがinit.elに次のようなダンプを出力する点です。このダンプにより、leafの :custom で管理している場合、2箇所を修正する必要が生じます。
(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 0.1))
  :global-minor-mode global-auto-revert-mode)

(leaf ka/font-setting
  :config
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


  (setq face-font-rescale-alist '((".*Cica.*" . 1.04)
                                  (".*Noto Serif CJK JP.*" . 1.04)
                                  (".*Fira Code.*" . 1.0)
                                  (".*Century Schoolbook L.*" . 1.0)
                                  ("-cdac$" . 1.3)))

  )

(leaf dired
  ;;(setq default-process-coding-system 'utf-8)
  ;; dired modeで文字化け抑止
  :hook (dired-mode-hook .
                         (lambda ()
                           (make-local-variable 'coding-system-for-read)
                           (setq coding-system-for-read 'utf-8))))

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :custom (flycheck-emacs-lisp-initialize-packages . t)
  (flycheck-disabled-checkers . '(javascript-jshint javascript-jscs))
  :init (global-flycheck-mode)
  :config
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

  (leaf flycheck-rust
    :doc "Flycheck: Rust additions and Cargo support"
    :req "emacs-24.1" "flycheck-28" "dash-2.13.0" "seq-2.3" "let-alist-1.0.4"
    :tag "convenience" "tools" "emacs>=24.1"
    :added "2021-01-06"
    :url "https://github.com/flycheck/flycheck-rust"
    :emacs>= 24.1
    :ensure t
    :after flycheck)
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
  ;;(exec-path-from-shell-copy-envs '("PATH" "MANPATH" "LANG" "LC_ALL" "LC_MESSAGES"))
  (exec-path-from-shell-copy-envs '("PATH" "MANPATH"))
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
  (setq which-key-idle-secondary-delay 10)
  )

;; Dashboard
(leaf dashboard
  :doc "A startup screen extracted from Spacemacs"
  :req "emacs-25.3" "page-break-lines-0.11"
  :tag "dashboard" "tools" "screen" "startup" "emacs>=25.3"
  :added "2020-12-17"
  :url "https://github.com/emacs-dashboard/emacs-dashboard"
  :emacs>= 25.3
  :require t
  :ensure t
  :after page-break-lines org
  :preface (dashboard-setup-startup-hook)
  :hook (after-init-hook . (lambda ()
                             (if (daemonp)
                                 (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))))

  :custom ((dashboard-set-heading-icons . t)
           (dashboard-center-content . t)
           ;; LANG=Cかつset-language-environment設定でやっと正常表示、
           ;; 面倒なので水平線表示しないようにする
           (dashboard-page-separator . "\n\n") 
           (dashboard-set-file-icons . t)
           (dashboard-items . '((recents  . 5)
                                (bookmarks . 5)
                                (projects . 8)
                                (agenda . 8))))

  )


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
                 (upcase (url-hexify-string str)))))


(leaf google-translate
  :doc "Emacs interface to Google Translate."
  :tag "convenience"
  :added "2021-01-06"
  :url "https://github.com/atykhonov/google-translate"
  :ensure t
  :require t
  )

(defun google-translate-auto ()
  "Automatically recognize and translate Japanese and English."
  (interactive)
  (if (use-region-p)
      (let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
        (deactivate-mark)
        (if (string-match (format "\\`[%s]+\\'" "[:ascii:]")
                          string)
            (google-translate-translate
             "en" "ja"
             string)
          (google-translate-translate
           "ja" "en"
           string)))
    (let ((string (read-string "Google Translate: ")))
      (if (string-match
           (format "\\`[%s]+\\'" "[:ascii:]")
           string)
          (google-translate-translate
           "en" "ja"
           string)
        (google-translate-translate
         "ja" "en"
         string)))))

;; Fix error of "Failed to search TKK"
(defun google-translate--get-b-d1 ()
  "Search TKK."
  (list 427110 1469889687))

;;;使い捨てファイルを開く
;;; TODO: org-modeで代替できそうなので将来削除するかも
(leaf open-junk-file
  :ensure t
  :config
  (setq open-junk-file-format "~/Documents/org/junk/%Y%m%d.")
  (setq open-junk-file-find-file-function 'find-file))

(leaf search-web
  :doc "Post web search queries using `browse-url'."
  :added "2020-12-31"
  :require t ; これがないと、うまくロードできなかった
  :ensure t
  :bind (("s-s" . hydra-search-web/body))
  :hydra
  (hydra-search-web (:color red :hint nil)
                    "
Search:  _a_mazon  _g_oogle  _t_ranslate  _e_ijiro  _m_aps  git_h_ub  _q_itta  _w_eblio  _p_〒  _k_古語  _r_類語 _._close"
                    ("a" (search-web-dwim "amazon jp"))
                    ("e" (search-web-dwim "eijiro"))
                    ("g" (search-web-dwim "google"))
                    ("m" (search-web-dwim "google maps"))
                    ("h" (search-web-dwim "github"))
                    ("q" (search-web-dwim "qitta"))
                    ("w" (search-web-dwim "weblio"))
                    ("p" (search-web-dwim "post"))
                    ("k" (search-web-dwim "kobun"))
                    ("r" (search-web-dwim "ruigo"))
                    ("t" google-translate-auto)
                    ("q" nil)
                    ("." nil :color blue))
  :config
  (add-to-list 'search-web-engines '("weblio" "http://weblio.jp/content/%s" nil))
  (add-to-list 'search-web-engines '("kobun" "http://kobun.weblio.jp/content/%s" nil))
  (add-to-list 'search-web-engines '("ruigo" "http://thesaurus.weblio.jp/content/%s" nil))
  (add-to-list 'search-web-engines '("github" "https://github.com/search?utf8=✓&q=%s&ref=simplesearch" nil))
  (add-to-list 'search-web-engines '("qitta" "https://qiita.com/search?q=%s" nil))
  (add-to-list 'search-web-engines '("post" "https://postcode.goo.ne.jp/search/q/%s/" nil))
  (add-to-list 'search-web-engines '("earth" "https://earth.google.com/web/search/%s/" nil)))

;;;---------------------

;; Use variable width font faces in current buffer
(defun my-buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face 'variable-pitch)
  (buffer-face-mode))

;; Use monospaced font faces in current buffer
(defun my-buffer-face-mode-fixed ()
  "Sets a fixed width (monospace) font in current buffer"
  (interactive)
  (setq buffer-face-mode-face 'fixed-pitch)
  (buffer-face-mode))

;;; TODO bundle
(defun my:current-font-object ()
  "print font object"
  (interactive)
  (message "%s" (font-xlfd-name (font-at (point)))))

(defun my:current-face()
  "print face"
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

(defun my:current-font-list()
  "print font list"
  (interactive)
  (message "%s" (dolist (x (x-list-fonts "*")) (print x))))

(global-set-key (kbd "s-u") 'hydra-my-util/body)

(defhydra hydra-my-util ()
  "utility"
  ("g" text-scale-increase "zoom in")
  ("l" text-scale-decrease "zoom out")
  ("t" variable-pitch-mode "toggle pitch")
  ("a" my:current-font-object "font object")
  ("b" my:current-face "face")
  ("c" my:current-font-list "font list")
  ("q" nil "quit")
  ("." nil :color blue))

;; Test
(global-set-key (kbd "s-b") 'hydra-bkmk/body)
(defhydra hydra-bkmk (:color pink :hint nil)
  "BookMark(bm/buildin)"
  ("m" bm-toggle "toggle" :column "marked")
  ("N" bm-lifo-next "LIFO N")
  ("P" bm-lifo-previous "LIFO P")
  ;;
  ("n" bm-common-next "next" :column "jump")
  ("p" bm-common-previous "prev")
  ("l" bm-show-all "list o/w")
  ;;
  ("s" bm-toggle-buffer-persistence "stay(toggle)" :column "edit")
  ("rr" bm-remove-all-current-buffer "remove c.buff")
  ("rR" bm-remove-all-all-buffers "remove a.buff")
  ;;
  ("aa" bm-bookmark-annotate "annotate" :column "annotate")
  ("aA" bm-bookmark-show-annotation "show anno")
  ;;
  ("xrl" bookmark-bmenu-list "blist" :exit t :column "Standard BkMk")
  ("xrm" bookmark-set "set current" :exit t)
  ("bo" bookmark-jump-other-window "jump other" :exit t)
  ("bc" counsel-bookmark "counsel bkmk")
  ("br" counsel-mark-ring "counsel mk ring")
  ;;
  ("q" nil "quit" :color blue :column "Quit")
  ("." nil "quit" :color blue)
  ("U" org-ctrl-c-ctrl-c "org ctl cc")
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
  :custom ((ivy-initial-inputs-alist . nil); defaultの部分一致から^strで先頭一致とするmode指定
           (ivy-use-virtual-buffers . t) ; add recentf/bookmarks
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

(leaf ivy-rich
  :doc "More friendly display transformer for ivy"
  :req "emacs-25.1" "ivy-0.13.0"
  :tag "ivy" "convenience" "emacs>=25.1"
  :added "2021-01-04"
  :url "https://github.com/Yevgnen/ivy-rich"
  :disabled t
  :emacs>= 25.1
  :ensure t
  :after ivy
  :config (ivy-rich-mode 1))

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

(leaf moody
  :doc "Tabs and ribbons for the mode line"
  :req "emacs-25.3"
  :tag "emacs>=25.3"
  :added "2021-01-03"
  :url "https://github.com/tarsius/moody"
  :emacs>= 25.3
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (setq moody-mode-line-height 24)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))


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

;; 各ウィンドウの左右の端に、狭いフリンジを
(set-fringe-mode 10)
;; カーソル形状
(setq-default cursor-type 'bar)
;; カーソル行ハイライト
(setq hl-line-face 'underline) ; 下線
(global-hl-line-mode)

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

(leaf yasnippet
  :doc "Yet another snippet extension for Emacs"
  :req "cl-lib-0.5"
  :tag "emulation" "convenience"
  :added "2021-01-01"
  :url "http://github.com/joaotavora/yasnippet"
  :ensure t)

;; language server protocol
;; M-x lsp
;; https://qiita.com/Ladicle/items/e666e3fb9fae9d807969
;; https://blog.takuchalle.dev/post/2018/12/11/emacs_lsp_mode/
;; https://blog.medalotte.net/archives/473
;; [TODO] company-lsp -> company-capf 
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
  :require t
  :hook   (lsp-mode . lsp-ui-mode)
  :costom (;; lsp-ui-doc
           (lsp-ui-doc-enable . t)
           (lsp-ui-doc-header . t)
           (lsp-ui-doc-include-signature . t)
           (lsp-ui-doc-position . 'top)
           (lsp-ui-doc-max-width  . 60)
           (lsp-ui-doc-max-height . 20)
           (lsp-ui-doc-use-childframe . t)
           (lsp-ui-doc-use-webkit . nil)
           
           ;; lsp-ui-flycheck
           (lsp-ui-flycheck-enable . t)
           
           ;; lsp-ui-sideline
           (lsp-ui-sideline-enable . t)
           (lsp-ui-sideline-ignore-duplicate . t)
           (lsp-ui-sideline-show-symbol . t)
           (lsp-ui-sideline-show-hover . t)
           (lsp-ui-sideline-show-diagnostics . t)
           (lsp-ui-sideline-show-code-actions . t)
           
           ;; lsp-ui-imenu
           (lsp-ui-imenu-enable . nil)
           (lsp-ui-imenu-kind-position . 'top)
           
           ;; lsp-ui-peek
           (lsp-ui-peek-enable . t)
           (lsp-ui-peek-always-show . t)
           (lsp-ui-peek-peek-height . 30)
           (lsp-ui-peek-list-width . 30)
           (lsp-ui-peek-fontify . 'always)))



(leaf lsp-ivy
  :doc "LSP ivy integration"
  :req "emacs-25.1" "dash-2.14.1" "lsp-mode-6.2.1" "ivy-0.13.0"
  :tag "debug" "languages" "emacs>=25.1"
  :added "2020-12-19"
  :url "https://github.com/emacs-lsp/lsp-ivy"
  :emacs>= 25.1
  :ensure t
  :after lsp-mode ivy)

(leaf company-lsp
  :doc "Company completion backend for lsp-mode."
  :req "emacs-25.1" "lsp-mode-6.0" "company-0.9.0" "s-1.2.0" "dash-2.11.0"
  :tag "emacs>=25.1"
  :added "2021-01-01"
  :url "https://github.com/tigersoldier/company-lsp"
  :emacs>= 25.1
  :ensure t
  :after lsp-mode company)

(leaf lsp-treemacs
  :doc "LSP treemacs"
  :req "emacs-26.1" "dash-2.14.1" "dash-functional-2.14.1" "f-0.20.0" "ht-2.0" "treemacs-2.5" "lsp-mode-6.0"
  :tag "languages" "emacs>=26.1"
  :added "2020-12-21"
  :disabled
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

(leaf markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (leaf markdown-toc :ensure t)
  (setq markdown-command "pandoc")
  (setq markdown-italic-underscore t)
  (setq markdown-asymmetric-header t)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-content-type "application/xhtml+xml")
  (setq markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
			     "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css"))
  (setq markdown-xhtml-header-content "
  <meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
  <style>
  body {
    box-sizing: border-box;
    max-width: 740px;
    width: 100%;
    margin: 40px auto;
    padding: 0 10px;
  }
  </style>
  <link rel='stylesheet' href='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/default.min.css'>
  <script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>
  <script>
  document.addEventListener('DOMContentLoaded', () => {
    document.body.classList.add('markdown-body');
    document.querySelectorAll('pre code').forEach((code) => {
      if (code.className != 'mermaid') {
        hljs.highlightBlock(code);
      }
    });
  });
  </script>
  <script src='https://unpkg.com/mermaid@8.4.8/dist/mermaid.min.js'></script>
  <script>
  mermaid.initialize({
    theme: 'default',  // default, forest, dark, neutral
    startOnLoad: true
  });
  </script>
  ")

  :bind ("C-x m" . hydra-markdown/body)
  :hydra
  (hydra-markdown
   (:color red :hint nil)
   "
    Markdown: _i_talic  消線:_x_  _f_ootnote  _t_able  t_o_c  _e_dit-code:_a_bort  pre_v_iew  md2_p_df  md2_d_ocx"
   ("i" markdown-insert-italic)
   ("x" markdown-insert-strike-through)
   ("t" markdown-insert-table)
   ("o" markdown-toc-generate-or-refresh-toc)
   ("f" markdown-insert-footnote)
   ("e" markdown-edit-code-block)
   ("a" edit-indirect-abort)
   ("v" markdown-preview)
   ;; Pndoc
   ("p" md2pdf)
   ("d" md2docx)
   ("<muhenkan>" nil))

  :init
  (defun md2pdf ()
    "Generate pdf from currently open markdown."
    (interactive)
    (let ((filename (buffer-file-name (current-buffer))))
      ;; Use wkhtmltopdf without latex
      (shell-command-to-string
       (concat "pandoc "
	       filename
	       " -f markdown -t html5 -o "
	       (file-name-sans-extension filename)
	       ".pdf"))
      (shell-command-to-string
       (concat "evince "
	       (file-name-sans-extension filename)
	       ".pdf"))))

  (defun md2docx ()
    "Generate docx from currently open markdown."
    (interactive)
    (let ((filename (buffer-file-name (current-buffer))))
      (shell-command-to-string
       (concat "pandoc "
	       filename
	       " -t docx -o "
	       (file-name-sans-extension filename)
	       ".docx -V mainfont=IPAPGothic -V fontsize=16pt --highlight-style=zenburn"))
      (shell-command-to-string
       (concat "xdg-open "
	       (file-name-sans-extension filename)
	       ".docx")))))

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

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (message "run org-font-setup")
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :fontset "fontset-VariableNoto" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;;;
;;; Org
;;; https://solist.work/blog/posts/information-organizize-method/
(leaf org
  :init
  (message "org init")
  (leaf org-plus-contrib :ensure t :require nil)
;;;  :leaf-autoload nil
  :bind (("C-c a" . org-agenda) ("C-c c" . org-capture))
  :hook (org-mode-hook . my-buffer-face-mode-variable)
  ;;  (org-mode-hook . efs/org-font-setup)
  :config
  (message "org config")
  (efs/org-font-setup)
  (message "-org config")
  ;; Easy Templates
  ;; Type C-c C-,
  (add-to-list 'org-structure-template-alist
               '("V" . "verbatim"))
  ;; org file default location
  (setq org-directory "~/Documents/org")
  ;; imenu max depth(Org headline)
  (setq org-imenu-depth 7)
  ;; Non-nil means adapt indentation to outline node level
  (setq org-adapt-indentation nil)
  ;; load org's modules
  ;; - ol-eww: make org-store-link' available on EWW
  ;; - ol-info:This file implements links to Info nodes from within Org mode.
  ;; - ol-docview: This file implements links to open files in doc-view-mode.
  (setq org-modules '(ol-info ol-docview ol-eww))
  ;;; Non-nil means make shift-cursor commands select text when possible.


  ;;; = Agenda =
  ;;; [TODO] https://tamura70.hatenadiary.org/entry/20100301/org
  ;;;; https://oremacs.com/2015/04/14/hydra-org-mode/
  ;;; C-c C-q : insert tags
  ;;; - Shift-right/left: TODO state change, Shift-right/down: priority
  (setq org-support-shift-select t)
  (setq org-log-done 'time)
  (setq org-use-speed-commands t)
  (setq org-src-fontify-natively t)
  ;; The files to be used for agenda display. Entry is directory or file.
  (setq org-agenda-files '("~/Documents/org"))
  (setq org-refile-targets '(org-agenda-files :maxlevel . 3))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(D)" "CANCEL(C)")
          (sequence "STUDY(s)" "|" "STUDIED(S)")          
          (sequence "WRITE(w)" "|" "WROTE(W)")
          (sequence "調査(i)" "|" "調査完(I)" "|" "記録(k)")
          ))
  (setq org-capture-templates
        '(("t" " Task/TODO")
          ("tt" " Task(Scheduled)" entry (file+headline "~/Documents/org/task.org" "")
           "** TODO %?\n SCHEDULED: %^t \n" :prepend t)
          ("td" " Todo" entry (file+headline "~/Documents/org/todo.org" "")
           "** TODO %?\n %U %i")
          ("td" " 調査" entry (file+headline "~/Documents/org/todo.org" "")
           "** 調査 %?\n %U %i")

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
          
          ("s" " Scrap" plain (file "~/Documents/org/scrap.org")
           "** Scrap %x" :immediate-finish t)
          
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
  (leaf ob
    :setq ((org-confirm-babel-evaluate . nil))
    :config
    (leaf ob-C
      :doc "Babel Functions for C and Similar Languages"
      :tag "out-of-MELPA" "reproducible research" "literate programming"
      :added "2021-01-05"
      :when (executable-find "C"))

    (leaf ob-shell
      :doc "Babel Functions for Shell Evaluation"
      :tag "out-of-MELPA" "reproducible research" "literate programming"
      :added "2021-01-05"
      :when (executable-find "shell"))

    (leaf ob-python
      :doc "Babel Functions for Python"
      :tag "out-of-MELPA" "reproducible research" "literate programming"
      :added "2021-01-05"
      :url "https://orgmode.org"
      :when (executable-find "python"))

    (leaf ob-emacs-lisp
      :doc "Babel Functions for Emacs-lisp Code"
      :tag "out-of-MELPA" "reproducible research" "literate programming"
      :added "2021-01-05"
      :url "https://orgmode.org"
      :when (executable-find "emacs-lisp"))

    (leaf ob-plantuml
      :doc "Babel Functions for Plantuml"
      :tag "out-of-MELPA" "reproducible research" "literate programming"
      :added "2021-01-05"
      :url "https://orgmode.org"
      :when (executable-find "plantuml")
      ;; TODO : install plantuml-jar file.
      ;;:setq ((org-plantuml-jar-path . plantuml-jar-path))
      :setq ((org-confirm-babel-evaluate . nil)))

    (leaf ob-calc
      :doc "Babel Functions for Calc"
      :tag "out-of-MELPA" "reproducible research" "literate programming"
      :added "2021-01-05"
      :url "https://orgmode.org"
      :when (executable-find "calc"))

    (leaf ob-dot
      :doc "Babel Functions for dot"
      :tag "out-of-MELPA" "reproducible research" "literate programming"
      :added "2021-01-05"
      :url "https://orgmode.org")

    (leaf ob-gnuplot
      :doc "Babel Functions for Gnuplot"
      :tag "out-of-MELPA" "reproducible research" "literate programming"
      :added "2021-01-05"
      :url "https://orgmode.org")

    (leaf ob-js
      :doc "Babel Functions for Javascript"
      :tag "out-of-MELPA" "js" "reproducible research" "literate programming"
      :added "2021-01-05"
      :url "https://orgmode.org")

    (leaf ob-latex
      :doc "Babel Functions for LaTeX"
      :tag "out-of-MELPA" "reproducible research" "literate programming"
      :added "2021-01-05"
      :url "https://orgmode.org")

    (leaf ob-makefile
      :doc "Babel Functions for Makefile"
      :tag "out-of-MELPA" "reproducible research" "literate programming"
      :added "2021-01-05"
      :url "https://orgmode.org"
      :when (executable-find "makefile"))

    (leaf ob-mscgen
      :doc "Babel Functions for Mscgen"
      :tag "out-of-MELPA" "reproducible research" "literate programming"
      :added "2021-01-05"
      :url "https://orgmode.org"
      :when (executable-find "makefile"))
    
    (org-babel-do-load-languages 'org-babel-load-languages
                                 '(
                                   (shell . t)
                                   (python . t)
                                   (plantuml . t); plantuml script(http://plantuml.sourceforge.net/) `org-plantuml-jar-path' should point to the jar file (when exec mode is `jar')
                                   (emacs-lisp . t)
                                   (C . t)
                                   (calc . t); Poorman's Mathematica
                                   (dot . t); DOT(http://www.graphviz.org/)
                                   (gnuplot . t); GnuPlot
                                   (js . t) ; Javascript
                                   (latex . t)
                                   (makefile . t)
                                   (mscgen . t)) ; message sequence charts(http://www.mcternan.me.uk/mscgen/index.html).
                                 ))
  ;;; TODO: setup https://github.com/conao3/orglyth.el
  ;;; Ref: https://qiita.com/conao3/items/f81cf964198d4da93a05
  ;;; https://qiita.com/conao3/items/f81cf964198d4da93a05

  :bind (("s-o" . hydra-org/body))
  :hydra
  (hydra-org (:color red :hint nil)
             "org-mode"
             ("t" (org-match-sparse-tree) "Search TAGs")
             ("s" (org-set-tags-command) "Set TAG")
             ("p" (org-set-property) "Set Property")
             ("d" (org-deadline) "Set Deadline")
             ("S" (call-interactively #'org-schedule) "Set SCHEDULED")
             ("c" (org-toggle-checkbox) "Checkbox")
             ("q" nil "quit")
             ("." nil :color blue))
  )


(leaf org-bullets
  :doc "Show bullets in org-mode as UTF-8 characters"
  :added "2021-01-04"
  :url "https://github.com/integral-dw/org-bullets"
  :ensure t
  :custom (org-bullets-bullet-list . '("◉" "○" "●" "○" "●" "○" "●"))
  :hook (org-mode-hook . org-bullets-mode)
  )

;; Ref. https://github.com/daviwil/emacs-from-scratch
(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(leaf visual-fill-column
  :doc "fill-column for visual-line-mode"
  :req "emacs-25.1"
  :tag "emacs>=25.1"
  :added "2021-01-04"
  :url "https://github.com/joostkremers/visual-fill-column"
  :emacs>= 25.1
  :ensure t
  :hook (org-mode . efs/org-mode-visual-fill))

;;; Modus Themes (Modus Operandi and Modus Vivendi)
;;; - high contrast theme
;;; https://protesilaos.com/modus-themes/

(leaf modus-themes
  :doc "Highly accessible themes (WCAG AAA)"
  :req "emacs-26.1"
  :tag "accessibility" "theme" "faces" "emacs>=26.1"
  :added "2021-01-03"
  :url "https://gitlab.com/protesilaos/modus-themes"
  :emacs>= 26.1
  :ensure t
  :bind ("<f2>" . modus-themes-toggle)
  :init
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil
        modus-themes-mode-line 'moody)
  :config
  ;; Load the theme of your choice
  (modus-themes-load-operandi)
  ;; ;; OR
  ;; (load-theme 'modus-operandi t)
  )

;;; Emacs session

;;; Run emacs without desktop : emacs --no-desktop
;;; Save : M-x desktop-save
;;; Change save path : M-x desktop-change-dir
;;; Revert : M-x desktop-revert
;;; Remove : M-x desktop-remove
;;; Clear : M-x desktop-clear
;;; See: https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
(desktop-save-mode 1)
