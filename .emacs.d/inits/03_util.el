
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
  :custom ((dashboard-set-heading-icons . t)
           (dashboard-center-content . t)
           ;; LANG=Cかつset-language-environment設定でやっと正常表示、
           ;; 面倒なので水平線表示しないようにする
           (dashboard-page-separator . "\n\n") 
           (dashboard-set-file-icons . t)
           (dashboard-items . '((recents  . 5)
                                (bookmarks . 5)
                                (projects . 8)
                                (agenda . 5)))))


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

;; leafでうまくロードできず,やむなくload-pathに配置している
(require 'google-translate)
(require 'google-translate-default-ui)
(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)

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

;;
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
  ("c" my:current-font-list "font list"))
