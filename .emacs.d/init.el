;;;;; init.el --- initialize emacs

;;;;; Commentary:
;;;; for Web/App develop and study some subjects.

;;;;; Code:
;;;; Prepare
;; Define user-emacs-directory if older than ver.23
(when (> emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d"))

;; Add load_path "~/.emacs.d/[elisp conf public_repos]"
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))
(add-to-load-path "elisp" "conf" "public_repos")

;; system-type predicates
(setq darwin-p  (eq system-type 'darwin)
      ns-p      (eq window-system 'ns)
      carbon-p  (eq window-system 'mac)
      w32-p     (eq window-system 'w32)
      linux-p   (eq system-type 'gnu/linux)
      cygwin-p  (eq system-type 'cygwin)
      nt-p      (eq system-type 'windows-nt)
      meadow-p  (featurep 'meadow)
      windows-p (or cygwin-p nt-p meadow-p w32-p))

;; Install package if isn't exist
(defvar my/favorite-packages
  '(
    ac-html
    all-ext
    async
    auto-complete
    coffee-mode
    dash
    emmet-mode
    epl
    expand-region
    flycheck
    geben
    hc-zenburn-theme
    helm
    helm-descbinds
    jade-mode
    markdown-mode
    multi-term
    multiple-cursors
    php-mode
    pkg-info
    plantuml-mode
    point-undo
    popup
    quickrun
    rainbow-mode
    rinari
    smartparens
    undo-tree
    undohist
    volatile-highlights
    yaml-mode
    web-mode
    )
  "My package list what are installed auto.")
(eval-when-compile
  (require 'cl))
(when (require 'package nil t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)
  (let ((pkgs (loop for pkg in my/favorite-packages
                    unless (package-installed-p pkg)
                    collect pkg)))
    (when pkgs
      ;; check for new packages (package versions)
      (message "%s" "Get latest versions of all packages...")
      (package-refresh-contents)
      (message "%s" " done.")
      (dolist (pkg pkgs)
        (package-install pkg)))))


;;;; theme
(load-theme 'hc-zenburn t)


;;;; Auto load major-modes

;; plantuml-mode
(autoload 'plantuml-mode "plantuml-mode" "PlantUML mode" t)
(add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
(setq plantuml-jar-path "/usr/local/jars/plantuml.jar")
(setq plantuml-java-options "")
(setq plantuml-options "-charset UTF-8 -ttxt")
(setq plantuml-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-c") 'plantuml-execute)
        map))
(eval-after-load "plantuml-mode"
  '(progn
     (defun plantuml-execute ()
       (interactive)
       (when (buffer-modified-p)
         (map-y-or-n-p "Save this buffer before executing PlantUML?"
                       'save-buffer (list (current-buffer))))
       (let ((code (buffer-string))
             out-file
             cmd)
         (when (string-match "^\\s-*@startuml\\s-+\\(\\S-+\\)\\s*$" code)
           (setq out-file (match-string 1 code)))
         (setq cmd (concat
                    "java -jar " plantuml-java-options " "
                    (shell-quote-argument plantuml-jar-path) " "
                    (and out-file (concat "-t" (file-name-extension out-file))) " "
                    plantuml-options " "
                    (buffer-file-name)))
         (message cmd)
         (shell-command cmd)
         (message "done")))
))


;; coffee-mode
(autoload 'coffee-mode "coffee-mode" "Major mode for editing by CoffeeScript." t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(eval-after-load "coffee-mode"
  '(progn
     (custom-set-variables
      '(tab-width 2)
      '(coffee-tab-width 2)
      '(coffee-args-compile '("-c" "--bare")))
     (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)
     (define-key coffee-mode-map (kbd "C-j") 'coffee-newline-and-indent)))

;; markdown-mode
(autoload 'markdown-mode "markdown-mode" "Major mode for editing by Markdown." t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(eval-after-load "markdown-mode"
  '(progn
     (setq markdown-css-paths "./css/md-default.css")
     (setq markdown-command "~/.nvm/v0.10.34/bin/marked")))

;; web-mode
(autoload 'web-mode "web-mode" "Major mode for editing web pages." t)
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
;; (set-face-attribute 'web-mode-symbol-face nil :foreground "#aa0")
;; (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "#777")
;; (set-face-attribute 'web-mode-html-tag-face nil :foreground "#aaa")

;; css-mode
(autoload 'css-mode "css-mode" "Major mode for editing by CSS." t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

;; php-mode
(autoload 'php-mode "php-mode" "Major mode for editing by PHP." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;; yaml-mode
(autoload 'yaml-mode "yaml-mode" "Major mode for editing by YAML." t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; jade-mode
(autoload 'jade-mode "jade-mode" "Major mode for editing by Jade." t)
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;; js2-mode
(autoload 'js2-mode "js2-mode" "Major mode for editing by JavaScript." t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(eval-after-load "js2-mode"
  '(progn
     (custom-set-variables
      '(tab-width 2)
      '(js2-basic-offset 2))))


;;;; Multi-term
(defun ad-advised-definition-p (def) "DEF is multi-term option." t)
(defun multi-term-dedicated-handle-other-window-advice (def) "DEF is multi-term option." t)
(when (require 'multi-term nil t)
  ;;    (setq multi-term-program "~/.bashrc")
  )
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-t") 'other-window)))


;;;; Helm
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files) ; Needn't
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-M-o") 'helm-occur)

(when (require 'helm nil t)
  (setq
   helm-idle-delay 0.1 ; show time delay
   helm-input-idle-delay 0.1 ; re-display time delay
   helm-candidate-number-limit 100 ; limit number of candidate
   helm-quick-update t ; performance up when too many candidate
   ;;   helm-enable-shortcuts 'alphabet
   ) ; choose key get be alphabet

  (when (require 'helm-config nil t)
    (setq helm-su-or-sudo "sudo")) ; execute sudo when root authority

  (require 'helm-match-plugin nil t)

  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (require 'helm-migemo nil t))

  (require 'helm-show-completion nil t)

  (when (require 'auto-install nil t)
    (require 'helm-auto-install nil t))

  ;; for helm
  ;; (when (require 'color-moccur nil t)
  ;;   (require 'helm-regexp)
  ;;   (define-key helm-moccur-map (kbd "C-s") 'moccur-from-helm-moccur))
  ;; (helm-lisp-complete-symbol-set-timer 150)

  (when (require 'helm-descbinds nil t)
    (helm-descbinds-mode))) ; describe-bindings change to helm


;;;; Require
;; all-ext
(require 'all-ext)

;; quickrun
(when (require 'quickrun nil t)
  (define-key global-map [f5] 'quickrun))

;; point-undo
(when (require 'point-undo nil t)
  (define-key global-map [f1] 'point-undo)
  (define-key global-map [f2] 'point-redo))

;; auto-complete
(when (require 'auto-complete-config nil t)
  (ac-config-default)
  (setq ac-auto-show-menu 0.3)
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (define-key ac-completing-map (kbd "C-p") 'ac-previous)
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  
  (add-hook 'html-mode-hook 'ac-html-enable)
  (add-hook 'web-mode-hook 'ac-html-enable))
(eval-after-load "web-mode"
  '(progn
     (add-to-list 'web-mode-ac-sources-alist
                  '("html" . (
                              ;; attribute-value better to be first
                              ac-source-html-attribute-value
                              ac-source-html-tag
                              ac-source-html-attribute)))))

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)

;; undohist
(require 'undohist)
(undohist-initialize)

;; volatile-highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)
(set-face-attribute 'vhl/default-face nil :background "SkyBlue4")

;; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)

;; moccur
(when (and (require 'color-moccur nil t)
           (require 'moccur-edit nil t))
  ;; AND検索
  (setq moccur-split-word t)
  ;; ディレクトリ検索のときに除外するファイル
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store"))

;; wdired
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
(define-key dired-mode-map (kbd "C-t") 'other-window)
(setq ls-lisp-use-insert-directory-program nil) ; not use `ls --dired`
(require 'ls-lisp)

;; wgrep
(require 'wgrep nil t)

;; rainbow-mode
(when (require 'rainbow-mode nil t)
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode))


;;;; Usability
;; Set meta key on Mac's command key
(when darwin-p
  (setq ns-command-modifier (quote meta)))

;; On カーソルコピー
(setq mouse-drag-copy-region t)

;; On share clipboard for emacs
(when darwin-p
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))
  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))
(when linux-p
  (setq x-select-enable-clipboard t))


;; Show existing キーバインド when push 'M-x ...'
(setq suggest-key-bindings 3) ; for 3 seconds

;; On バックアップ・自動保存 in temporary directory
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))
(setq auto-save-default t)     ; on
(setq make-backup-files t)     ; on
(setq vc-make-backup-files t)  ; on

;; Mac で GUI から起動しても、シェルの PATH 環境変数を引き継ぐ
;; - http://qiita.com/catatsuy/items/3dda714f4c60c435bb25
(defun exec-path-from-shell-initialize ()
  "Set up PATH by the user's shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; On 他で変更されたら再読込
(global-auto-revert-mode +1)

;; On 補完時の大文字小文字区別
(setq completion-ignore-case t)
;; (setq read-file-name-completion-ignore-case t)
;; (setq read-buffer-completion-ignore-case t)

;; On 検索時の大文字小文字の区別
(setq-default case-fold-search t)

;; On タブではなくスペースを使う
;; (setq default-tab-width 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'indent-relative-maybe)

;; ediff
;; コントロール用のバッファを同一フレーム内に表示
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; diffのバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)


;;;; Hook
;; Chmod +x when save script file
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Display value of elisp
(defun elisp-mode-hooks ()
  "It's elisp-mode-hooks."
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)

;; Don't auto-delete-whitespace on markdown-mode
(defun markdown-turn-off-whitespace-action ()
  "It's markdown-mode-turn-off-whitespace-action-hook."
  (setq whitespace-action nil))
(add-hook 'markdown-mode-hook 'markdown-turn-off-whitespace-action)

;; As soon as compile then save on markdown-mode
(eval-after-load "markdown-mode"
  '(progn
     (defun markdown-auto-compile-hook ()
       "It's markdown-auto-compile-hook."
       (when (eq major-mode 'markdown-mode)
         (markdown-export) nil))
     (add-hook 'after-save-hook 'markdown-auto-compile-hook)))

;; tab-width each mode
(add-hook 'web-mode-hook '(lambda ()
                            (setq tab-width 2)
                            (setq web-mode-enable-css-colorization t)
                            (setq web-mode-markup-indent-offset 2)))

;; flycheck mode
(add-hook 'after-init-hook #'global-flycheck-mode)


;;;; Interface
;; Set 2:1 for helf-char/full-char size
(when darwin-p
  (setq face-font-rescale-alist
        '((".*Menlo.*" . 1.0)
          (".*Hiragino_Mincho_ProN.*" . 1.2)
          (".*nfmotoyacedar-bold.*" . 1.2)
          (".*nfmotoyacedar-medium.*" . 1.2)
          (".*-cdac$.*" . 1.3))))

;; On 行番号の表示 for 4 rows
(global-linum-mode t)
(set-face-attribute 'linum nil
                    :foreground "#555")
(setq linum-format "%4d")

;; On 対応するカッコを強調表示
(setq show-parent-delay 0)
(show-paren-mode t)
(setq show-paren-style 'mixed) ; `parenthesis' or `expression' or `mixed'
;; (set-face-background 'show-paren-match-face "#07a")
                                        ;(set-face-underline-p 'show-paren-match-face "yellow")

;; Show file path on title bar
                                        ;(setq frame-title-format "%f")

;; Set region color
                                        ; (set-face-background 'region "#22a")

;; Set highlight on current row
(global-hl-line-mode t)
(set-face-background 'hl-line "#3a3f3d")
;;(set-face-attribute 'highlight nil :foreground 'unspecified)
;; (set-face-foreground 'highlight nil)
;; (set-face-background 'highlight "#f00")
;; (custom-set-faces
;; '(highlight ((t (:background "#f00" :foreground "black" :bold t))))
;; )
;; (set-face-underline-p 'highlight "#aaf")

;; hilight trailing whitespace
(setq-default show-trailing-whitespace t)
(set-face-attribute 'trailing-whitespace nil
                    :background "#444444")

;; yes or no -> y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; height between row and row
                                        ; (setq-default line-spacing 0)

;; enable rectangular selection
(cua-mode t)
(define-key global-map (kbd "C-RET") 'cua-set-rectangle-mark)
(setq cua-enable-cua-keys nil)

;; hide startup page
(setq inhibit-startup-screen t)

;; hide scratch initial message
(setq initial-scratch-message "")

;; hide menu bar
(menu-bar-mode 0)


;;;; Use UTF-8
;; Prefer utf-8
(prefer-coding-system 'utf-8)
;; ターミナルの文字コード
(set-terminal-coding-system 'utf-8)
;; キーボードから入力される文字コード
(set-keyboard-coding-system 'utf-8)
;; ファイルのバッファのデフォルト文字コード
(set-buffer-file-coding-system 'utf-8)
;; バッファのプロセスの文字コード
(setq default-buffer-file-coding-system 'utf-8)
;; フ<<ァイル名の文字コード
(setq file-name-coding-system 'utf-8)
;; Set File name on Mac *after Prefer utf-8
(when darwin-p
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
;;Set File name on Win *after Prefer utf-8
(when windows-p
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))


;;;; Display on mode line
;; columns/rows number
(column-number-mode t)
;; file size
(size-indication-mode t)
;; count lines/chars number with region
(defun count-lines-and-chars () "Show amount of lines & chars."
  (if mark-active
      (format "%d lines, %d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ;; bright echo area
    ;;(count-lines-region (region-beginning) (region-end))
    ""))
(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))


;;;; Keybind
;; Delete backword on C-h
(keyboard-translate ?\C-h ?\C-?)
;; Delete word on M-h
(define-key global-map (kbd "M-h") 'backward-kill-word)
;; Toggle return at max rows on 'C-c l'
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
;; Change emacs window on 'C-t'
(define-key global-map (kbd "C-t") 'other-window)
;; Help key on 'C-x ?'
(define-key global-map (kbd "C-x ?") 'help-command)


;;;; My function
(defun align-regexp-repeated (start stop regexp)
  "Like 'align-regexp', but repeated for multiple columns from START - STOP with REGEXP.  See http://www.emacswiki.org/emacs/AlignCommands."
  (interactive "r\nsAlign regexp: ")
  (let ((spacing 1)
        (old-buffer-size (buffer-size)))
    ;; If our align regexp is just spaces, then we don't need any
    ;; extra spacing.
    (when (string-match regexp " ")
      (setq spacing 0))
    (align-regexp start stop
                  ;; add space at beginning of regexp
                  (concat "\\([[:space:]]*\\)" regexp)
                  1 spacing t)
    ;; modify stop because align-regexp will add/remove characters
    (align-regexp start (+ stop (- (buffer-size) old-buffer-size))
                  ;; add space at end of regexp
                  (concat regexp "\\([[:space:]]*\\)")
                  1 spacing t)))

(defun my-window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-char (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))

;;;; Fix error
;;(let ((gls "/usr/local/bin/gls"))
;;  (if (file-exists-p gls) (setq insert-directory-program gls)))
(defvar ruby-keyword-end-re "nil")

;;; init.el ends here
