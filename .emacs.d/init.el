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

;; Install package if isn't exist
(defvar my/favorite-packages
  '(
    async
    auto-complete
    dash
    emmet-mode
    epl
    expand-region
    flycheck
    helm
    helm-descbinds
    markdown-mode
    multi-term
    multiple-cursors
    pkg-info
    popup
    smartparens
    undo-tree
    web-mode
    volatile-highlights
    undohist
    )
    "package list for auto install")
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


;;;; Auto mode

;; markdown-mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-css-path
      "./css/md-default.css")

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
;; (set-face-attribute 'web-mode-symbol-face nil :foreground "#aa0")
;; (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "#777")
;; (set-face-attribute 'web-mode-html-tag-face nil :foreground "#aaa")

;;;; Multi-term
(defun ad-advised-definition-p (def) t)
(defun multi-term-dedicated-handle-other-window-advice (def) t)
  (when (require 'multi-term nil t)
    ;;    (setq multi-term-program "~/.bashrc")
    )
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-t") 'other-window)))


;;;; Helm
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
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

  (when (require 'helm-complete nil t)
    (helm-lisp-complete-symbol-set-timer 150)) ; re-search lisp symbol candidate

  (require 'helm-show-completion nil t)

  (when (require 'auto-install nil t)
    (require 'helm-auto-install nil t))

  ;; for helm
  ;; (when (require 'color-moccur nil t)
  ;;   (require 'helm-regexp)
  ;;   (define-key helm-moccur-map (kbd "C-s") 'moccur-from-helm-moccur))

  (when (require 'helm-descbinds nil t)
    (helm-descbinds-mode))) ; describe-bindings change to helm


;;;; Require
;; auto-complete
(require 'auto-complete-config)
(ac-config-default)

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


;;;; Usability
;; Set meta key on Mac's command key
(when (eq system-type 'darwin)
    (setq ns-command-modifier (quote meta)))

;; On カーソルコピー
(setq mouse-drag-copy-region t)

;; Show existing キーバインド when push 'M-x ...'
(setq suggest-key-bindings 5) ; for 5 seconds

;; On バックアップ・自動保存 in temporary directory
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))
(setq auto-save-default t)     ; on
(setq make-backup-files t)     ; on
(setq vc-make-backup-files t)  ; on

;; Mac で GUI から起動しても、シェルの PATH 環境変数を引き継ぐ
; - http://qiita.com/catatsuy/items/3dda714f4c60c435bb25
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
(setq-default tab-width 2)
(setq default-tab-width 2)
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'indent-relative-maybe)

;; hide startup page
(setq inhibit-startup-screen t)

;; hide scratch initial message
(setq initial-scratch-message "")


;;;; Hook
;; Chmod +x when save script file
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Display value of elisp
(defun elisp-mode-hooks ()
  "elisp-mode-hooks"
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)

;; Don't auto-delete-whitespace on markdown-mode
(defun markdown-turn-off-whitespace-action ()
  "markdown-mode-turn-off-whitespace-action-hook"
  (setq whitespace-action nil))
(add-hook 'markdown-mode-hook 'markdown-turn-off-whitespace-action)

;; As soon as compile then save on markdown-mode
(defun markdown-auto-compile-hook ()
  "markdown-auto-compile-hook"
  (when (eq major-mode 'markdown-mode)
    (markdown-export) nil))
(add-hook 'after-save-hook 'markdown-auto-compile-hook)


;;;; Interface
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

;; yes or no -> y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; height between row and row
  ; (setq-default line-spacing 0)

;; enable rectangular selection
  ;(cua-mode t)
  ;(setq cua-enable-cua-keys (kbd "C-M-@"))


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
;; ファイル名の文字コード
(setq file-name-coding-system 'utf-8)
;; Set File name on Mac *after Prefer utf-8
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
;;Set File name on Win *after Prefer utf-8
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))


;;;; Display on mode line
;; columns/rows number
(column-number-mode t)
;; file size
(size-indication-mode t)
;; count lines/chars number with region
(defun count-lines-and-chars ()
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
;; Toggle return at max rows on 'C-c l'
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
;; Change emacs window on 'C-t'
(define-key global-map (kbd "C-t") 'other-window)
;; Help key on 'C-x ?'
(define-key global-map (kbd "C-x ?") 'help-command)

