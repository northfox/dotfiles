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
    markdown-mode
    multi-term
    multiple-cursors
    pkg-info
    popup
    smartparens
    undo-tree
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


;;;; Auto mode

;; md
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-css-path
      "~/devtool/libs/css/md-default.css")


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

  (when (require 'descbinds-helm nil t)
    descbinds-helm-install)) ; describe-bindings change to helm


;;;; Require
;; auto-complete
(require 'auto-complete-config)
(ac-config-default)


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
;; On 行番号の表示
(global-linum-mode t)
(setq linum-format "%4d:") ; for 4 rows

;; On 対応するカッコを強調表示
(setq show-parent-delay 0)
(show-paren-mode t)
(setq show-paren-style 'mixed) ; `parenthesis' or `expression' or `mixed'
(set-face-background 'show-paren-match-face "#07a")
  ;(set-face-underline-p 'show-paren-match-face "yellow")

;; Show file path on title bar
  ;(setq frame-title-format "%f")

;; Set region color
(set-face-background 'region "#22a")
;(set-face-background 'region "color-17")

;; Set highlight on current row
(set-face-background 'highlight "#222")
(set-face-foreground 'highlight nil)
  ;(set-face-underline-p 'highlight "#aaf")
(global-hl-line-mode t)


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
