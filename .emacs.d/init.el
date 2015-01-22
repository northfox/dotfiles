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

;; On パッケージ管理
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


;;;; Usability
;; Set meta key on Mac's command key
(when (eq system-type 'darwin)
    (setq ns-command-modifier (quote meta)))

;; On カーソルコピー
(setq mouse-drag-copy-region t)

;; Show existing キーバインド when push 'M-x ...'
(setq suggest-key-bindings 5) ; for 5 seconds

;; バックアップ・自動保存
(setq auto-save-default t)      ; on
(setq make-backup-files nil)    ; off
(setq vc-make-backup-files nil) ; off

;; Mac で GUI から起動しても、シェルの PATH 環境変数を引き継ぐ
;; - http://qiita.com/catatsuy/items/3dda714f4c60c435bb25
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; On 行番号の表示
(global-linum-mode t)
(setq linum-format "%4d:") ; for 4 rows

;; On 対応するカッコを強調表示
(show-paren-mode t)
(setq show-paren-style 'mixed) ; `parenthesis' or `expression' or `mixed'

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

;; Use UTF-8
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

;; Set delete backward on C-h
(define-key global-map "\C-h" 'delete-backward-char)
;; Set help on C-?
(define-key global-map "\M-?" 'help-for-help)

