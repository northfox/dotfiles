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


;;;; auto mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


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

;; Show file path on title bar
(setq frame-title-format "%f")

;; Set region color
(set-face-background 'region "color-17")

;; Set highlight on current row
(set-face-background 'highlight "#222")
(set-face-foreground 'highlight nil)
(set-face-underline-p 'highlight t)
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
