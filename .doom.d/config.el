;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
(setq doom-theme 'doom-snazzy)
(setq tab-width 2)
(setq evil-auto-indent nil)
(global-prettify-symbols-mode t)
(setq-default prettify-symbols-alist '(("lambda" . ?λ)
                                       ("->" . ?→)
                                       ("->>" . ?↠)
                                       ("=>" . ?⇒)
                                       ("/=" . ?≠)
                                       ("!=" . ?≠)
                                       ("==" . ?≡)
                                       ("<=" . ?≤)
                                       (">=" . ?≥)
                                       ("=<<" . ?≪)
                                       (">>=" . ?≫)
                                       ("<=<" . ?↢)
                                       (">=>" . ?↣)
                                       ("&&" . ?∧)
                                       ("||" . ?∨)
                                       ("not" . ?¬)))
(setq plantuml-output-type "png")
;; FUNCTIONS
(defun golem/setup-tide-mode ()
  (interactive)
  (setq flycheck-checker 'javascript-eslint)
  (setq tide-format-options '(:indentSize 2 :tabSize 2))
  (setq typescript-indent-level 2)
  (flycheck-add-next-checker 'javascript-eslint 'typescript-tide)

;;  (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;  (setq flycheck-checker 'javascript-eslint)
  )
(defun golem/typescript-eslint-fix-file ()
  (interactive)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint
          (and root
               (expand-file-name "node_modules/.bin/eslint"
                                 root))))
         (shell-command (concat eslint " --fix " (buffer-file-name)))
         (message (concat eslint " --fix " (buffer-file-name)))
         )
  (revert-buffer (buffer-file-name) t)
  )
(defun golem/tide-format ()
  (interactive)
(if (bound-and-true-p tide-mode)
    (lambda ()
      (tide-format)
      (save-buffer)
      (revert-buffer (buffer-file-name) t)
      )
  )
)
;; HOOKS
;;

(add-hook 'tide-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'golem/tide-format)
            (add-hook 'tide-mode-hook 'golem/setup-tide-mode)
            ))

;; INITS
(with-eval-after-load 'projectile
     (define-key projectile-mode-map [f6] 'projectile-test-project)
     (define-key projectile-mode-map [f5] 'projectile-run-project)
)

(with-eval-after-load 'tide
  (setq tide-format-options '(:indentSize 2 :tabSize 2))
)
