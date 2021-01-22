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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                      ;
;                     GLOBALS                          ;
;                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq debug-on-error t)
(setq projectile-globally-ignored-directories '("~/.emacs.d/.local" "*node_modules" "dist"))
(global-flycheck-mode t)
(exec-path-from-shell-initialize)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq doom-theme 'doom-snazzy)
(setq tab-width 2)
(setq evil-auto-indent nil)
(setq treemacs-width 30)
(setq highlight-indent-guides-method 'character)
(global-prettify-symbols-mode t)
(global-company-mode t)
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

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;; FUNCTIONS
(defun golem/treemacs (&optional args)
  (interactive)
  (if (eq (treemacs-current-visibility) 'visible)
      (progn
        (evil-window-next 0)
        (treemacs-display-current-project-exclusively)
        )
      )
  )
(defun golem/project-setup ()
  (interactive)
  (treemacs)
  (golem/project-setup-terminal)
  (golem/project-setup-flycheck)
  )

(defun golem/project-setup-terminal ()
  (interactive)
  (add-to-list 'display-buffer-alist
             `(,"^\\*doom:\\(?:v?term\\|e?shell\\)-popup"
               (+popup-buffer)
               (actions)
               (side . bottom)
               (size . 0.15)
               (window-width . 40)
               (window-height . 0.35)
               (slot)
               (vslot . -5)
               (window-parameters
                (ttl)
                (quit)
                (select . t)
                (modeline)
                (autosave)
                (transient . t)
                (no-delete-other-windows . t)
                (no-other-window . t))))
  (+eshell/toggle nil)
  )
(defun golem/project-setup-flycheck ()
  (interactive)
  (flycheck-mode +1)
  (add-to-list 'display-buffer-alist
               `(,"^\\*Flycheck errors\\*$"
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . right)
                 (size . 0.15)
                 (reusable-frames . visible)
                 (window-width   . 0.25)
                 (window-parameters
                  (no-delete-other-windows . t)
                  )
                 ))
  (flycheck-list-errors)
  )

;; MODES
(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode)
  )
;; ADVICES
(advice-add 'treemacs :after #'golem/treemacs)
;; HOOKS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                      ;
;                   KEYBINDS                           ;
;                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(map! :map projectile-mode-map :n "SPC p w" #'+helm/project-search)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                      ;
;                       JAVA                           ;
;                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun golem/setup-java-mode ()
  (interactive)
  (require 'dap-java)
  (subword-mode)
  (dap-mode)
  (dap-ui-mode)
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)
  (ggtags-mode)
  )
(add-hook 'java-mode-hook 'golem/setup-java-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                      ;
;                       RUBY                           ;
;                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(push 'company-robe company-backends)
(defun golem/setup-ruby-mode ()
  (interactive)
  (flycheck-mode t)
  (robe-mode)
  (company-mode +1)
  (robe-start t)
  )
(add-hook 'ruby-mode-hook 'golem/setup-ruby-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                      ;
;                    JAVASCRIPT                        ;
;                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun golem/tide-format ()
  (interactive)
  (if (bound-and-true-p tide-mode)
    ;  (tide-format)
    ;  (abbrev-edit-save-buffer)
    ;  (revert-buffer (buffer-file-name) t)
      (golem/typescript-eslint-fix-file)
  )
)

(defun golem/setup-tide-mode ()
  (interactive)
  (tide-setup)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (setq flycheck-checker 'javascript-eslint)
  (setq tide-format-options '(:indentSize 2 :tabSize 2))
  (setq typescript-indent-level 2)
  (flycheck-add-next-checker 'javascript-eslint 'typescript-tide)
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
         (shell-command (concat eslint " --fix " (buffer-file-name)) nil nil)
         (revert-buffer (buffer-file-name) t)
    ;(set-process-sentinel
    ;     (start-process-shell-command "Shell" "*Output*" (concat eslint " --fix " (buffer-file-name)))
    ;     (lambda (_ _) (revert-buffer (buffer-file-name) t))
    ;     ;(message (concat eslint " --fix " (buffer-file-name)))
    ;     ;(revert-buffer (buffer-file-name) t)
    ;     )
    )
  )

(add-hook 'typescript-tsx-mode-hook
          (lambda ()
            (golem/setup-tide-mode)
            ))
;; INITS
(with-eval-after-load 'projectile
  (define-key projectile-mode-map [f6] 'projectile-test-project)
  (define-key projectile-mode-map [f5] 'projectile-run-project)
  (define-key projectile-mode-map [f4] 'projectile-compile-project)
  (define-key projectile-mode-map [f9] 'treemacs)
)

(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map [f1] 'treemacs-create-file)
  (define-key treemacs-mode-map [f2] 'treemacs-create-dir)
)

(with-eval-after-load 'tide
  (setq tide-format-options '(:indentSize 2 :tabSize 2))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                      ;
;                MISC/AUTOMATED                        ;
;                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Overriding doom internals

;; Shell window height. Structure is the same, values are different
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((projectile-project-compilation-cmd . "mvn -DskipTests package")
     (projectile-project-run-cmd . "mvn")
     (projectile-project-compilation-cmd . "mvn package")
     (projectile-project-test-cmd . "mvn test")
     (eval advice-add 'projectile-test-project :before
           (lambda
             (&rest r)
             (setenv "TESTREGEXP"
                     (read-string "Execute file pattern: "))))
     (projectile-project-test-cmd . "make test"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
