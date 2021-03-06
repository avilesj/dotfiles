
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(setq debug-on-error nil)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/emacs-golem")
(require 'golem)
(when (eq system-type 'darwin) ;; mac specific settings
  (set-keyboard-coding-system nil)
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (setq which-key-show-transient-maps nil)
)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))



;; General Settings

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#1D252C" "#D95468" "#8BD49C" "#EBBF83" "#5EC4FF" "#E27E8D" "#70E1E8" "#A0B3C5"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes '(doom-molokai))
 '(custom-safe-themes
   '("f951343d4bbe5a90dba0f058de8317ca58a6822faa65d8463b0e751a07ec887c" "1728dfd9560bff76a7dc6c3f61e9f4d3e6ef9d017a83a841c117bd9bebe18613" "67798cfaf7b064072bf519ed1ade02a8f4412df89c560e35f25d1936cf35b8ce" "6bc387a588201caf31151205e4e468f382ecc0b888bac98b2b525006f7cb3307" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(fci-rule-color "#383838")
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(jdee-db-active-breakpoint-face-colors (cons "#10151C" "#5EC4FF"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#10151C" "#8BD49C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#10151C" "#384551"))
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(objed-cursor-color "#D95468")
 '(org-agenda-files
   '("~/org-files/Home.org" "~/org-files/Amadita.org" "/Users/joseaviles/org-files/Desk.org" "/Users/joseaviles/org-files/Organize.org"))
 '(package-selected-packages
   '(apib-mode plantuml-mode sbt-mode company-lsp lsp-mode go-mode expand-region rainbow rainbow-mode doom-themes company-tern js-mode load-env-vars typescript-mode exec-path-from-shell yaml-mode org-present yasnippet yasnipper org-bullets multiple-cursors swiper treemacs-projectile treemacs eyebrowse doom-modeline prettier-js popup-kill-ring symon list-packages-ext spaceline company-mode rainbow-delimiters rjsx-mode avy ido-vertical-mode ace-jump-mode beacon use-package spacemacs-theme which-key magit zenburn-theme web-mode web elpa-mirror elpa-clone elpa-audit flycheck projectile ## cyberpunk-theme company tide org))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(plantuml-jar-args nil)
 '(sql-postgres-login-params
   '((database :default "joseaviles" :completion
               #[771 "\211\242\302=\206
 \211\303=?\2053 r\301\204 p\202( \304 \305!\203% \306!\202& p\262q\210\307\300!$)\207"
                     [#[257 "\300 \207"
                            [sql-postgres-list-databases]
                            2 "

(fn _)"]
                      nil boundaries metadata minibuffer-selected-window window-live-p window-buffer complete-with-action]
                     8 "

(fn STRING PRED ACTION)"]
               :must-match confirm)))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
