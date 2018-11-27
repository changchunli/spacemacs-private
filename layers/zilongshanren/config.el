(defvar org-agenda-dir ""
  "gtd org files location")

(defvar deft-dir ""
  "deft org files locaiton")

(defvar blog-admin-dir ""
  "blog-admin files location")

(if (and (spacemacs/system-is-mswindows) window-system)
  (setq
 org-agenda-dir "f:/org"
 deft-dir "f:/org"
 blog-admin-dir "f:/william.com")
  (setq
 org-agenda-dir "~/org"
 deft-dir "~/org"
 blog-admin-dir "~/william.com"))

;;load sensitive data
;; or (like spacemacs init.el) put the above variable into it, then the own value separated from public config
;; .emacs.secrets.el for example:
;; (setq-default
;;  org-agenda-dir "~/Dropbox/Apps/emacs/gtd"
;;  deft-dir "~/Dropbox/Apps/emacs/notes"
;;  blog-admin-dir "~/Documents/hexo"
;;  )
;; (slack-register-team
;;   :name "emacs-slack"
;;   :default t
;;   :client-id "xxxxxxxxx"
;;   :client-secret "xxxxxxxxx"
;;   :token "xxxxxxxxx"
;;   :subscribed-channels '(xxxxxxxxx))
;; (setq paradox-github-token "")
;; (load "~/Dropbox/Apps/emacs/.emacs.secrets.el" t)
