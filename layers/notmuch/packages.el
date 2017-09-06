;;; packages.el --- notmuch layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author:  <changchunli93@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `notmuch-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `notmuch/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `notmuch/pre-init-PACKAGE' and/or
;;   `notmuch/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

;; (defconst notmuch-packages
;;   '()
;;   "The list of Lisp packages required by the notmuch layer.

;; Each entry is either:

;; 1. A symbol, which is interpreted as a package to be installed, or

;; 2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
;;     name of the package to be installed or loaded, and KEYS are
;;     any number of keyword-value-pairs.

;;     The following keys are accepted:

;;     - :excluded (t or nil): Prevent the package from being loaded
;;       if value is non-nil

;;     - :location: Specify a custom installation location.
;;       The following values are legal:

;;       - The symbol `elpa' (default) means PACKAGE will be
;;         installed using the Emacs package manager.

;;       - The symbol `local' directs Spacemacs to load the file at
;;         `./local/PACKAGE/PACKAGE.el'

;;       - A list beginning with the symbol `recipe' is a melpa
;;         recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(setq notmuch-packages
      '(notmuch
        counsel
        company
        yasnippet
        org-mime
        org-notmuch
        helm-notmuch
        gnus-alias
        org
        avy
        wid-edit
        persp-mode
        notmuch-labeler
        ))

(defun notmuch/post-init-org-mime ()
  (use-package org-mime
    :after notmuch
    :init (setq org-mime-library 'mml)
    )
  )

(defun notmuch/post-init-company-mode ()
  (spacemacs|add-company-backends
    :backends (company-capf notmuch-company company-yasnippet)
    :modes notmuch-message-mode
    )
  )

(defun notmuch/post-init-yasnippet-mode ())


(defun notmuch/post-init-persp-mode ()
  ;; do not save erc buffers
  (with-eval-after-load 'persp-mode
    (push (lambda (b) (with-current-buffer b (eq major-mode 'notmuch-hello-mode))) persp-filter-save-buffers-functions)
    (push (lambda (b) (with-current-buffer b (eq major-mode 'notmuch-tree-mode))) persp-filter-save-buffers-functions)
    (push (lambda (b) (with-current-buffer b (eq major-mode 'notmuch-search-mode))) persp-filter-save-buffers-functions)
    (push (lambda (b) (with-current-buffer b (eq major-mode 'notmuch-show-mode))) persp-filter-save-buffers-functions)
    (push (lambda (b) (with-current-buffer b (eq major-mode 'notmuch-message-mode))) persp-filter-save-buffers-functions)
    )

  (spacemacs|define-custom-layout "@Notmuch"
    :binding "n"
    :body
    (progn
      (defun spacemacs-layouts/add-notmuch-buffer-to-persp ()
        (persp-add-buffer (current-buffer) (persp-get-by-name "@Notmuch")))
      (add-hook 'notmuch-tree-mode-hook #'spacemacs-layouts/add-notmuch-buffer-to-persp)
      (add-hook 'notmuch-search-mode-hook #'spacemacs-layouts/add-notmuch-buffer-to-persp)
      (add-hook 'notmuch-hello-mode-hook #'spacemacs-layouts/add-notmuch-buffer-to-persp)
      (add-hook 'notmuch-show-mode-hook #'spacemacs-layouts/add-notmuch-buffer-to-persp)
      (add-hook 'notmuch-message-mode-hook #'spacemacs-layouts/add-notmuch-buffer-to-persp)
      (call-interactively 'notmuch)
      )))

;; For each package, define a function notmuch/init-<package-notmuch>
(defun notmuch/post-init-counsel ())
(defun notmuch/post-init-avy ()
  (use-package avy
    :ensure t
    :config
    (progn
      (defun ace-link--notmuch-hello-collect ()
        "Collect the positions of visible links in *notmuch-hello*."
        (let (candidates pt)
          (save-excursion
            (save-restriction
              (goto-char (point-min))
              (setq pt (point))
              (while (progn (widget-forward 1)
                            (> (point) pt))
                (setq pt (point))
                (when (get-char-property (point) 'button)
                  (push (point) candidates)))))
          (nreverse candidates)))

      (defun ace-link--notmuch-hello-action (pt)
        (when (number-or-marker-p pt)
          (goto-char (1+ pt))
          (widget-button-press (point))))

      (defun ace-link-notmuch-hello ()
        "Open a visible link in *notmuch-hello*."
        (interactive)
        (let ((pt (avy-with ace-link-notmuch-hello
                    (avy--process
                     (ace-link--notmuch-hello-collect)
                     #'avy--overlay-pre))))
          (ace-link--notmuch-hello-action pt)))

      (defun ace-link--notmuch-show-collect ()
        "Collect the positions of visible links in `notmuch-show' buffer."
        (let (candidates pt)
          (save-excursion
            (save-restriction
              (narrow-to-region
               (window-start)
               (window-end))
              (goto-char (point-min))
              (while (re-search-forward "https?://" nil t)
                (setq pt (- (point) (length (match-string 0))))
                (push pt candidates))))
          (nreverse candidates)))

      (defun ace-link--notmuch-show-action  (pt)
        (goto-char pt)
        (browse-url-at-point))

      (defun ace-link-notmuch-show ()
        "Open a visible link in `notmuch-show' buffer."
        (interactive)
        (let ((pt (avy-with ace-link-notmuch-show
                    (avy--process
                     (ace-link--notmuch-show-collect)
                     #'avy--overlay-pre))))
          (ace-link--notmuch-show-action pt)))
      )
    )
  )

(defun notmuch/init-gnus-alias ()
  (use-package gnus-alias
    :init (add-hook 'message-setup-hook
                    'gnus-alias-determine-identity)
    :config (progn
              (setq notmuch/gnus-alias-identities
                    '(("home" nil
                       "Somebody Someone <somebody@someone.com>" ;; Sender address
                       nil                                       ;; Organization header
                       nil                                       ;; Extra headers
                       nil                                       ;; Extra body text
                       "~/.signature")))

              (setq gnus-alias-identity-alist
                    '(("personal" nil                      ;; does not refer to any other identity
                       "Changchun Li <changchunli93@gmail.com>" ;; sender address
                       nil                                 ;; organization header
                       nil                                 ;; extra headers
                       nil                                 ;; extra body text
                       ;;"~/.signature"
                       )
                      ("work" nil
                       "Changchun Li <changchunli93@gmail.com>"
                       nil
                       nil
                       nil
                       ;;"~/.signature.work"
                       )))

              ;; Use "work" identity by default
              (setq gnus-alias-default-identity "work")
              )))

(defun notmuch/ini-org-notmuch ()
  (use-package org-notmuch))

(defun notmuch/post-init-org ()
  (use-package org
    :config (progn
              (add-hook 'message-mode-hook 'turn-on-orgstruct)
              (add-hook 'message-mode-hook 'turn-on-orgstruct++)
              (add-hook 'message-mode-hook 'turn-on-orgtbl))))


(defun notmuch/post-init-wid-edit ())
;; (defun notmuch/post-init-org ())
(defun notmuch/init-notmuch-labeler ())
(defun notmuch/init-notmuch ()
  "Initialize my package"
  (use-package notmuch
    :commands (notmuch-tree)
    :defer t
    :init (progn

            (defun my-buffer-face-mode-notmuch ()
              "Sets a fixed width (monospace) font in current buffer"
              (interactive)
              (setq buffer-face-mode-face '(:family "Inziu Iosevka SC"))
              (buffer-face-mode))
            (add-hook 'notmuch-hello-mode-hook 'my-buffer-face-mode-notmuch)
            (add-hook 'notmuch-tree-mode-hook 'my-buffer-face-mode-notmuch)
            (add-hook 'notmuch-search-mode-hook 'my-buffer-face-mode-notmuch)
            (add-hook 'notmuch-message-mode-hook 'my-buffer-face-mode-notmuch)
            (add-hook 'notmuch-message-mode-hook
                      (lambda ()
                        (set (make-local-variable 'company-backends) '(notmuch-company (company-ispell :with company-yasnippet)))))
            (add-hook 'notmuch-tree-mode-hook (lambda () (setq line-spacing nil)))


            (spacemacs/set-leader-keys
              "an" 'notmuch
              "at" 'notmuch-tree
              "a;" 'notmuch-search
              "ai" 'counsel-notmuch
              ))

    :config (progn

              (defun notmuch-start-notmuch-sentinel (proc event)
                "Process sentinel function used by `notmuch-start-notmuch'."
                (let* ((err-file (process-get proc 'err-file))
                       (err-buffer (or (process-get proc 'err-buffer)
                                       (find-file-noselect err-file)))
                       (err (when (not (zerop (buffer-size err-buffer)))
                              (with-current-buffer err-buffer (buffer-string))))
                       (sub-sentinel (process-get proc 'sub-sentinel))
                       (real-command (process-get proc 'real-command)))
                  (condition-case err
                      (progn
                        ;; Invoke the sub-sentinel, if any
                        (when sub-sentinel
                          (funcall sub-sentinel proc event))
                        ;; Check the exit status.  This will signal an error if the
                        ;; exit status is non-zero.  Don't do this if the process
                        ;; buffer is dead since that means Emacs killed the process
                        ;; and there's no point in telling the user that (but we
                        ;; still check for and report stderr output below).
                        (when (buffer-live-p (process-buffer proc))
                          (notmuch-check-async-exit-status proc event real-command err))
                        ;; If that didn't signal an error, then any error output was
                        ;; really warning output.  Show warnings, if any.
                        (let ((warnings
                               (when err
                                 (with-current-buffer err-buffer
                                   (goto-char (point-min))
                                   (end-of-line)
                                   ;; Show first line; stuff remaining lines in the
                                   ;; errors buffer.
                                   (let ((l1 (buffer-substring (point-min) (point))))
                                     (skip-chars-forward "\n")
                                     (cons l1 (unless (eobp)
                                                (buffer-substring (point) (point-max)))))))))
                          (when warnings
                            (notmuch-logged-error (car warnings) (cdr warnings)))))
                    (error
                     ;; Emacs behaves strangely if an error escapes from a sentinel,
                     ;; so turn errors into messages.
                     (message "%s" (error-message-string err))))
                  (when err-buffer
                    (set-process-query-on-exit-flag (get-buffer-process err-buffer) nil)
                    (kill-buffer err-buffer))
                  (when err-file (ignore-errors (delete-file err-file)))))
              (eval-after-load "notmuch-hello" `(define-key notmuch-hello-mode-map "o" 'ace-link-notmuch-hello))
              (eval-after-load "notmuch-show" `(define-key notmuch-show-mode-map "o" 'ace-link-notmuch-show))

              (defun notmuch-update ()
                (interactive)
                (start-process-shell-command "manually update email" nil "afew -a -m && mbsync gmail && notmuch new && afew -a -t")
                (notmuch-hello-update)
                )

              (defvar counsel-notmuch-history nil
                "History for `counsel-notmuch'.")

              (defun counsel-notmuch-cmd (input)
                "Return mail"
                (counsel-require-program "/usr/bin/notmuch")
                (format "notmuch search %s" input)
                )

              (defun counsel-notmuch-function (input)
                "helper function"
                (setq counsel-notmuch-base-command "/usr/bin/notmuch search")
                (if (< (length input) 3)
                    (counsel-more-chars 3)
                  (counsel--async-command
                   (counsel-notmuch-cmd input)) '("" "working...")))

              (defun counsel-notmuch-action-tree (thread)
                "open search result in tree view"
                (setq thread-id (car (split-string thread "\\ +")))
                (notmuch-tree thread-id initial-input nil)
                )

              (defun my-notmuch-show (thread-id &optional elide-toggle parent-buffer query-context buffer-name)
                "Run \"notmuch show\" with the given thread ID and display results.

ELIDE-TOGGLE, if non-nil, inverts the default elide behavior.

The optional PARENT-BUFFER is the notmuch-search buffer from
which this notmuch-show command was executed, (so that the
next thread from that buffer can be show when done with this
one).

The optional QUERY-CONTEXT is a notmuch search term. Only
messages from the thread matching this search term are shown if
non-nil.

The optional BUFFER-NAME provides the name of the buffer in
which the message thread is shown. If it is nil (which occurs
when the command is called interactively) the argument to the
function is used.

Returns the buffer containing the messages, or NIL if no messages
matched."
                (interactive "sNotmuch show: \nP")
                (let (
                      ;; (buffer-name "*counsel-notmuch-show*")
                      ;; We override mm-inline-override-types to stop application/*
                      ;; parts from being displayed unless the user has customized
                      ;; it themselves.
                      (mm-inline-override-types
                       (if (equal mm-inline-override-types
                                  (eval (car (get 'mm-inline-override-types 'standard-value))))
                           (cons "application/*" mm-inline-override-types)
                         mm-inline-override-types)))
                  (switch-to-buffer (get-buffer-create buffer-name))
                  ;; No need to track undo information for this buffer.
                  (let ((inhibit-read-only t))
                    (erase-buffer))
                  (setq buffer-undo-list t)

                  (notmuch-show-mode)

                  ;; Set various buffer local variables to their appropriate initial
                  ;; state. Do this after enabling `notmuch-show-mode' so that they
                  ;; aren't wiped out.
                  (setq notmuch-show-thread-id thread-id
                        notmuch-show-parent-buffer parent-buffer
                        notmuch-show-query-context query-context

                        notmuch-show-process-crypto notmuch-crypto-process-mime
                        ;; If `elide-toggle', invert the default value.
                        notmuch-show-elide-non-matching-messages
                        (if elide-toggle
                            (not notmuch-show-only-matching-messages)
                          notmuch-show-only-matching-messages))

                  (add-hook 'post-command-hook #'notmuch-show-command-hook nil t)
                  (jit-lock-register #'notmuch-show-buttonise-links)

                  (notmuch-tag-clear-cache)

                  (let ((inhibit-read-only t))
                    (if (notmuch-show--build-buffer)
                        ;; Messages were inserted into the buffer.
                        (current-buffer)

                      ;; No messages were inserted - presumably none matched the
                      ;; query.
                      (kill-buffer (current-buffer))
                      (ding)
                      (message "No messages matched the query!")
                      nil))))

              (defun counsel-notmuch-action-show (thread)
                "open search result in show view"
                (setq thread-id (car (split-string thread "\\ +")))
                (my-notmuch-show thread-id nil nil nil "*counsel-notmuch-show*")
                )



              (defun counsel-notmuch (&optional initial-input)
                "search for your email in notmuch"
                (interactive)
                (ivy-set-prompt 'counsel-notmuch counsel-prompt-function)
                (ivy-read "Notmuch Search"
                          #'counsel-notmuch-function
                          :initial-input initial-input
                          :dynamic-collection t
                          ;; :keymap counsel-notmuch-map
                          :history 'counsel-notmuch-history
                          :action '(1
                                    ("o" counsel-notmuch-action-show "Show")
                                    ("t" counsel-notmuch-action-tree "Tree View")
                                    )
                          :unwind (lambda ()
                                    (counsel-delete-process)
                                    (swiper--cleanup))
                          :caller 'counsel-notmuch))

              (defun counsel-notmuch-transformer (str)
                "blah"
                (when (string-match "thread:" str)

                  (setq mid (substring str 25))
                  (setq date (substring str 25 37))
                  (setq mat (substring mid (string-match "[[]" mid) (+ (string-match "[]]" mid) 1)))
                  (setq people (truncate-string-to-width (s-trim (nth 1 (split-string mid "[];]"))) 20))
                  (setq subject (truncate-string-to-width (s-trim (nth 1 (split-string mid "[;]"))) (- (window-width) 32)))
                  (setq output (format "%s\t%10s\t%20s\t%s" date mat people subject))
                  output
                  )
                )

              (ivy-set-display-transformer 'counsel-notmuch 'counsel-notmuch-transformer)


              (defun open-message-with-mail-app-notmuch-tree ()
                (interactive)
                (let* ((msg-path (car (plist-get (notmuch-tree-get-message-properties) :filename)))
                       (temp (make-temp-file "notmuch-message-" nil ".eml")))
                  (shell-command-to-string (format "cp '%s' '%s'; open '%s' -a Mail; l/bin/rm '%s'" msg-path temp temp temp))))
              (defun open-message-with-mail-app-notmuch-show ()
                (interactive)
                (let* ((msg-path (car (plist-get (notmuch-show-get-message-properties) :filename)))
                       (temp (make-temp-file "notmuch-message-" nil ".eml")))
                  (shell-command-to-string (format "cp '%s' '%s'; open '%s' -a Mail; l/bin/rm '%s'" msg-path temp temp temp))))

              (defun notmuch-search-delete () (interactive) (notmuch-search-add-tag (list "+deleted" "-inbox" "-unread")) (notmuch-search-next-thread))
              (defun notmuch-tree-delete () (interactive) (notmuch-tree-add-tag (list "+deleted" "-inbox" "-unread")) (notmuch-tree-next-message))

              (defun notmuch-search-killed () (interactive) (notmuch-search-add-tag (list "+killed" "-inbox" "-unread")) (notmuch-search-next-thread))
              (defun notmuch-tree-killed () (interactive) (notmuch-tree-add-tag (list "+killed" "-inbox" "-unread")) (notmuch-tree-next-message))


              (setq notmuch-fcc-dirs nil
                    notmuch-show-logo nil
                    message-kill-buffer-on-exit t
                    message-send-mail-function 'message-send-mail-with-sendmail
                    notmuch-search-oldest-first nil
                    send-mail-function 'sendmail-send-it
                    sendmail-program "/usr/bin/postfix"
                    )

              (spacemacs/set-leader-keys-for-major-mode 'notmuch-message-mode
                dotspacemacs-major-mode-leader-key 'notmuch-mua-send-and-exit
                "k" 'notmuch-mua-kill-buffer
                "s" 'message-dont-send  ; saves as draft
                "f" 'mml-attach-file)

              (evilified-state-evilify-map notmuch-search-mode-map
                :mode notmuch-search-mode
                :bindings
                (kbd "j") 'notmuch-search-next-thread
                (kbd "k") 'notmuch-search-previous-thread
                (kbd "t") 'notmuch-tree-from-search-thread
                (kbd "T") 'notmuch-tree-from-search-current-query
                (kbd "d") 'notmuch-search-delete
                (kbd "x") 'notmuch-search-killed
                )
              (evilified-state-evilify-map notmuch-tree-mode-map
                :mode notmuch-tree-mode
                :bindings
                (kbd "j") 'notmuch-tree-next-message
                (kbd "k") 'notmuch-tree-prev-message
                (kbd "S") 'notmuch-search-from-tree-current-query
                (kbd "t") 'notmuch-tree
                (kbd "r") 'notmuch-search-reply-to-thread-sender
                (kbd "i") 'open-message-with-mail-app-notmuch-tree
                (kbd "d") 'notmuch-tree-delete
                (kbd "x") 'notmuch-tree-killed
                )

              (evilified-state-evilify-map notmuch-hello-mode-map
                :mode notmuch-hello-mode
                :bindings
                (kbd "t") 'notmuch-tree
                (kbd "q") 'notmuch-hello-update
                (kbd "e") 'notmuch-update
                )

              (evilified-state-evilify-map notmuch-show-mode-map
                :mode notmuch-show-mode
                :bindings
                (kbd "i") 'open-message-with-mail-app-notmuch-show
                (kbd "I") 'notmuch-show-view-all-mime-parts
                )

              )))

;;; packages.el ends here
