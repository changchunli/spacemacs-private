;;; packages.el --- ct-dired Layer packages File for Spacemacs
;;
;; Author: C. Tan
;; Email:  noinil@gmail.com
;;

(setq ct-dired-packages
      '(dired
        ;; dired+
        ))

(defun ct-dired/post-init-dired ()
  ;; (dired-hide-details-mode 0)
  )

(defun ct-dired/init-dired+ ()
  (use-package dired+
    :init))

(defun ct-dired/post-init-dired+ ()
  (with-eval-after-load 'dired+
    (set-face-attribute 'diredp-number nil
                        :foreground "OliveDrab" :background nil)
    (set-face-attribute 'diredp-dir-heading nil
                        :foreground "deep sky blue" :background nil :weight 'ultra-bold)
    (set-face-attribute 'diredp-dir-priv nil
                        :foreground "Deepskyblue4" :background nil :weight 'ultra-bold)
    (set-face-attribute 'diredp-read-priv nil
                        :foreground "Peru" :background nil)
    (set-face-attribute 'diredp-write-priv nil
                        :foreground "OliveDrab" :background nil)
    (set-face-attribute 'diredp-exec-priv nil
                        :foreground "Gold4" :background nil)
    (set-face-attribute 'diredp-no-priv nil
                        :foreground "Deepskyblue4" :background nil)
    (set-face-attribute 'diredp-dir-name nil
                        :foreground "Deepskyblue1" :background nil)
    (set-face-attribute 'diredp-file-name nil
                        :foreground "LightGray" :background nil)
    (set-face-attribute 'diredp-deletion nil
                        :foreground "red" :background nil)
    (set-face-attribute 'diredp-file-suffix nil
                        :foreground "SteelBlue" :background nil)
    (set-face-attribute 'diredp-flag-mark nil
                        :foreground "green yellow" :background nil :weight 'bold)
    (set-face-attribute 'diredp-flag-mark-line nil
                        :background "dark green" :foreground "white")
    (set-face-attribute 'diredp-other-priv nil
                        :foreground "#111117175555" :background nil)
    (set-face-attribute 'diredp-rare-priv nil
                        :foreground "#FFFF00008080" :background nil)))
