;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' wmelpaill lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation nil
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation nil
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; Lhttps://github.com/redguardtoo/emacs.d.gitist of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ivy
     (olivetti :variables
               olivetti-body-width 0.6
               olivetti-hide-mode t)
     bibtex nginx twitter asciidoc elfeed speed-reading
     (plantuml :variables
               plantuml-jar-path "~/.spacemacs.d/plantuml.jar"
               org-plantuml-jar-path "~/.spacemacs.d/plantuml.jar")
     better-defaults
     helm
     ranger
     ;; neotree
     (treemacs :variables
               treemacs-use-follow-mode t
               treemacs-use-filewatch-mode t
               treemacs-use-collapsed-directories 3)
     (colors :variables
             colors-colorize-identifiers 'all
             colors-enable-nyan-cat-progress-bar t)
     prodigy search-engine
     (sql :variables sql-capitalize-keywords t)
     nim ipython-notebook pdf-tools graphviz asm perl6
     (syntax-checking :variables
                      syntax-checking-enable-by-default nil
                      syntax-checking-enable-tooltips nil)
     (spell-checking :variables spell-checking-enable-by-default nil)
     (vinegar :variables vinegar-reuse-dired-buffer t)
     (spacemacs-layouts :variables
                        layouts-enable-autosave nil
                        layouts-autosave-delay 300)
     (git :variables
          git-magit-status-fullscreen t
          magit-diff-refine-hunk 'all
          magit-push-always-verify nil
          magit-save-repository-buffers 'dontask
          magit-revert-buffers 'silent
          magit-refs-show-commit-count 'all
          magit-revision-show-gravatars nil)
     github gnus semantic
     (version-control :variables version-control-global-margin t)
     ;; diff-h1
     salt
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.0
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-company-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-private-snippets-directory "~/.spacemacs.d/snippets"
                      :disabled-for org markdown)
     (osx :variables
          osx-dictionary-dictionary-choice "Simplified Chinese - English"
          osx-command-as 'super)
     (restclient :variables restclient-use-org t)
     restructuredtext selectric octave
     (gtags :disabled-for clojure emacs-lisp javascript latex python shell-scripts)
     (shell :variables
            shell-enable-smart-eshell t
            shell-default-shell 'ansi-term
            shell-default-position 'bottom
            shell-default-height 30
            shell-default-full-span nil
            shell-default-term-shell "/bin/zsh")
     shell-scripts docker ansible puppet rebox
     evil-commentary
     (evil-snipe :variables
                 evil-snipe-enable-alternate-f-and-t-behaviors t)
     fasd finance floobits xkcd autohotkey csv nlinum
     (latex :variables
            latex-enable-auto-fill t
            latex-enable-magic nil
            latex-enable-folding t
            ;; latex-build-command "XeLaTeX"
            )
     deft
     erc chrome d emoji evernote
     (markdown :variables markdown-live-preview-engine 'vmd)
     (org :variables
          org-want-todo-bindings t
          org-enable-github-support t
          org-enable-bootstrap-support t
          org-enable-reveal-js-support t
          org-enable-hugo-support t)
     gpu
     notmuch
     groovy
     kotlin
     yaml
     react
     spacemacs-evil spacemacs-language
     ;; yasnippet
     tmux themes-megapack
     ;; if use .emacs.d/layer/+vim/vim-empty-lines/local/vim-empty-lines-mode/vim-empty-lines-mode.el,
     ;; this causes a org-capture error: Capture template 'n': integer-or-marker-p.
     ;; change .emacs.d/layer/+vim/vim-empty-lines/local/vim-empty-lines-mode/vim-empty-lines-mode.el to
     ;; https://github.com/hchbaw/vim-empty-lines-mode/blob/thb/vim-empty-lines-mode.el
     ;; NOTE: this bug has been fixed.
     vim-empty-lines
     spotify pandoc vagrant sphinx parinfer erlang protobuf 
     (python :variables
             python-enable-yapf-format-on-save t
             python-sort-imports-on-save t
             python-test-runner '(nose pytest))
     (ruby :variables
           ruby-enable-enh-ruby-mode t
           ruby-version-manager 'rvm)
     ruby-on-rails
     (rust :variables
           rust-enable-rustfmt-on-save t)
     windows-scripts agda jr
     lua
     html
     major-modes
     (javascript :variables javascript-disable-tern-port-files t)
     (typescript :variables
                 typescript-fmt-on-save nil
                 typescript-fmt-tool 'typescript-formatter)
     emacs-lisp
     (clojure :variables clojure-enable-fancify-symbols t)
     sml pass
     (go :variables 
         go-use-gometalinter t
         gofmt-command "goimports"
         go-tab-width 4
         go-use-gocheck-for-testing t
         go-use-test-args "-race -timeout 10s")
     major-modes racket scheme purescript common-lisp dash
     jabber cscope rcirc games php vimscript geolocation idris
     coq django
     (terraform :variables terraform-auto-format-on-save t)
     (haskell :variables
              haskell-enable-ghci-ng-support t
              haskell-enable-shm-support t
              haskell-completion-backend 'dante
              haskell-enable-hindent-style "andrew-gibiansky")
     (java :variables java-backend 'meghanada)
     swift
     (scala :variables
            scala-auto-insert-asterisk-in-comments t
            scala-auto-start-ensime nil
            scala-enable-eldoc-mode nil)
     (ess :variables
          ess-enable-smart-equals t)
     (c-c++ :variables
            c-c++-enable-clang-support t
            c-c++-enable-clang-format-on-save t
            c-c++-enable-rtags-support t
            c-c++-default-mode-for-headers 'c++-mode)
     (elm :variables
          elm-sort-imports-on-save t
          elm-format-on-save t
          elm-reactor-port "3000"
          elm-reactor-address "0.0.0.0")
     (typography :variables typography-enable-typographic-editing nil)
     (elixir :variables flycheck-elixir-credo-strict t) 
     imenu-list slack systemd command-log faust
     zilongshanren
     (chinese :packages youdao-dictionary fcitx
              :variables chinese-enable-fcitx nil
              chinese-enable-youdao-dict t)
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; dotspacemacs-additional-packages '(sicp)
   dotspacemacs-additional-packages '(sicp wolfram linum-relative yasnippet-snippets)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages
   '(
                    ;; magit-gh-pulls magit-gitflow org-projectile evil-mc realgud
                    ;; evil-args evil-ediff evil-exchange evil-unimpaired
                    ;; evil-indent-plus volatile-highlights smartparens
                    ;; spaceline holy-mode skewer-mode rainbow-delimiters
                    ;; highlight-indentation vi-tilde-fringe eyebrowse
                    ;; org-bullets smooth-scrolling org-repo-todo org-download org-timer
                    ;; livid-mode git-gutter git-gutter-fringe  evil-escape
                    ;; leuven-theme gh-md evil-lisp-state spray lorem-ipsum symon
                    ;; ac-ispell ace-jump-mode auto-complete auto-dictionary
                    ;; clang-format define-word google-translate disaster epic
                    ;; fancy-battery org-present orgit orglue spacemacs-theme
                    ;; helm-flyspell flyspell-correct-helm clean-aindent-mode
                    ;; helm-c-yasnippet ace-jump-helm-line helm-make magithub
                    ;; helm-themes helm-swoop helm-spacemacs-help smeargle
                    ;; ido-vertical-mode flx-ido company-quickhelp counsel-projectile
                    ;; window-purpose ivy-purpose helm-purpose spacemacs-purpose-popwin
                    ess-R-object-popup org-mac-link wolfram-mode textmate-to-yas.el
                    )
   dotspacemacs-install-packages 'used-only
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         solarized-dark
                         solarized-light
                         spacemacs-light
                         leuven)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Consolas"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text t
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers '(:relative t
                               :disabled-for-modes org-mode
                                                   org-present-mode
                               :size-limit-kb 1000)
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'origami
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed
   )
)

(defun dotspacemacs/user-init ()
  (setq configuration-layer-elpa-archives
        '(("melpa-cn" . "https://elpa.emacs-china.org/melpa/")
          ("org-cn"   . "https://elpa.emacs-china.org/org/")
          ("gnu-cn"   . "https://elpa.emacs-china.org/gnu/")))

  ;; https://github.com/syl20bnr/spacemacs/issues/2705
  ;; (setq tramp-mode nil)
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  ;; ss proxy. But it will cause anacond-mode failed.
  (setq socks-server '("Default server" "127.0.0.1" 1080 5))
  (setq evil-shift-round nil)
  (setq byte-compile-warnings '(not obsolete))
  (setq warning-minimum-level :error)
  ;; hack for remove purpose mode
  (setq purpose-mode nil)
)

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  ;; 解决org表格里面中英文对齐的问题
  (when (configuration-layer/layer-usedp 'chinese)
    (when (and (spacemacs/system-is-mac) window-system)
      (spacemacs//set-monospaced-font "Source Code Pro" "Hiragino Sans GB" 14 16)))

  ;; Setting Chinese Font
  (when (and (spacemacs/system-is-mswindows) window-system)
    (setq ispell-program-name "aspell")
    (setq w32-pass-alt-to-system nil)
    (setq w32-apps-modifier 'super)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec :family "Microsoft Yahei" :size 14))))

  (fset 'evil-visual-update-x-selection 'ignore)

  ;; force horizontal split window
  (setq split-width-threshold 120)
  ;; (linum-relative-on)

  (spacemacs|add-company-backends :modes text-mode)

  (add-hook 'doc-view-mode-hook 'auto-revert-mode)

  ;; temp fix for ivy-switch-buffer
  ;; (spacemacs/set-leader-keys "bb" 'helm-mini)

  (global-hungry-delete-mode t)
  (spacemacs|diminish helm-gtags-mode)
  (spacemacs|diminish ggtags-mode)
  (spacemacs|diminish which-key-mode)
  (spacemacs|diminish spacemacs-whitespace-cleanup-mode)
  (spacemacs|diminish counsel-mode)

  (evilified-state-evilify-map special-mode-map :mode special-mode)

  (add-to-list 'auto-mode-alist
               '("Capstanfile\\'" . yaml-mode))

  (push "~/.spacemacs.d/yasmate/snippets" yas-snippet-dirs)
  
  ;; texlive path configure
  (defun TeXlive (year)
    "Use TeXlive with the given year (given as string), nil if no TeXlive."
    (interactive
     (let* ((year
             (directory-files "/usr/local/texlive/" nil "\\`[0-9]+\\'")))
       (setq year
             (completing-read (format "Year to use (default %s): "
                                      (car (last year)))
                              (cons "none" year)
                              nil t nil nil (car (last year))))
       (list (unless (string= year "none") year))))
    (let ((path (getenv "PATH")))
      (while (string-match "/usr/local/texlive/[0-9]+/bin/x86_64-linux:" path)
        (setq path (replace-match "" t t path)))
      (when year
        (setq path (format "/usr/local/texlive/%s/bin/x86_64-linux:%s"
                           year path)))
      (setenv "PATH" path)))

  (TeXlive 2017)
  (setq exec-path (append exec-path
                          '("/usr/local/texlive/2017/bin/x86_64-linux/")))
  
  ;; set TeX-view-program-list
  ;; use TeX-view-program-list-builtin, which is in auctex
  ;; (setq TeX-view-program-list
  ;; '(("Okular" "okular --unique %o#src:%n`pwd`/./%b")
  ;; ("Skim" "displayline -b -g %n %o %b")
  ;; ("Zathura" "zathura-sync.sh %n:1:%b %o")))
  
  ;; set pdf viewer on OSX and Linux
  (cond
   ((spacemacs/system-is-mac) (setq TeX-view-program-selection '((output-pdf "Skim"))))
   ((spacemacs/system-is-linux) (setq TeX-view-program-selection '((output-pdf "Zathura")))))

  ;; enable PDF-LaTeX synchronization
  ;; press SPC m v to highlight line in PDF
  ;; press shift cmd and click in PDF to show line in sourcecode
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t)
  (setq TeX-source-correlate-method 'synctex)

  (defun js-indent-line ()
    "Indent the current line as JavaScript."
    (interactive)
    (let* ((parse-status
            (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (point) (save-excursion (back-to-indentation) (point)))))
      (if (nth 3 parse-status)
          'noindent
        (indent-line-to (js--proper-indentation parse-status))
        (when (> offset 0) (forward-char offset)))))

  (global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
  (defun un-indent-by-removing-4-spaces ()
    "remove 4 spaces from beginning of of line"
    (interactive)
    (save-excursion
      (save-match-data
        (beginning-of-line)
        ;; get rid of tabs at beginning of line
        (when (looking-at "^\\s-+")
          (untabify (match-beginning 0) (match-end 0)))
        (when (looking-at (concat "^" (make-string tab-width ?\ )))
          (replace-match "")))))

  (defun zilongshanren/toggle-major-mode ()
    (interactive)
    (if (eq major-mode 'fundamental-mode)
        (set-auto-mode)
      (fundamental-mode)))
  (spacemacs/set-leader-keys "otm" 'zilongshanren/toggle-major-mode)

  ;; (add-hook 'text-mode-hook 'spacemacs/toggle-spelling-checking-on)
  
  ;; org-mode default open file function
  ;; (add-to-list 'org-file-apps '("\\.pdf\\" . (lambda (file link) (zilongshanren-org/org-pdfview-open link))))
  ;; (setq org-file-apps '(("\\.pdf\\'" . (lambda (file link) (zilongshanren-org/org-pdfview-open link)))))
  ;; PDFs visited in Org-mode are opened in Evince (and not in the default choice) https://stackoverflow.com/a/8836108/789593
  (add-hook 'org-mode-hook
            '(lambda ()
               (delete '("\\.pdf\\'" . default) org-file-apps)
               (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (zilongshanren-org/org-pdfview-open link))))))

  (add-hook 'ess-mode-hook
            (lambda ()
              (ess-toggle-underscore nil)))

)

(setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
(load custom-file 'no-error 'no-message)
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
)
