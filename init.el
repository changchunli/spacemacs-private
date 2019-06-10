;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; (ivy :variables ivy-enable-advanced-buffer-information nil)
     ;; better-defaults
     ;; ranger
     ;; colors
     ;; prodigy
     ;; search-engine
     ;; graphviz
     ;; (haskell :variables haskell-enable-hindent t
     ;;          haskell-completion-backend 'intero)
     ;; (syntax-checking :variables syntax-checking-enable-by-default nil
     ;;                  syntax-checking-enable-tooltips nil)
     ;; (spell-checking :variables spell-checking-enable-by-default nil)
     ;; ;; (vinegar :variables vinegar-reuse-dired-buffer t)
     ;; (spacemacs-layouts :variables layouts-enable-autosave nil
     ;;                    layouts-autosave-delay 300)
     
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     zilongshanren
     agda ansible asciidoc asm autohotkey
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-company-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-idle-delay 0.2
                      auto-completion-private-snippets-directory nil
                      ;; spacemacs-default-company-backends '(company-files company-capf)
                      :disabled-for org markdown)
     (better-defaults :variables
                      better-defaults-move-to-beginning-of-code-first t
                      better-defaults-move-to-end-of-code-first nil)
     bibtex bm
     (c-c++ :variables
            c-c++-enable-auto-newline t
            c-c++-enable-clang-support t
            c-c++-enable-clang-format-on-save t
            c-c++-enable-rtags-support t
            c-c++-enable-google-style t
            c-c++-enable-google-newline t
            c-c++-default-mode-for-headers 'c++-mode)
     cfengine
     (chinese :packages youdao-dictionary fcitx
              :variables
              chinese-enable-fcitx nil
              chinese-enable-youdao-dict t)
     chrome
     (clojure :variables clojure-enable-fancify-symbols t)
     cmake coffeescript
     (colors :variables
             colors-colorize-identifiers 'all
             colors-enable-nyan-cat-progress-bar t)
     common-lisp command-log confluence
     copy-as-format
     coq crystal cscope csharp csv
     d dash deft debug django docker
     (elfeed :variables
             rmh-elfeed-org-files (list
                                   (concat dotspacemacs-directory "layers/README.org"))
             elfeed-goodies/entry-pane-position 'bottom)
     (elm :variables
          elm-sort-imports-on-save t
          elm-format-on-save t
          elm-reactor-port "3000"
          elm-reactor-address "0.0.0.0")
     (elixir :variables flycheck-elixir-credo-strict t)
     emacs-lisp emberjs emoji epub erc erlang
     (ess :variables
          ess-enable-smart-equals t
          ess-use-auto-complete t
          ess-use-eldoc t
          ess-ask-about-transfile nil)
     evernote evil-commentary
     (evil-snipe :variables
                 evil-snipe-enable-alternate-f-and-t-behaviors t)
     factor fasd faust finance floobits forth fsharp games geolocation
     (git :variables
          git-magit-status-fullscreen t
          magit-diff-refine-hunk 'all
          magit-push-always-verify nil
          magit-save-repository-buffers 'dontask
          magit-revert-buffers 'silent
          magit-refs-show-commit-count 'all
          magit-revision-show-gravatars nil)
     github gnus
     (go :variables
         go-use-gometalinter t
         gofmt-command "goimports"
         go-tab-width 4
         go-use-gocheck-for-testing t
         go-use-test-args "-test.v -failfast -count=1 -race -timeout 10s"
         godoc-at-point-function 'godoc-gogetdoc
         flycheck-gometalinter-vendor t
         flycheck-gometalinter-fast t
         flycheck-gometalinter-disable-linters '("goconst")
         flycheck-gometalinter-deadline "10s")
     gpu graphviz groovy
     (gtags :variables gtags-enable-by-default t
            :disabled-for clojure emacs-lisp javascript latex python shell-scripts)
     (haskell :variables
              haskell-enable-ghci-ng-support t
              haskell-enable-shm-support t
              haskell-completion-backend 'dante
              haskell-enable-hindent-style "andrew-gibiansky")
     helm
     html hy
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     idris imenu-list ipython-notebook
     ;; (ivy :variables ivy-enable-advanced-buffer-information t)
     jabber
     (java :variables java-backend 'meghanada)
     (javascript :variables
                 javascript-backend 'nil
                 javascript-disable-tern-port-files t)
     jr json julia
     ;; keyboard-layout
     kotlin
     (latex :variables
            latex-build-command "LaTeX"
            latex-enable-auto-fill t
            latex-enable-magic nil
            latex-enable-folding t)
     lsp lua major-modes
     (markdown :variables markdown-live-preview-engine 'vmd)
     multiple-cursors
     ;; neotree
     nginx nim
     ;; nlinum
     node notmuch
     ;; ocaml
     octave
     (olivetti :variables
               olivetti-body-width 0.6
               olivetti-hide-mode t)
     (org :variables
          org-want-todo-bindings t
          org-enable-github-support t
          org-enable-bootstrap-support t
          org-enable-reveal-js-support t
          org-enable-hugo-support t
          org-enable-org-journal-support t
          org-journal-dir "~/org/journal/"
          org-journal-file-format "%Y-%m-%d"
          org-journal-date-prefix "#+TITLE: "
          org-journal-date-format "%A, %B %d %Y"
          org-journal-time-prefix "* "
          org-journal-time-format ""
          ;; org-bullets-bullet-list '("■" "◆" "▲" "▶")
          org-bullets-bullet-list '("◉" "○" "✸" "◻" "❀" "✡")
          org-projectile-file "~/org/TODOs.org"
          spaceline-org-clock-p t
          org-startup-indented t
          org-startup-folded t)
     (osx :variables
          osx-dictionary-dictionary-choice "Simplified Chinese - English"
          osx-use-option-as-meta t
          osx-command-as 'super
          osx-option-as 'meta
          osx-control-as 'control
          osx-function-as 'none
          osx-right-command-as 'left
          osx-right-option-as 'none
          osx-right-control-as 'left)
     pact pandoc parinfer pass pdf perl6 phoenix php
     (plantuml :variables
               plantuml-jar-path "~/.spacemacs.d/plantuml.jar"
               org-plantuml-jar-path "~/.spacemacs.d/plantuml.jar")
     prettier prodigy protobuf puppet purescript
     (python :variables
             python-backend 'anaconda
             python-enable-yapf-format-on-save t
             python-fill-column 80
             python-sort-imports-on-save t
             ;; python-shell-completion-native-enable nil
             python-test-runner '(nose pytest))
     racket
     (ranger :variables
             ranger-ignored-extensions '("mkv" "iso" "mp4" "pdf")
             ranger-override-dired t
             ranger-show-hidden nil)
     rcirc react
     (rebox :variables rebox-enable-in-text-mode t)
     (restclient :variables restclient-use-org t)
     restructuredtext
     (ruby :variables
           ruby-enable-enh-ruby-mode t
           ruby-version-manager 'rvm)
     ruby-on-rails
     (rust :variables
           rust-enable-rustfmt-on-save t
           rust-format-on-save t)
     salt
     (scala :variables
            scala-auto-insert-asterisk-in-comments t
            scala-enable-eldoc t
            scala-indent:use-javadoc-style t
            scala-auto-start-ensime nil
            scala-enable-eldoc-mode nil)
     scheme search-engine selectric semantic semantic-web
     (shell :variables
            shell-enable-smart-eshell t
            shell-default-shell 'eshell
            shell-default-position 'bottom
            shell-default-height 30
            shell-default-full-span nil
            shell-default-term-shell "/bin/zsh"
            shell-protect-eshell-prompt t)
     shell-scripts slack smex sml spacemacs-evil spacemacs-language
     (spacemacs-layouts :variables
                        layouts-enable-autosave nil
                        layouts-autosave-delay 300)
     spacemacs-org speed-reading
     (spell-checking :variables
                     ispell-program-name "aspell"
                     ispell-dictionary "american"
                     spell-checking-enable-by-default nil
                     spell-checking-enable-auto-dictionary t
                     enable-flyspell-auto-completion t)
     sphinx spotify
     (sql :variables sql-capitalize-keywords t)
     swift
     (syntax-checking :variables
                      syntax-checking-enable-by-default nil
                      syntax-checking-enable-tooltips nil)
     systemd templates tern
     (terraform :variables terraform-auto-format-on-save t)
     themes-megapack
     (theming :variables
              theming-headings-inherit-from-default 'all
              theming-headings-same-size 'all
              theming-headings-bold 'all)
     tmux
     (treemacs :variables
               treemacs-use-follow-mode t
               treemacs-use-filewatch-mode t
               treemacs-use-collapsed-directories 3)
     twitter
     (typescript :variables
                 tide-tsserver-executable "~/.config/yarn/global/node_modules/.bin/tsserver"
                 typescript-fmt-on-save nil
                 typescript-fmt-tool 'typescript-formatter)
     (typography :variables typography-enable-typographic-editing nil)
     ;; unicode-fonts
     vagrant
     (version-control :variables
                      version-control-global-margin t
                      version-control-diff-tool 'git-gutter
                      version-control-diff-side 'left)
     ;; if use .emacs.d/layer/+vim/vim-empty-lines/local/vim-empty-lines-mode/vim-empty-lines-mode.el,
     ;; this causes a org-capture error: Capture template 'n': integer-or-marker-p.
     ;; change .emacs.d/layer/+vim/vim-empty-lines/local/vim-empty-lines-mode/vim-empty-lines-mode.el to
     ;; https://github.com/hchbaw/vim-empty-lines-mode/blob/thb/vim-empty-lines-mode.el
     ;; NOTE: this bug has been fixed.
     vim-empty-lines vimscript
     (vinegar :variables vinegar-reuse-dired-buffer t)
     ;; wakatime
     web-beautify windows-scripts xclipboard xkcd yaml
     ;; ycmd
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   ;; dotspacemacs-additional-packages '(sicp)
   ;; dotspacemacs-additional-packages '(column-marker dired+ framemove
   ;; linum-relative llvm-mode sicp vline wolfram)
   dotspacemacs-additional-packages '(linum-relative sicp ssh-agency wolfram)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; '(magit-gh-pulls magit-gitflow  evil-mc realgud tern company-tern
   ;;                  evil-args evil-ediff evil-exchange evil-unimpaired
   ;;                  evil-indent-plus volatile-highlights smartparens
   ;;                  spaceline holy-mode skewer-mode rainbow-delimiters
   ;;                  highlight-indentation vi-tilde-fringe eyebrowse ws-butler
   ;;                  org-bullets smooth-scrolling org-repo-todo org-download org-timer
   ;;                  livid-mode git-gutter git-gutter-fringe  evil-escape
   ;;                  leuven-theme gh-md evil-lisp-state spray lorem-ipsum symon
   ;;                  ac-ispell ace-jump-mode auto-complete auto-dictionary
   ;;                  clang-format define-word google-translate disaster epic
   ;;                  fancy-battery org-present orgit orglue spacemacs-theme
   ;;                  helm-flyspell flyspell-correct-helm clean-aindent-mode
   ;;                  helm-c-yasnippet ace-jump-helm-line helm-make magithub
   ;;                  helm-themes helm-swoop helm-spacemacs-help smeargle
   ;;                  ido-vertical-mode flx-ido company-quickhelp ivy-rich
   ;;                  window-purpose ivy-purpose helm-purpose spacemacs-purpose-popwin
   ;;                  )

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')

   dotspacemacs-install-packages 'used-only
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; ;; If non-nil then Spacemacs will import your PATH and environment variables
   ;; ;; from your default shell on startup. This is enabled by default for macOS
   ;; ;; users and X11 users.
   ;; dotspacemacs-import-env-vars-from-shell (and (display-graphic-p)
   ;;                                              (or (eq system-type 'darwin)
   ;;                                                  (eq system-type 'gnu/linux)
   ;;                                                  (eq window-system 'x)))

   ;; ;; If nil then use the default shell is used to fetch the environment
   ;; ;; variables. Set this variable to a different shell executable path to
   ;; ;; import the environment variables from this shell. Note that
   ;; ;; `file-shell-name' is preserved and always points to the default shell. For
   ;; ;; instance to use your fish shell environment variables set this variable to
   ;; ;; `/usr/local/bin/fish'.
   ;; ;; (default nil)
   ;; dotspacemacs-import-env-vars-shell-file-name nil

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
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (bookmarks . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(dracula
                         jazz
                         monokai
                         spacemacs-dark
                         solarized-dark
                         solarized-light
                         spacemacs-light
                         leuven)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Consolas"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
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
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   ;; dotspacemacs-remap-Y-to-y$ t

   ;; ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; ;; there. (default t)
   ;; dotspacemacs-retain-visual-state-on-shift t

   ;; ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; ;; (default nil)
   ;; dotspacemacs-visual-line-move-text t

   ;; ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; ;; (default nil)
   ;; dotspacemacs-ex-substitute-global nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 10

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   ;; dotspacemacs-helm-resize nil

   ;; ;; if non-nil, the helm header is hidden when there is only one source.
   ;; ;; (default nil)
   ;; dotspacemacs-helm-no-header nil

   ;; ;; define the position to display `helm', options are `bottom', `top',
   ;; ;; `left', or `right'. (default 'bottom)
   ;; dotspacemacs-helm-position 'bottom

   ;; ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; ;; source settings. Else, disable fuzzy matching in all sources.
   ;; ;; (default 'always)
   ;; dotspacemacs-helm-use-fuzzy 'always

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
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
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
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

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If non-nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers '(:relative t
                                         :disabled-for-modes org-mode
                                         org-present-mode
                                         dired-mode
                                         doc-view-mode
                                         markdown-mode
                                         pdf-view-mode
                                         text-mode
                                         :size-limit-kb 1000)

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'origami

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
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

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; ;; The default package repository used if no explicit repository has been
   ;; ;; specified with an installed package.
   ;; ;; Not used for now. (default nil)
   ;; dotspacemacs-default-package-repository nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (setq-default configuration-layer-elpa-archives
                '(("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
                  ("org-cn"   . "http://elpa.emacs-china.org/org/")
                  ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")))

  ;; (setq debug-on-error t)

  ;; https://github.com/syl20bnr/spacemacs/issues/2705
  ;; (setq tramp-mode nil)
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  ;; ss proxy. But it will cause anacond-mode failed.
  (setq socks-server '("Default server" "127.0.0.1" 1080 5))
  ;; (setq evil-shift-round nil)
  (setq byte-compile-warnings '(not obsolete))
  (setq warning-minimum-level :error)
  ;; hack for remove purpose mode
  ;; (setq purpose-mode nil)

  ;; -----------------------------------------------------------------
  ;; copy from https://github.com/noinil/spacemacs_layers/blob/master/init.el
  (setq-default
   ;; Miscellaneous
   python-shell-interpreter-args "--simple-prompt"
   exec-path-from-shell-arguments (quote ("-l"))

   ;; Backups
   auto-save-interval 600
   auto-save-timeout 300
   backup-by-copying t
   delete-old-versions t
   kept-new-versions 5
   kept-old-versions 2
   make-backup-files nil

   ;; Evil
   evil-shift-round nil

   ;; Ranger
   ranger-override-dired nil

   ;; Theme modifications
   spacemacs-evil-cursors '(("normal" "DarkGoldenrod2" box)
                            ("insert" "chartreuse3" (bar . 2))
                            ("emacs" "SkyBlue2" box)
                            ("hybrid" "SkyBlue2" (bar . 2))
                            ("replace" "chocolate" (hbar . 2))
                            ("evilified" "LightGoldenrod3" box)
                            ("visual" "gray" (hbar . 2))
                            ("motion" "dark cyan" box)
                            ("lisp" "HotPink1" box)
                            ("iedit" "firebrick1" box)
                            ("iedit-insert" "firebrick1" (bar . 2)))

   theming-modifications
   '((spacemacs-dark
      ;; font-lock-faces
      (font-lock-builtin-face :foreground "#b55353" :weight bold)
      (font-lock-comment-delimiter-face :foreground "#62562c")
      (font-lock-comment-face :foreground "#61524f")
      (font-lock-warning-face :underline nil)
      (font-lock-constant-face :foreground "#6f8bad")
      (font-lock-doc-face :foreground "#5165b8" :slant italic)
      (font-lock-string-face :foreground "#0083d9" :slant italic)
      (font-lock-function-name-face :foreground "#408a9c")
      (font-lock-variable-name-face :foreground "#5f995f")
      (font-lock-keyword-face :foreground "DeepSkyBlue" :weight bold)
      (font-lock-negation-char-face :foreground "SlateBlue")
      (font-lock-reference-face :foreground "SlateBlue")
      (font-lock-type-face :foreground "#725da8")

      ;; latex-font-faces
      (font-latex-sectioning-0-face :foreground "SpringGreen")
      (font-latex-sectioning-1-face :foreground "OrangeRed")
      (font-latex-sectioning-2-face :foreground "DarkOrange")
      (font-latex-sectioning-3-face :foreground "Orange")
      (font-latex-sectioning-4-face :foreground "Gold")
      (font-latex-sedate-face :foreground "SteelBlue")
      (font-latex-slide-title-face :foreground "DeepSkyBlue")
      (font-latex-math-face :foreground "LawnGreen")
      (font-latex-string-face :inherit font-lock-string-face)

      ;; mode-line-faces
      (mode-line :box (:color "dark cyan" :line-width 1.2))
      (powerline-active1 :box (:color "dark cyan" :line-width 1.2) :background "gray20")
      (powerline-active2 :box (:color "dark cyan" :line-width 1.2) :background "#363A3A")
      (mode-line-inactive :box (:color "DarkSlateGray" :line-width 1.2))
      (powerline-inactive1 :box (:color "DarkSlateGray" :line-width 1.2))
      (powerline-inactive2 :box (:color "DarkSlateGray" :line-width 1.2))

      ;; flycheck-faces
      (flycheck-fringe-error :background nil)
      (flycheck-fringe-warning :background nil)
      (flycheck-fringe-info :background nil)

      ;; misc
      (helm-prefarg :foreground "PaleGreen")
      (helm-source-header :background "dark cyan")
      (mode-line-buffer-id :foreground "Turquoise")
      (which-key-key-face :foreground "dark cyan")
      (term :foreground nil :background nil))

     (monokai
      ;; Font locking
      (default :foreground "#b2b2b2")
      (font-lock-builtin-face :foreground "#81a618" :weight bold)
      (font-lock-comment-delimiter-face :foreground "#78766c")
      (font-lock-comment-face :foreground "#6e684c" :slant italic)
      (font-lock-warning-face :underline nil)
      (font-lock-constant-face :foreground "#e1a7a1")
      (font-lock-doc-face :foreground "#5165b8" :slant italic)
      (font-lock-function-name-face :foreground "#aadbdd")
      (font-lock-keyword-face :foreground "#a7c98b" :weight bold)
      (font-lock-negation-char-face :foreground "#8657a1")
      (font-lock-reference-face :foreground "#8657a1")
      (font-lock-string-face :foreground "#1aaeed" :slant italic)
      (font-lock-type-face :foreground "#7aa183")
      (font-lock-variable-name-face :foreground "#b8bfed")
      (helm-prefarg :foreground "PaleGreen")
      (mode-line-buffer-id :foreground "Turquoise")
      (which-key-key-face :foreground "dark cyan")
      (web-mode-html-attr-value-face
       :inherit font-lock-string-face :foreground nil)
      (web-mode-html-attr-name-face
       :inherit font-lock-variable-name-face :foreground nil)
      (web-mode-html-tag-face
       :inherit font-lock-builtin-face :foreground nil :weight bold)
      (web-mode-html-tag-bracket-face
       :inherit web-mode-html-tag-face :foreground nil)
      (web-mode-comment-face
       :inherit font-lock-comment-face :foreground nil)

      (mode-line :box (:color "dark cyan" :line-width 1.2))
      (powerline-active1 :box (:color "dark cyan" :line-width 1.2) :background "gray20")
      (powerline-active2 :box (:color "dark cyan" :line-width 1.2) :background "#363A3A")
      (mode-line-inactive :box (:color "DarkSlateGray" :line-width 1.2))
      (powerline-inactive1 :box (:color "DarkSlateGray" :line-width 1.2))
      (powerline-inactive2 :box (:color "DarkSlateGray" :line-width 1.2))

      ;; Flycheck
      (flycheck-fringe-error :background nil)
      (flycheck-fringe-warning :background nil)
      (flycheck-fringe-info :background nil)

      ;; Other
      (company-tooltip-annotation
       :foreground "#ff9eb8" :background "#49483e")
      (erc-timestamp-face
       :inherit font-lock-comment-face :foreground nil)
      (evil-search-highlight-persist-highlight-face
       :background "#fc5fef" :foreground "#000000")
      (region :background "#998f84")
      (term :foreground nil :background nil))))
  ;; ---------------------------------------------------------------
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
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

  ;;(with-eval-after-load 'helm
  ;;  (setq helm-display-function 'helm-default-display-buffer))

  ;; force horizontal split window
  (setq split-width-threshold 120)
  ;; (linum-relative-on)

  ;; Add org-projectile-todo-files to the agenda automatically
  (with-eval-after-load 'org-agenda
    (require 'org-projectile)
    (mapcar '(lambda (file)
               (when (file-exists-p file)
                 (push file org-agenda-files)))
            (org-projectile-todo-files)))

  (spacemacs|add-company-backends :modes text-mode)

  ;; Issue #10366 (Python completion broken for the first Python buffer you
  ;; visit), this issue might be fixed.
  ;; (spacemacs//python-setup-anaconda-company)

  ;; Improved company faces
  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

  ;; -----------------------------------------------------------------
  ;; copy from https://github.com/noinil/spacemacs_layers/blob/master/init.el
  ;; Misc
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

  ;; Auto-completion: company
  (global-company-mode)

  ;; ;; Enabling multi-dictionary support with hunspell
  ;; (with-eval-after-load "ispell"
  ;;   (setq ispell-program-name "hunspell")
  ;;   ;; ispell-set-spellchecker-params has to be called
  ;;   ;; before ispell-hunspell-add-multi-dic will work
  ;;   (ispell-set-spellchecker-params)
  ;;   (ispell-hunspell-add-multi-dic "pl_PL,en_GB")
  ;;   (setq ispell-dictionary "pl_PL,en_GB"))

  ;; Utility Definitions
  (defun ct-define-key (keymap &rest bindings)
    (declare (indent 1))
    (while bindings
      (define-key keymap (pop bindings) (pop bindings))))

  ;; Variable Definitions
  (setq-default
   ;; Google Translate
   google-translate-default-source-language "en"
   google-translate-default-target-language "zh-CN"

   ;; calendar
   calendar-chinese-all-holidays-flag t
   calendar-chinese-celestial-stem ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"]
   calendar-chinese-terrestrial-branch ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"]
   calendar-chinese-location-name "北京"
   calendar-latitude [35 1 north]
   calendar-longitude [135 46 east]
   calendar-week-start-day 1

   ;; editor-misc
   isearch-allow-scroll t
   line-spacing 0.2
   tab-width 4
   ruler-mode-current-column-char 86
   size-indication-mode t
   smartparens-global-mode t
   paradox-github-token t

   spaceline-window-numbers-unicode t
   spaceline-workspace-numbers-unicode t
   spaceline-hud-p t
   sentence-end-double-space t
   powerline-default-separator nil
   x-underline-at-descent-line nil)

  (setq mark-holidays-in-calendar t)

  ;; holiday-fixed 用于设定公历事件
  ;; holiday-float 用于设定浮动公历事件
  ;; holiday-lunar 用于设定农历事件，各项的意义参考后面的 diary-float 说明。
  ;; holiday-solar-term 是一些预定义但还需要特殊计算的节日，如农历的24节气。
  (setq my-holidays
        '(;;公历节日
          (holiday-fixed 2 14 "情人节")
          (holiday-fixed 9 10 "教师节")
          (holiday-float 6 0 3 "父亲节")
          (holiday-float 5 0 2 "母亲节")
          ;; 农历节日
          (holiday-lunar 1 1 "春节" 0)
          (holiday-lunar 1 15 "元宵节" 0)
          (holiday-solar-term "清明" "清明节")
          (holiday-lunar 5 5 "端午节" 0)
          (holiday-lunar 7 7 "七夕情人节" 0)
          (holiday-lunar 8 15 "中秋节" 0)
          ;;纪念日
          ;; (holiday-fixed 1 1 "儿子生日")
          ;; (holiday-lunar 2 2 "老婆生日"  0)
          (holiday-lunar 3 26 "我的生日" 0)
          ))
  (setq calendar-holidays my-holidays)  ;只显示我定制的节假日

  ;; Keybindings
  (ct-define-key evil-normal-state-map
    (kbd "C-+") 'spacemacs/zoom-frm-in
    (kbd "C--") 'spacemacs/zoom-frm-out
    (kbd "C-_") 'spacemacs/zoom-frm-unzoom
    (kbd "C-n") 'evil-next-line-first-non-blank
    (kbd "C-p") 'evil-previous-line-first-non-blank)

  (ct-define-key yas-minor-mode-map
    ;; (kbd "TAB") 'yas-expand
    (kbd "C-S-n") 'yas-next-field
    (kbd "C-S-p") 'yas-prev-field)

  ;; File Format Association
  (dolist (e '(("pdb" . text-mode)
               ("gp" . gnuplot-mode)
               ("C" . c++-mode)
               ("h" . c++-mode)))
    (push (cons (concat "\\." (car e) "\\'") (cdr e)) auto-mode-alist))

  ;; Mode hooks
  (add-hook 'makefile-mode-hook 'whitespace-mode)
  (remove-hook 'prog-mode-hook 'spacemacs//show-trailing-whitespace)
  (add-hook 'prog-mode-hook
            (lambda ()
              (when (> (buffer-size) 100000)
                (turn-off-show-smartparens-mode))))

  ;; Diminish
  (when (eq 'hybrid dotspacemacs-editing-style)
    (diminish 'hybrid-mode))
  (with-eval-after-load 'helm-gtags
    (diminish 'helm-gtags-mode))

  ;; -------------------------------------------------------------------

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

  (add-to-list 'yas-snippet-dirs "~/.spacemacs.d/yasmate/snippets")

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

  (TeXlive 2018)
  (setq exec-path (append exec-path
                          '("/usr/local/texlive/2018/bin/x86_64-linux/")))

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

  (setq inhibit-compacting-font-caches t)
  (global-display-line-numbers-mode -1)

  (defun moon-override-yank-pop (&optional arg)
    "Delete the region before inserting poped string."
    (when (and evil-mode (eq 'visual evil-state))
      (kill-region (region-beginning) (region-end))))

  (advice-add 'counsel-yank-pop :before #'moon-override-yank-pop)
  (setq ivy-more-chars-alist '((counsel-ag . 2)
                               (counsel-grep .2)
                               (t . 3)))

  ;; boost find file and load saved persp layout  performance
  ;; which will break some function on windows platform
  ;; eg. known issues: magit related buffer color, reopen will fix it
  (when (spacemacs/system-is-mswindows)
    (progn (setq find-file-hook nil)
           (setq vc-handled-backends nil)
           (setq magit-refresh-status-buffer nil)
           (add-hook 'find-file-hook 'spacemacs/check-large-file)

           ;; emax.7z in not under pdumper release
           ;; https://github.com/m-parashar/emax64/releases/tag/pdumper-20180619
           (defvar emax-root (concat (expand-file-name "~") "/emax"))

           (when (file-exists-p emax-root)
             (progn
               (defvar emax-root (concat (expand-file-name "~") "/emax"))
               (defvar emax-bin64 (concat emax-root "/bin64"))
               (defvar emax-mingw64 (concat emax-root "/mingw64/bin"))
               (defvar emax-lisp (concat emax-root "/lisp"))

               (setq exec-path (cons emax-bin64 exec-path))
               (setenv "PATH" (concat emax-bin64 ";" (getenv "PATH")))

               (setq exec-path (cons emax-mingw64 exec-path))
               (setenv "PATH" (concat emax-mingw64 ";" (getenv "PATH")))
               ))

           (add-hook 'projectile-mode-hook '(lambda () (remove-hook 'find-file-hook #'projectile-find-file-hook-function)))))

  (defun counsel-locate-cmd-es (input)
    "Return a shell command based on INPUT."
    (counsel-require-program "es.exe")
    (encode-coding-string (format "es.exe -i -r -p %s"
                                  (counsel-unquote-regex-parens
                                   (ivy--regex input t)))
                          'gbk))
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
            '(lambda ()
               (ess-toggle-underscore nil)))

  ;; Draw box with IBM single line box characters. - activate with C-x d
  ;;
  ;; Press Esc to exit.
  ;;
  ;; Essentially treats the left hand on a QWERTY layout
  ;; as the box (i.e. QWE,ASD,ZXC)
  ;; Horizontal and vertical lines are on R -> horz and V -> vert
  ;; All other keys are available so you can move and
  ;; space as required.  The bindings are active until you hit Esc.
  ;;
  ;; Full bindings here.
  ;; q -> top left     ┌
  ;; w -> top          ┬
  ;; e -> top right    ┐
  ;; a -> left         ├
  ;; s -> center       ┼
  ;; d -> right        ┤
  ;; z -> bottom left  └
  ;; x -> bottom       ┴
  ;; c -> bottom right ┘
  ;; r -> horizontal   ─
  ;; v -> vertical     │

  ;; You can use Artist mode for things like this too, this is just yet another way. I built it for me.

;;; Code:

  ;; (global-set-key (kbd "C-x d")
  ;;                 (defhydra hydra-draw-box (:color pink)
  ;;                   "Draw box with IBM single line box characters (ESC to Quit)."
  ;;                   ("ESC" nil :color blue) ;; Esc to exit.
  ;;                   ("q" (lambda () (interactive) (insert "┌")) "top left ┌")
  ;;                   ("w" (lambda () (interactive) (insert "┬")) "top ┬")
  ;;                   ("e" (lambda () (interactive) (insert "┐")) "top right ┐")
  ;;                   ("a" (lambda () (interactive) (insert "├")) "left ├")
  ;;                   ("s" (lambda () (interactive) (insert "┼")) "center ┼")
  ;;                   ("d" (lambda () (interactive) (insert "┤")) "right ┤")
  ;;                   ("z" (lambda () (interactive) (insert "└")) "bottom left └")
  ;;                   ("x" (lambda () (interactive) (insert "┴")) "bottom ┴")
  ;;                   ("c" (lambda () (interactive) (insert "┘")) "bottom right ┘")
  ;;                   ("r" (lambda () (interactive) (insert "─")) "horizontal ─")
  ;;                   ("v" (lambda () (interactive) (insert "│")) "vertical │")))

  (with-eval-after-load 'transient
    (transient-bind-q-to-quit))

  )

(setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
(load custom-file 'no-error 'no-message)
;; -----------------------------------------------------------------
;; copy from https://github.com/noinil/spacemacs_layers/blob/master/init.el
(custom-set-faces
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(erc-input-face ((t (:foreground "green yellow"))))
 '(erc-my-nick-face ((t (:foreground "lime green" :weight ultra-bold))))
 '(erc-notice-face ((t (:foreground "#7F9F7F" :weight semi-light))))
 '(erc-prompt-face ((t (:foreground "yellow1" :weight extra-bold))))
 '(flycheck-warning ((t (:underline (:color "lime green" :style wave)))))
 '(info-title-1 ((t (:inherit default :height 1.0 :weight bold))))
 '(info-title-2 ((t (:inherit default :height 1.0 :weight bold))))
 '(info-title-3 ((t (:inherit default :height 1.0 :weight bold))))
 '(info-title-4 ((t (:inherit default :height 1.0 :weight bold))))
 '(isearch ((t (:foreground "green yellow" :weight bold))))
 '(lazy-highlight ((t (:foreground "orange" :weight bold))))
 '(markdown-header-face ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-1 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-2 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-3 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-4 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-5 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-6 ((t (:inherit default :height 1.0 :weight bold))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "magenta"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "purple"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "blue1"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "dark green"))))
 '(rainbow-delimiters-depth-10-face ((t (:foreground "#B4CDCD"))))
 '(ruler-mode-column-number ((t (:inherit ruler-mode-default :foreground "#B4CDCD"))))
 '(ruler-mode-comment-column ((t (:inherit ruler-mode-default :foreground "#CD3700"))))
 '(ruler-mode-default ((t (:inherit default :background "#1D2D1D" :foreground "seagreen" :box (:line-width 1 :color "#ADFF2F" :style released-button)))))
 '(ruler-mode-fill-column ((t (:inherit ruler-mode-default :foreground "#CD3700"))))
 '(ruler-mode-pad ((t (:inherit ruler-mode-default))))
 '(show-paren-mismatch ((t (:foreground "green yellow" :weight bold))))
 '(widget-field ((t (:background "dark gray" :foreground "blue3")))))
;; -----------------------------------------------------------------

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
