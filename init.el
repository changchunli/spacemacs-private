;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
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
              chinese-enable-fcitx t
              chinese-enable-youdao-dict t)
     chrome
     (clojure :variables clojure-enable-fancify-symbols t)
     cmake coffeescript
     (colors :variables
             colors-colorize-identifiers 'all
             colors-enable-nyan-cat-progress-bar t)
     common-lisp command-log confluence coq crystal cscope csharp csv
     d dash deft debug django docker elfeed
     (elm :variables
          elm-sort-imports-on-save t
          elm-format-on-save t
          elm-reactor-port "3000"
          elm-reactor-address "0.0.0.0")
     (elixir :variables flycheck-elixir-credo-strict t)
     emacs-lisp emoji epub erc erlang
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
     helm html hy
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     idris imenu-list ipython-notebook
     ;; ivy
     jabber
     (java :variables java-backend 'meghanada)
     (javascript :variables javascript-disable-tern-port-files t)
     jr json
     ;; keyboard-layout
     kotlin
     (latex :variables
            latex-build-command "LaTeX"
            latex-enable-auto-fill t
            latex-enable-magic nil
            latex-enable-folding t)
     lsp lua major-modes
     (markdown :variables markdown-live-preview-engine 'vmd)
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
          org-journal-dir "~/org-notes/journal/"
          org-journal-file-format "%Y-%m-%d"
          org-journal-date-prefix "#+TITLE: "
          org-journal-date-format "%A, %B %d %Y"
          org-journal-time-prefix "* "
          org-journal-time-format ""
          org-bullets-bullet-list '("■" "◆" "▲" "▶")
          org-projectile-file "~/org-notes/TODOs.org"
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
     pandoc parinfer pass pdf perl6 php
     (plantuml :variables
               plantuml-jar-path "~/.spacemacs.d/plantuml.jar"
               org-plantuml-jar-path "~/.spacemacs.d/plantuml.jar")
     prodigy protobuf puppet purescript
     (python :variables
             python-backend 'anaconda
             python-enable-yapf-format-on-save t
             python-fill-column 80
             python-sort-imports-on-save t
             ;; python-shell-completion-native-enable nil
             python-test-runner '(nose pytest))
     racket ranger rcirc react
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
            shell-default-shell 'ansi-term
            shell-default-position 'bottom
            shell-default-height 30
            shell-default-full-span nil
            shell-default-term-shell "/bin/zsh")
     shell-scripts slack smex sml spacemacs-evil spacemacs-language
     (spacemacs-layouts :variables
                        layouts-enable-autosave nil
                        layouts-autosave-delay 300)
     speed-reading
     (spell-checking :variables spell-checking-enable-by-default nil)
     sphinx spotify
     (sql :variables sql-capitalize-keywords t)
     swift
     (syntax-checking :variables
                      syntax-checking-enable-by-default nil
                      syntax-checking-enable-tooltips nil)
     systemd templates tern
     (terraform :variables terraform-auto-format-on-save t)
     themes-megapack theming tmux
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
   dotspacemacs-additional-packages '(linum-relative sicp wolfram)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(
     ;; magit-gh-pulls magit-gitflow evil-mc realgud
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
     ;; ido-vertical-mode flx-ido company-quickhelp
     ;; window-purpose ivy-purpose helm-purpose spacemacs-purpose-popwin
     org-mac-link)

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
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
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
   dotspacemacs-elpa-subdirectory nil

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
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
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
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
                         spacemacs-dark
                         solarized-dark
                         solarized-light
                         spacemacs-light
                         leuven)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
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
   dotspacemacs-large-file-size 1

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

   ;; If non-nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
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

   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
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
  (setq evil-shift-round nil)
  (setq byte-compile-warnings '(not obsolete))
  (setq warning-minimum-level :error)
  ;; hack for remove purpose mode
  (setq purpose-mode nil)
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included
in the dump."
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

  )

(setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
(load custom-file 'no-error 'no-message)

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
