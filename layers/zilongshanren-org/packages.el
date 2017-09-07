;;; packages.el --- zilong-ui layer packages file for Spacemacs.

;;
;; Copyright (c) 2014-2016 zilongshanren
;;
;; Author: guanghui <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst zilongshanren-org-packages
  '(
    (org :location built-in)
    org-mac-link
    org-pomodoro
    deft
    (blog-admin :location (recipe
                           :fetcher github
                           :repo "codefalling/blog-admin"))
    ;; org-tree-slide
    ;; ox-reveal
    ;; worf
    ;; org-download
    ;; plain-org-wiki
    )
  )

(defun zilongshanren-org/init-blog-admin ()
  (use-package blog-admin
    :defer t
    :commands blog-admin-start
    :init
    (progn
      ;; do your configuration here
      (setq blog-admin-backend-type 'hexo
            blog-admin-backend-path blog-admin-dir
            blog-admin-backend-new-post-with-same-name-dir nil
            blog-admin-backend-hexo-config-file "_config.yml"
            )
      (add-hook 'blog-admin-backend-after-new-post-hook 'find-file)
      )))

(defun zilongshanren-org/post-init-org-pomodoro ()
  (progn
    (add-hook 'org-pomodoro-finished-hook '(lambda () (zilongshanren/growl-notification "Pomodoro Finished" "☕️ Have a break!" t)))
    (add-hook 'org-pomodoro-short-break-finished-hook '(lambda () (zilongshanren/growl-notification "Short Break" "🐝 Ready to Go?" t)))
    (add-hook 'org-pomodoro-long-break-finished-hook '(lambda () (zilongshanren/growl-notification "Long Break" " 💪 Ready to Go?" t)))
    ))

;;In order to export pdf to support Chinese, I should install Latex at here: https://www.tug.org/mactex/
;; http://freizl.github.io/posts/2012-04-06-export-orgmode-file-in-Chinese.html
;;http://stackoverflow.com/questions/21005885/export-org-mode-code-block-and-result-with-different-styles
(defun zilongshanren-org/post-init-org ()
  (add-hook 'org-mode-hook (lambda () (spacemacs/toggle-line-numbers-off)) 'append)
  (with-eval-after-load 'org
    (progn
      
      (spacemacs|disable-company org-mode)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "," 'org-priority)
      (require 'org-compat)
      (require 'org)
      ;; (add-to-list 'org-modules "org-habit")
      (add-to-list 'org-modules 'org-habit)
      (require 'org-habit)

      (setq org-refile-use-outline-path 'file)
      (setq org-outline-path-complete-in-steps nil)
      (setq org-refile-targets
            '((nil :maxlevel . 4)
              (org-agenda-files :maxlevel . 4)))
      ;; config stuck project
      (setq org-stuck-projects
            '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

      (setq org-agenda-inhibit-startup t) ;; ~50x speedup
      (setq org-agenda-span 'day)
      (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
      (setq org-agenda-window-setup 'current-window)
      (setq org-log-done t
            org-edit-timestamp-down-means-later t
            org-archive-mark-done nil
            org-hide-emphasis-markers t
            org-catch-invisible-edits 'show
            ;; org-export-coding-system 'utf-8
            org-fast-tag-selection-single-key 'expert
            org-html-validation-link nil
            org-export-kill-product-buffer-when-displayed t
            org-tags-column 80
            org-support-shift-select t)

;;; from https://github.com/purcell/emacs.d/blob/master/lisp/init-org.el

      ;; Lots of stuff from http://doc.norang.ca/org-mode.html

      (defun sanityinc/grab-ditaa (url jar-name)
        "Download URL and extract JAR-NAME as `org-ditaa-jar-path'."
        ;; TODO: handle errors
        (message "Grabbing " jar-name " for org.")
        (let ((zip-temp (make-temp-name "emacs-ditaa")))
          (unwind-protect
              (progn
                (when (executable-find "unzip")
                  (url-copy-file url zip-temp)
                  (shell-command (concat "unzip -p " (shell-quote-argument zip-temp)
                                         " " (shell-quote-argument jar-name)
                                         " > " (shell-quote-argument org-ditaa-jar-path)))))
            (when (file-exists-p zip-temp)
              (delete-file zip-temp)))))

      (with-eval-after-load 'ob-ditaa
        (unless (and (boundp 'org-ditaa-jar-path)
                     (file-exists-p org-ditaa-jar-path))
          (let ((jar-name "ditaa.jar")
                (url "http://jaist.dl.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip"))
            (setq org-ditaa-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
            (unless (file-exists-p org-ditaa-jar-path)
              (sanityinc/grab-ditaa url jar-name)))))

      (define-minor-mode prose-mode
        "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
        nil " Prose" nil
        (if prose-mode
            (progn
              (when (fboundp 'writeroom-mode)
                (writeroom-mode 1))
              (setq truncate-lines nil)
              (setq word-wrap t)
              (setq cursor-type 'bar)
              (when (eq major-mode 'org)
                (kill-local-variable 'buffer-face-mode-face))
              (buffer-face-mode 1)
              ;;(delete-selection-mode 1)
              (set (make-local-variable 'blink-cursor-interval) 0.6)
              (set (make-local-variable 'show-trailing-whitespace) nil)
              (ignore-errors (flyspell-mode 1))
              (visual-line-mode 1))
          (kill-local-variable 'truncate-lines)
          (kill-local-variable 'word-wrap)
          (kill-local-variable 'cursor-type)
          (kill-local-variable 'show-trailing-whitespace)
          (buffer-face-mode -1)
          ;; (delete-selection-mode -1)
          (flyspell-mode -1)
          (visual-line-mode -1)
          (when (fboundp 'writeroom-mode)
            (writeroom-mode 0))))

      ;;(add-hook 'org-mode-hook 'buffer-face-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Refiling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (setq org-refile-use-cache nil)

      ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
      (setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

      (defadvice org-refile (after sanityinc/save-all-after-refile activate)
        "Save all org buffers after each refile operation."
        (org-save-all-org-buffers))

      ;; Exclude DONE state tasks from refile targets
      (defun sanityinc/verify-refile-target ()
        "Exclude todo keywords with a done state from refile targets."
        (not (member (nth 2 (org-heading-components)) org-done-keywords)))
      (setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

      (defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
        "A version of `org-refile' which allows refiling to any subtree."
        (interactive "P")
        (let ((org-refile-target-verify-function))
          (org-refile goto default-buffer rfloc msg)))

      (defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
        "A version of `org-agenda-refile' which allows refiling to any subtree."
        (interactive "P")
        (let ((org-refile-target-verify-function))
          (org-agenda-refile goto rfloc no-update)))

      ;; Targets start with the file name - allows creating level 1 tasks
      ;;(setq org-refile-use-outline-path (quote file))
      (setq org-refile-use-outline-path t)
      (setq org-outline-path-complete-in-steps nil)

      ;; Allow refile to create parent tasks with confirmation
      (setq org-refile-allow-creating-parent-nodes 'confirm)


      ;; 加密文章
      ;; "http://coldnew.github.io/blog/2013/07/13_5b094.html"
      ;; org-mode 設定
      (require 'org-crypt)

      ;; 當被加密的部份要存入硬碟時，自動加密回去
      (org-crypt-use-before-save-magic)

      ;; 設定要加密的 tag 標籤為 secret
      (setq org-crypt-tag-matcher "secret")

      ;; 避免 secret 這個 tag 被子項目繼承 造成重複加密
      ;; (但是子項目還是會被加密喔)
      (setq org-tags-exclude-from-inheritance (quote ("secret")))

      ;; 用於加密的 GPG 金鑰
      ;; 可以設定任何 ID 或是設成 nil 來使用對稱式加密 (symmetric encryption)
      (setq org-crypt-key nil)

      ;; (add-to-list 'auto-mode-alist '("\.org\\'" . org-mode))

      (setq org-todo-keywords
            (quote ((sequence "TODO(t)" "STARTED(s)" "NEXT(n)" "|" "DONE(d!/!)")
                    (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
                    (sequence "WAITING(w@/!)" "SOMEDAY(S)" "DELEGATED(e!)" 
                              "HOLD(h)" "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)")))
            org-todo-repeat-to-state "NEXT")

      (setq org-todo-keyword-faces
            (quote (("NEXT" :inherit warning)
                    ("PROJECT" :inherit font-lock-string-face))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; Save the running clock and all clock history when exiting Emacs, load it on startup
      (org-clock-persistence-insinuate)
      (setq org-clock-persist t)
      (setq org-clock-in-resume t)

      ;; Change task state to STARTED when clocking in
      (setq org-clock-in-switch-to-state "STARTED")
      ;; Save clock data and notes in the LOGBOOK drawer
      (setq org-clock-into-drawer t)
      ;; Removes clocked tasks with 0:00 duration
      (setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line
      ;; Show clock sums as hours and minutes, not "n days" etc.
      (setq org-time-clocksum-format
            '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

      ;; Remove empty LOGBOOK drawers on clock out
      (defun sanityinc/remove-empty-drawer-on-clock-out ()
        (interactive)
        (save-excursion
          (beginning-of-line 0)
          (org-remove-empty-drawer-at "LOGBOOK" (point))))

      (with-eval-after-load 'org-clock
                  (add-hook 'org-clock-out-hook 'sanityinc/remove-empty-drawer-on-clock-out 'append))



      ;; TODO: warn about inconsistent items, e.g. TODO inside non-PROJECT
      ;; TODO: nested projects!


;;; Archiving

      (setq org-archive-mark-done nil)
      (setq org-archive-location "%s_archive::* Archive")


;;; Show the clocked-in task - if any - in the header line
      (defun sanityinc/show-org-clock-in-header-line ()
        (setq-default header-line-format '((" " org-mode-line-string " "))))

      (defun sanityinc/hide-org-clock-from-header-line ()
        (setq-default header-line-format nil))

      (add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
      (add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
      (add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

      (setq org-tags-match-list-sublevels nil)

      (add-hook 'org-mode-hook '(lambda ()
                                  ;; keybinding for editing source code blocks
                                  ;; keybinding for inserting code blocks
                                  (local-set-key (kbd "C-c i s")
                                                 'zilongshanren/org-insert-src-block)))
      (require 'ox-publish)
      (add-to-list 'org-latex-classes '("ctexart" "\\documentclass[11pt]{ctexart}
                                        [NO-DEFAULT-PACKAGES]
                                        \\usepackage[utf8]{inputenc}
                                        \\usepackage[T1]{fontenc}
                                        \\usepackage{fixltx2e}
                                        \\usepackage{graphicx}
                                        \\usepackage{longtable}
                                        \\usepackage{float}
                                        \\usepackage{wrapfig}
                                        \\usepackage{rotating}
                                        \\usepackage[normalem]{ulem}
                                        \\usepackage{amsmath}
                                        \\usepackage{textcomp}
                                        \\usepackage{marvosym}
                                        \\usepackage{wasysym}
                                        \\usepackage{amssymb}
                                        \\usepackage{booktabs}
                                        \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
                                        \\tolerance=1000
                                        \\usepackage{listings}
                                        \\usepackage{xcolor}
                                        \\lstset{
                                        %行号
                                        numbers=left,
                                        %背景框
                                        framexleftmargin=10mm,
                                        frame=none,
                                        %背景色
                                        %backgroundcolor=\\color[rgb]{1,1,0.76},
                                        backgroundcolor=\\color[RGB]{245,245,244},
                                        %样式
                                        keywordstyle=\\bf\\color{blue},
                                        identifierstyle=\\bf,
                                        numberstyle=\\color[RGB]{0,192,192},
                                        commentstyle=\\it\\color[RGB]{0,96,96},
                                        stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
                                        %显示空格
                                        showstringspaces=false
                                        }
                                        "
                                        ("\\section{%s}" . "\\section*{%s}")
                                        ("\\subsection{%s}" . "\\subsection*{%s}")
                                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      ;; {{ export org-mode in Chinese into PDF
      ;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
      ;; and you need install texlive-xetex on different platforms
      ;; To install texlive-xetex:
      ;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
      ;; }}

      (add-to-list 'org-latex-classes '("article" "\\documentclass[12pt,a4paper,german,normalheadings]{article}
                                        \\usepackage{bm}
                                        \\usepackage{amsfonts}
                                        %% \\usepackage{CJK}
                                        \\usepackage{graphicx}
                                        \\usepackage{amsmath}
                                        \\usepackage{amssymb}
                                        \\usepackage{subfigure}
                                        \\usepackage{indentfirst}
                                        \\usepackage{multicol}    % 正文双栏
                                        %% \\usepackage{picins}      % 图片嵌入段落宏包 比如照片 % Something will have error
                                        \\usepackage{abstract}    % 2栏文档，一栏摘要及关键字宏包
                                        \\usepackage{anysize} % 对于像 book 等双面版式来说，这里的 left 和 right 再奇偶页会互换。
                                        %% \\usepackage{hyperref} % 文献引用的宏包
                                        \\usepackage{listings}\\lstloadlanguages{C,C++,matlab,mathematica} %程序清单关键字宏包
                                        \\usepackage{color} % 可以产生有颜色的符号
                                        \\usepackage{units} % 用于美化单位及分式
                                        \\usepackage{tabularx} % 用于灵活地控制表格的生成
                                        \\usepackage{mathrsfs} % 用于产生一种数学用的花体字
                                        \\usepackage{xcolor}
                                        \\usepackage{xeCJK}
                                        \\usepackage{lmodern}
                                        \\usepackage{verbatim}
                                        \\usepackage{fixltx2e}
                                        \\usepackage{longtable}
                                        \\usepackage{float}
                                        \\usepackage{tikz}
                                        \\usepackage{wrapfig}
                                        \\usepackage{soul}
                                        \\usepackage{textcomp}
                                        \\usepackage{listings}
                                        \\usepackage{geometry}
                                        \\usepackage{algorithm}
                                        \\usepackage{algorithmic}
                                        \\usepackage{marvosym}
                                        \\usepackage{wasysym}
                                        \\usepackage{latexsym}
                                        \\usepackage{natbib}
                                        \\usepackage{fancyhdr}
                                        \\usepackage{fancyvrb}
                                        \\usepackage{fancybox}
                                        \\usepackage[xetex,colorlinks=true,CJKbookmarks=true,
                                                      linkcolor=blue,
                                                      urlcolor=blue,
                                                      menucolor=blue]{hyperref}
                                        \\usepackage{fontspec,xunicode,xltxtra}
                                        %% \\usepackage{chngcntr}
                                        %% \\counterwithout{equation}{chapter}
                                        %% \\counterwithout{equation}{section}
                                        %% \\setmainfont[BoldFont=Adobe Heiti Std]{Adobe Song Std}
                                        %% \\setsansfont[BoldFont=Adobe Heiti Std]{AR PL UKai CN}
                                        %% \\setmonofont{Bitstream Vera Sans Mono}  
                                        %% \\newcommand\\fontnamemono{AR PL UKai CN}%等宽字体
                                        %% \\newfontinstance\\MONO{\\fontnamemono}
                                        %% \\newcommand{\\mono}[1]{{\\MONO #1}}
                                        %% \\setCJKmainfont[Scale=0.9]{Adobe Heiti Std}%中文字体
                                        %% \\setCJKmonofont[Scale=0.9]{Adobe Heiti Std}
                                        \\hypersetup{unicode=true}
                                        \\geometry{a4paper, textwidth=6.5in, textheight=10in,
                                        marginparsep=7pt, marginparwidth=.6in}
                                        \\definecolor{foreground}{RGB}{220,220,204}%浅灰
                                        \\definecolor{background}{RGB}{62,62,62}%浅黑
                                        \\definecolor{preprocess}{RGB}{250,187,249}%浅紫
                                        \\definecolor{var}{RGB}{239,224,174}%浅肉色
                                        \\definecolor{string}{RGB}{154,150,230}%浅紫色
                                        \\definecolor{type}{RGB}{225,225,116}%浅黄
                                        \\definecolor{function}{RGB}{140,206,211}%浅天蓝
                                        \\definecolor{keyword}{RGB}{239,224,174}%浅肉色
                                        \\definecolor{comment}{RGB}{180,98,4}%深褐色
                                        \\definecolor{doc}{RGB}{175,215,175}%浅铅绿
                                        \\definecolor{comdil}{RGB}{111,128,111}%深灰
                                        \\definecolor{constant}{RGB}{220,162,170}%粉红
                                        \\definecolor{buildin}{RGB}{127,159,127}%深铅绿
                                        \\punctstyle{kaiming}
                                        \\title{}
                                        \\fancyfoot[C]{\\bfseries\\thepage}
                                        \\chead{\\MakeUppercase\\sectionmark}
                                        \\pagestyle{fancy}
                                        \\tolerance=1000
                                        [NO-DEFAULT-PACKAGES]
                                        [NO-PACKAGES]"
                                        ("\\section{%s}" . "\\section*{%s}")
                                        ("\\subsection{%s}" . "\\subsection*{%s}")
                                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      (setq org-latex-default-class "article")
      (setq org-latex-pdf-process
            '(
              "xelatex -interaction nonstopmode -output-directory %o %f"
              "xelatex -interaction nonstopmode -output-directory %o %f"
              "xelatex -interaction nonstopmode -output-directory %o %f"
              "rm -fr %b.out %b.log %b.tex auto"))
      
      (setq org-latex-listings t)
      ;; Options for \lset command（reference to listing Manual)
      (setq org-latex-listings-options
            '(
              ("basicstyle" "\\color{foreground}\\small\\mono")           ; 源代码字体样式
              ("keywordstyle" "\\color{function}\\bfseries\\small\\mono") ; 关键词字体样式
              ("identifierstyle" "\\color{doc}\\small\\mono")
              ("commentstyle" "\\color{comment}\\small\\itshape")         ; 批注样式
              ("stringstyle" "\\color{string}\\small")                    ; 字符串样式
              ("showstringspaces" "false")                                ; 字符串空格显示
              ("numbers" "left")                                          ; 行号显示
              ("numberstyle" "\\color{preprocess}")                       ; 行号样式
              ("stepnumber" "1")                                          ; 行号递增
              ("backgroundcolor" "\\color{background}")                   ; 代码框背景色
              ("tabsize" "4")                                             ; TAB等效空格数
              ("captionpos" "t")                                          ; 标题位置 top or buttom(t|b)
              ("breaklines" "true")                                       ; 自动断行
              ("breakatwhitespace" "true")                                ; 只在空格分行
              ("showspaces" "false")                                      ; 显示空格
              ("columns" "flexible")                                      ; 列样式
              ("frame" "single")                                          ; 代码框：阴影盒
              ("frameround" "tttt")                                       ; 代码框： 圆角
              ("framesep" "0pt")
              ("framerule" "8pt")
              ("rulecolor" "\\color{background}")
              ("fillcolor" "\\color{white}")
              ("rulesepcolor" "\\color{comdil}")
              ("framexleftmargin" "10mm")
              ))

      ;; 导出Beamer的设置
      ;; allow for export=>beamer by placing #+LaTeX_CLASS: beamer in org files
      ;;-----------------------------------------------------------------------------
      (add-to-list 'org-latex-classes
                   ;; beamer class, for presentations
                   '("beamer"
                              "\\documentclass[11pt,professionalfonts]{beamer}
                              \\mode
                              \\usetheme{{{{Warsaw}}}}
                              %\\usecolortheme{{{{beamercolortheme}}}}
  
                              \\beamertemplateballitem
                              \\setbeameroption{show notes}
                              \\usepackage{graphicx}
                              \\usepackage{tikz}
                              \\usepackage{xcolor}
                              \\usepackage{xeCJK}
                              \\usepackage{amsmath}
                              \\usepackage{lmodern}
                              \\usepackage{fontspec,xunicode,xltxtra}
                              \\usepackage{polyglossia}
                              %% \\setmainfont{Times New Roman}
                              %% \\setCJKmainfont{DejaVu Sans YuanTi}
                              %% \\setCJKmonofont{DejaVu Sans YuanTi Mono}
                              \\usepackage{verbatim}
                              \\usepackage{listings}
                              \\institute{{{{beamerinstitute}}}}
                              \\subject{{{{beamersubject}}}}"
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\begin{frame}[fragile]\\frametitle{%s}"
                     "\\end{frame}"
                     "\\begin{frame}[fragile]\\frametitle{%s}"
                     "\\end{frame}")))

      (setq ps-paper-type 'a4
            ps-font-size 16.0
            ps-print-header nil
            ps-landscape-mode nil)

      ;;reset subtask
      (setq org-default-properties (cons "RESET_SUBTASKS" org-default-properties))

      ;; (add-hook 'org-after-todo-state-change-hook 'org-subtask-reset)

      (setq org-plantuml-jar-path
            (expand-file-name "~/.spacemacs.d/plantuml.jar"))
      (setq org-ditaa-jar-path "~/.spacemacs.d/ditaa.jar")

      (org-babel-do-load-languages
       'org-babel-load-languages
       '((perl . t)
         ;; (R . t)
         (gnuplot . t)
         (haskell . nil)
         (ledger . t)
         (ocaml . nil)
         (octave . t)
         (ruby . t)
         (sh . t)
         (screen . nil)
         ;; (,(if (locate-library "ob-sh") 'sh 'shell) . t)
         (sql . nil)
         (sqlite . t)
         (dot . t)
         (js . t)
         (latex .t)
         (python . t)
         (emacs-lisp . t)
         (matlab . t)
         (plantuml . t)
         (C . t)
         (ditaa . t)))


      (require 'ox-md nil t)
      ;; copy from chinese layer
      (defadvice org-html-paragraph (before org-html-paragraph-advice
                                            (paragraph contents info) activate)
        "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
        (let* ((origin-contents (ad-get-arg 1))
               (fix-regexp "[[:multibyte:]]")
               (fixed-contents
                (replace-regexp-in-string
                 (concat
                  "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
          (ad-set-arg 1 fixed-contents)))

      ;; define the refile targets
      (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
      (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
      (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
      (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
      (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
      (setq org-agenda-files (list org-agenda-dir))

      (with-eval-after-load 'org-agenda
        (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
        (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
          "." 'spacemacs/org-agenda-transient-state/body)
        (add-to-list 'org-agenda-after-show-hook 'org-show-entry)
        )
      ;; the %i would copy the selected text into the template
      ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
      ;;add multi-file journal
      (setq org-capture-templates
            '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
               "* TODO [#B] %?\n  %i\n"
               :empty-lines 1)
              ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
               "* %?\n  %i\n %U"
               :empty-lines 1)
              ("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
               "* TODO [#B] %?\n  %i\n %U"
               :empty-lines 1)
              ("s" "Code Snippet" entry
               (file org-agenda-file-code-snippet)
               "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
              ("w" "work" entry (file+headline org-agenda-file-gtd "Papers")
               "* TODO [#A] %?\n  %i\n %U"
               :empty-lines 1)
              ("c" "Chrome" entry (file+headline org-agenda-file-note "Quick notes")
               "* TODO [#C] %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
               :empty-lines 1)
              ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
               "* TODO [#C] %?\n  %i\n %a \n %U"
               :empty-lines 1)
              ("j" "Journal Entry"
               entry (file+datetree org-agenda-file-journal)
               "* %?"
               :empty-lines 1)))

      ;;; Agenda views

      (setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))


      (let ((active-project-match "-INBOX/PROJECT"))

        (setq org-stuck-projects
              `(,active-project-match ("NEXT")))

        (setq org-agenda-compact-blocks t
              org-agenda-sticky t
              ;; org-agenda-start-on-weekday nil
              org-agenda-span 'day
              org-agenda-include-diary nil
              org-agenda-sorting-strategy
              '((agenda habit-down time-up user-defined-up effort-up category-keep)
                (todo category-up effort-up)
                (tags category-up effort-up)
                (search category-up))
              org-agenda-window-setup 'current-window
              org-agenda-custom-commands
              `(("N" "Notes" tags "NOTE"
                 ((org-agenda-overriding-header "Notes")
                  (org-tags-match-list-sublevels t)))
                ("g" "GTD"
                 ((agenda "" nil)
                  (tags "INBOX"
                        ((org-agenda-overriding-header "Inbox")
                         (org-tags-match-list-sublevels nil)))
                  (stuck ""
                         ((org-agenda-overriding-header "Stuck Projects")
                          (org-agenda-tags-todo-honor-ignore-options t)
                          (org-tags-match-list-sublevels t)
                          (org-agenda-todo-ignore-scheduled 'future)))
                  (tags-todo "-INBOX"
                             ((org-agenda-overriding-header "Next Actions")
                              (org-agenda-tags-todo-honor-ignore-options t)
                              (org-agenda-todo-ignore-scheduled 'future)
                              (org-agenda-skip-function
                               '(lambda ()
                                  (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                                      (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                              (org-tags-match-list-sublevels t)
                              (org-agenda-sorting-strategy
                               '(todo-state-down effort-up category-keep))))
                  (tags-todo ,active-project-match
                             ((org-agenda-overriding-header "Projects")
                              (org-tags-match-list-sublevels t)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-INBOX/-NEXT"
                             ((org-agenda-overriding-header "Orphaned Tasks")
                              (org-agenda-tags-todo-honor-ignore-options t)
                              (org-agenda-todo-ignore-scheduled 'future)
                              (org-agenda-skip-function
                               '(lambda ()
                                  (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
                                      (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                              (org-tags-match-list-sublevels t)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "/WAITING"
                             ((org-agenda-overriding-header "Waiting")
                              (org-agenda-tags-todo-honor-ignore-options t)
                              (org-agenda-todo-ignore-scheduled 'future)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "/DELEGATED"
                             ((org-agenda-overriding-header "Delegated")
                              (org-agenda-tags-todo-honor-ignore-options t)
                              (org-agenda-todo-ignore-scheduled 'future)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-INBOX"
                             ((org-agenda-overriding-header "On Hold")
                              (org-agenda-skip-function
                               '(lambda ()
                                  (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                                      (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                              (org-tags-match-list-sublevels nil)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  ;; (tags-todo "-NEXT"
                  ;;            ((org-agenda-overriding-header "All other TODOs")
                  ;;             (org-match-list-sublevels t)))
                  ))
                ;;An entry without a cookie is treated just like priority ' B '.
                ;;So when create new task, they are default 重要且紧急
                ("w" . "任务安排")
                ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
                ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
                ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
                ("b" "Blog" tags-todo "BLOG")
                ("p" . "项目安排")
                ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"papers\"")
                ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"changchunli\"")
                ("W" "Weekly Review"
                 ((stuck "") ;; review stuck projects as designated by org-stuck-projects
                  (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
                  )))))


      (add-hook 'org-agenda-mode-hook 'hl-line-mode)


      (defvar zilongshanren-website-html-preamble
        "<div class='nav'>
<ul>
<li><a href='http://zilongshanren.com'>博客</a></li>
<li><a href='/index.html'>Wiki目录</a></li>
</ul>
</div>")
      (defvar zilongshanren-website-html-blog-head
        " <link rel='stylesheet' href='css/site.css' type='text/css'/> \n
    <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/worg.css\"/>")
      (setq org-publish-project-alist
            `(
              ("blog-notes"
               :base-directory "~/org-notes"
               :base-extension "org"
               :publishing-directory "~/org-notes/public_html/"

               :recursive t
               :html-head , zilongshanren-website-html-blog-head
               :publishing-function org-html-publish-to-html
               :headline-levels 4       ; Just the default for this project.
               :auto-preamble t
               :exclude "gtd.org"
               :exclude-tags ("ol" "noexport")
               :section-numbers nil
               :html-preamble ,zilongshanren-website-html-preamble
               :author "Changchun Li"
               :email "changchunli93@gmail.com"
               :auto-sitemap t          ; Generate sitemap.org automagically...
               :sitemap-filename "index.org" ; ... call it sitemap.org (it's the default)...
               :sitemap-title "我的wiki"     ; ... with title 'Sitemap'.
               :sitemap-sort-files anti-chronologically
               :sitemap-file-entry-format "%t" ; %d to output date, we don't need date here
               )
              ("blog-static"
               :base-directory "~/org-notes"
               :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
               :publishing-directory "~/org-notes/public_html/"
               :recursive t
               :publishing-function org-publish-attachment
               )
              ("blog" :components ("blog-notes" "blog-static"))))



      (add-hook 'org-after-todo-statistics-hook 'zilong/org-summary-todo)
      ;; used by zilong/org-clock-sum-today-by-tags

      (define-key org-mode-map (kbd "s-p") 'org-priority)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "tl" 'org-toggle-link-display)
      (define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile)

      ;; hack for org headline toc
      (defun org-html-headline (headline contents info)
        "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
        (unless (org-element-property :footnote-section-p headline)
          (let* ((numberedp (org-export-numbered-headline-p headline info))
                 (numbers (org-export-get-headline-number headline info))
                 (section-number (and numbers
                                      (mapconcat #'number-to-string numbers "-")))
                 (level (+ (org-export-get-relative-level headline info)
                           (1- (plist-get info :html-toplevel-hlevel))))
                 (todo (and (plist-get info :with-todo-keywords)
                            (let ((todo (org-element-property :todo-keyword headline)))
                              (and todo (org-export-data todo info)))))
                 (todo-type (and todo (org-element-property :todo-type headline)))
                 (priority (and (plist-get info :with-priority)
                                (org-element-property :priority headline)))
                 (text (org-export-data (org-element-property :title headline) info))
                 (tags (and (plist-get info :with-tags)
                            (org-export-get-tags headline info)))
                 (full-text (funcall (plist-get info :html-format-headline-function)
                                     todo todo-type priority text tags info))
                 (contents (or contents ""))
                 (ids (delq nil
                            (list (org-element-property :CUSTOM_ID headline)
                                  (org-export-get-reference headline info)
                                  (org-element-property :ID headline))))
                 (preferred-id (car ids))
                 (extra-ids
                  (mapconcat
                   (lambda (id)
                     (org-html--anchor
                      (if (org-uuidgen-p id) (concat "ID-" id) id)
                      nil nil info))
                   (cdr ids) "")))
            (if (org-export-low-level-p headline info)
                ;; This is a deep sub-tree: export it as a list item.
                (let* ((type (if numberedp 'ordered 'unordered))
                       (itemized-body
                        (org-html-format-list-item
                         contents type nil info nil
                         (concat (org-html--anchor preferred-id nil nil info)
                                 extra-ids
                                 full-text))))
                  (concat (and (org-export-first-sibling-p headline info)
                               (org-html-begin-plain-list type))
                          itemized-body
                          (and (org-export-last-sibling-p headline info)
                               (org-html-end-plain-list type))))
              (let ((extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
                    (first-content (car (org-element-contents headline))))
                ;; Standard headline.  Export it as a section.
                (format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
                        (org-html--container headline info)
                        (org-export-get-reference headline info)
                        (concat (format "outline-%d" level)
                                (and extra-class " ")
                                extra-class)
                        (format "\n<h%d id=\"%s\">%s%s</h%d>\n"
                                level
                                preferred-id
                                extra-ids
                                (concat
                                 (and numberedp
                                      (format
                                       "<span class=\"section-number-%d\">%s</span> "
                                       level
                                       (mapconcat #'number-to-string numbers ".")))
                                 full-text)
                                level)
                        ;; When there is no section, pretend there is an
                        ;; empty one to get the correct <div
                        ;; class="outline-...> which is needed by
                        ;; `org-info.js'.
                        (if (eq (org-element-type first-content) 'section) contents
                          (concat (org-html-section first-content "" info) contents))
                        (org-html--container headline info))))))))))

(defun zilongshanren-org/init-org-mac-link ()
  (use-package org-mac-link
    :commands org-mac-grab-link
    :init
    (progn
      (add-hook 'org-mode-hook
                (lambda ()
                  (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link))))
    :defer t))

(defun zilongshanren-org/post-init-ox-reveal ()
  (setq org-reveal-root "file:////home/william/.emacs.d/reveal-js"))


(defun zilongshanren-org/init-org-tree-slide ()
  (use-package org-tree-slide
    :init
    (spacemacs/set-leader-keys "oto" 'org-tree-slide-mode)))


(defun zilongshanren-org/init-org-download ()
  (use-package org-download
    :defer t
    :init
    (org-download-enable)))

(defun zilongshanren-org/init-plain-org-wiki ()
  (use-package plain-org-wiki
    :init
    (setq pow-directory "~/org-notes")))

(defun zilongshanren-org/init-worf ()
  (use-package worf
    :defer t
    :init
    (add-hook 'org-mode-hook 'worf-mode)))

(defun zilongshanren-org/post-init-deft ()
  (progn
    (setq deft-use-filter-string-for-filename t)
    (setq deft-recursive t)
    (setq deft-extension "org")
    (setq deft-directory deft-dir)))
;;; packages.el ends here
