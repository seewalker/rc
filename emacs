;questions:
;  how to make commands which take a repetition argument. e.g. <int>C-x}
;  8l

;its as if /usr/local/bin isn't being searched.
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH") ))
(setenv "ESHELL" "/usr/local/bin/zsh")

(when (> emacs-major-version 22)
        (add-to-list 'load-path "~/.emacs.d/el-get/evil")
        (require 'evil)
        (evil-mode 1)    
)

(defun toggle-fullscreen ()
    "Toggle full screen"
    (interactive)
    (set-frame-parameter
      nil 'fullscreen
      (when (not (frame-parameter nil 'fullscreen)) 'fullboth))) 
(global-set-key (kbd "C-S-f") 'toggle-fullscreen)

;mapc discards the value of the map (so its clearly used for its side effects).
(mapc
 (lambda (p)
   (add-to-list 'load-path p))
 '("~/.emacs.d/el-get/el-get" "~/mySrc/org-mode/lisp" "~/mySrc/org-mode/contrib/lisp" ".emacs.d/plugin"))

(mapc
 (lambda (pair)
   (add-to-list 'auto-mode-alist pair))
 '( ("\\.hs\\'" . haskell-mode) ("\\.org\\'" . org-mode)))
;greek resolution:
(defun pretty-greek ()
  (let ((greek '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "omicron" "pi" "rho" "sigma_final" "sigma" "tau" "upsilon" "phi" "chi" "psi" "omega")))
    (loop for word in greek
          for code = 97 then (+ 1 code)
          do  (let ((greek-char (make-char 'greek-iso8859-7 code))) 
                (font-lock-add-keywords nil
				    `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[a-zA-Z]")
                                           (0 (progn (decompose-region (match-beginning 2) (match-end 2))
                                                     nil)))))
                (font-lock-add-keywords nil 
                                        `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[^a-zA-Z]")
                                           (0 (progn (compose-region (match-beginning 2) (match-end 2)
                                                                     ,greek-char)
                                                     nil)))))))))  (add-hook 'lisp-mode-hook 'pretty-greek)
(add-hook 'emacs-lisp-mode-hook 'pretty-greek)

;org mode stuff
(add-hook 'org-mode-hook (lambda () (org-indent-mode t)) t)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(setq org-clock-persist 'history)
(setq org-startup-align-all-tables t)
(setq org-feed-alist
      '( ("Slashdot"
	  "http://rss.slashdot.org/Slashdot/slashdot"
	  "~/org/feeds.org" "Slashdot")
	 )
)
(org-clock-persistence-insinuate)
(org-babel-do-load-languages
 'org-babel-load-languages
 '( (python . t) (R . t) (perl . t) (haskell . t) (latex . t)))
(info "(Org)Languages")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(if window-system
    (load-theme 'zenburn t)
  (load-theme 'wombat t))
(require 'rainbow-delimiters)
; prog-mode-hook is triggered when a buffer is determined to contain source code.
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;eldoc stuff:

; This dolist call (a macro) is supposed to do what the next three lines do,
; but for some reason it doesn't work.
;(let (hook)
 ; (dolist ('emacs-lisp-mode-hook)
  ;  (add-hook hook 'turn-on-eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(load "c-eldoc")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;mode-line stuff
(require 'octicons)
(make-face 'octicons-mode-line)
(set-face-attribute 'octicons-mode-line nil
                    :inherit 'mode-line
                    :inherit 'octicons)

(setq-default mode-line-format (list
    " "
    '(:eval (if (vc-backend buffer-file-name)
                (list
                 (propertize octicon-octoface 'face 'octicons-modeline)
                 (propertize " "              'face 'mode-line))))
   mode-line-mule-info
   'mode-line-modified
   "-  "
   'mode-line-buffer-identification
   "  (%l, %c)  "
   'mode-line-modes
   " -- "
   `(vc-mode vc-mode)
))

;Settings
(setq inhibit-splash-screen t)
(display-time)
(defalias 'yes-or-no-p 'y-or-n-p)

(defvar terminal-notifier-command (executable-find "terminal-notifier") "The path to terminal-notifier.")
 
(defun terminal-notifier-notify (title message)
  "Show a message with `terminal-notifier-command`."
  (start-process "terminal-notifier"
                 "*terminal-notifier*"
                 terminal-notifier-command
                 "-title" title
                 "-message" message
                 "-activate" "org.gnu.Emacs"))

;This doesn't seem to actually work. 
(defun timed-notification(time msg)
  (interactive "sNotification when (e.g: 2 minutes, 60 seconds, 3 days): \nsMessage: ")
  (run-at-time time nil (lambda (msg) (terminal-notifier-notify "Emacs" msg)) msg))

;ipython setup
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
