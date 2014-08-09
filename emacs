(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

(when (> emacs-major-version 22)
        (add-to-list 'load-path "~/.emacs.d/el-get/evil")
        (require 'evil)
        (evil-mode 1)    
)


;mapc discards the value of the map (so its clearly used for its side effects).
(mapc
 (lambda (p)
   (add-to-list 'load-path p))
 '("~/.emacs.d/el-get/el-get" "~/mySrc/org-mode/lisp" "~/mySrc/org-mode/contrib/lisp"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(if window-system
    (load-theme 'zenburn t)
  (load-theme 'wombat t))

;Settings
(setq inhibit-splash-screen t)
(column-number-mode 1)
(display-time)
(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key "\C-cu" 'browse-url-at-point)
(defun google (query)
  "google search"
  (interactive "sSearch: ")
  (w3m-browse-url (concat "http://www.google.com/search?q=" (w3m-url-encode-string query))))
