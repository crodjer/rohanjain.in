+++
title = "Emacs: Flymake with virtualenvs in python-mode"
slug = "emacs-flymake-with-virtualenvs-in-python-mode"
date = "2014-01-28T00:00:00Z"
publishdate = "2014-01-28T00:00:00Z"
+++

Lately, I have been writing some python with emacs. So, I have been trying to
get the popular checkers (`pylint`/`pyflakes`) to work with flymake and
virtualenvs. The issues with existing solutions to get flymake working with the
checkers is that most of them assume a global version of executable (which would
be fine if it weren't for the whole python 2 and 3 incompatibility).

The `python-mode` in the latest emacs versions (mine is `24.3.1`) includes a
basic support for `virtualenvs`. I run `pylint/pyflakes` through the command
`env` with the environment variables calculated from the functions provided by
`python-mode`. Here is the code extracted from my
[emacs configuration](https://github.com/crodjer/configs/blob/master/.emacs):

```elisp
(defun python-calculate-env ()
  "Calculate env variables for current python virtualenv."
  ;; ;; This also overrides PYTHONHOME to "", which breaks the process
  ;; ;; environment. Otherwise, a neat idea.
  ;; (mapcar
  ;;  (lambda (string)
  ;;    (let ((splitted-string (split-string string "=")))
  ;;      (format
  ;;       "%s=\"%s\""
  ;;       (car splitted-string)
  ;;       (or (cadr splitted-string)
  ;;           ""))))
  ;; (python-shell-calculate-process-environment)))
  (remove-if
   (lambda (x)
     (or
      ;; If environment
      (string-match " " x)
      (not (string-match "=" x))))
   (python-shell-calculate-process-environment)))

(defun python-virtualenv-exec (command args)
  "Generate a flymake friendly list executable in virtualenv, for provided
commands."
  (list "env" (append (python-calculate-env) (list command) args)))

(when (load "flymake" t)
  (defun flymake-python-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (python-virtualenv-exec
       "pylint"
       (list
        ;; Pylint args. Will depend on the checker being used.
        "-r" "n"
        "--msg-template='{path}:{line}:{category} [{msg_id} {obj}] {msg}'"
        local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-python-init)))

(defun flymake-mode-hook-function ()
  (when (derived-mode-p 'python-mode)
    (flymake-mode t)))
;; Run flymake mode hook function after the local variables are set (eg: through
;; .dir-locals.el
(add-hook 'hack-local-variables-hook #'flymake-mode-hook-function)
(setq virtualenv-workon-starts-python nil)
```
