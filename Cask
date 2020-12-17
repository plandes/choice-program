;; -*- emacs-lisp -*-
(source gnu)
(source melpa)

;; the Elisp file uses dependency 2.12.0 because `package-lint' insists
;; anything else "appears too high: try 2.12.0"
(depends-on "dash" "2.17.0")

(package-file "choice-program.el")

(files "*.el" (:exclude ".dir-locals.el"))

(development
 (depends-on "package-lint")
 (depends-on "ert-runner"))
