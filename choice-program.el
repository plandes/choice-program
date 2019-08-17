;;; choice-program.el --- parameter based program

;; Copyright (C) 2015 - 2019 Paul Landes

;; Version: 0.9
;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: exec execution parameter option
;; URL: https://github.com/plandes/choice-program
;; Package-Requires: ((emacs "26"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Run a program in an async buffer with a particular choice, which is
;; prompted by the user.

;;; Code:

(require 'eieio)
(require 'choice-program-complete)

(defvar choice-prog-exec-debug-p nil
  "*If non-nil, output debuging to buffer *Option Prog Debug*.")

(defvar choice-prog-instance-syms nil
  "A list of choice-prog instance variables.")

(defgroup choice-prog nil
  "Parameter choice driven program execution."
  :group 'choice-prog
  :prefix "choice-prog-")

(defclass choice-prog ()
  ((name :initarg :name
	 :initform nil
	 :type (or null string)
	 :documentation "Name of the choice program launcher.")
   (program :initarg :program
	       :type string
	       :documentation "The conduit program to run.")
   (interpreter :initarg :interpreter
		:type (or null string)
		:documentation "The interpreter (i.e. /bin/sh) or nil.")
   (selection-args :initarg :selection-args
		   :type list
		   :documentation "List of arguments used to get the options.")
   (choice-prompt :initarg :choice-prompt
		  :initform "Choice"
		  :type string
		  :documentation "Name of the parameter choice list \
\(i.e. Mmenomic) when used for prompting.  This should always be capitalized.")
   (choice-switch-name :initarg :choice-switch-name
		       :initform "-o"
		       :type string
		       :documentation "Name of the parameter switch \
\(i.e. -m).")
   (dryrun-switch-name :initarg :dryrun-switch-name
		       :initform "-d"
		       :type string
		       :documentation "Name of the switch given to the \
program execute a dry run (defaults to -n).")
   (verbose-switch-form :initarg :verbose-switch-form
			:initform nil
			:type (or null string)
			:documentation "Switch and/or parameter given to the \
program to produce verbose output.")
   (buffer-name :initarg :buffer-name
		:initform nil
		:type (or symbol string)
		:documentation "The name of the buffer to generate when \
executing the synchronized command.")
   (documentation :initarg :documentation
		  :initform ""
		  :type string
		  :documentation "Documentation about this choice program.
This is used for things like what is used for the generated function
documentation.")
   (prompt-history :initarg :prompt-history
		   :protection :private
		   :initform (gensym "choice-program-prompt-history")
		   :type symbol
		   :documentation "History variable used for user prompts.")
   (display-buffer :initarg :display-buffer
		   :initform t
		   :type boolean
		   :documentation "\
Whether or not to display the buffer on execution."))
  :documentation "Represents a single `actionable' program instance.")

(cl-defmethod initialize-instance ((this choice-prog) &optional args)
  (if (null (plist-get args :buffer-name))
      (setq args
	    (plist-put args :buffer-name
		       (format "*%s Output*"
			       (capitalize (slot-value this 'program))))))
  (cl-call-next-method this args))

(cl-defmethod object-print ((this choice-prog) &optional strings)
  "Return a string as a representation of the in memory instance of THIS."
  (apply #'cl-call-next-method this
	 (format " %s (%s)"
		 (slot-value this 'program)
		 (mapconcat #'identity (slot-value this 'selection-args) " "))
	 strings))

(cl-defmethod choice-prog-name ((this choice-prog))
  "Return the name of the choice program launcher."
  (with-slots (name program) this
    (or name program)))

(cl-defmethod choice-prog-debug ((this choice-prog) object)
  (with-current-buffer
      (get-buffer-create "*Option Prog Debug*")
    (goto-char (point-max))
    (insert (format (if (stringp object) "%s" "%S") object))
    (newline)))

(cl-defmethod choice-prog-exec-prog ((this choice-prog) args &optional no-trim-p)
  (with-output-to-string
    (with-current-buffer
	standard-output
      (let ((prg (executable-find (slot-value this 'program)))
	    (inter (and (slot-value this 'interpreter)
			(executable-find (slot-value this 'interpreter)))))
	(when inter
	  (setq args (append (list prg) args))
	  (setq prg inter))
	(if choice-prog-exec-debug-p
	    (choice-prog-debug this (format "execution: %s %s"
					    (slot-value this 'program)
					    (mapconcat 'identity args " "))))
	(apply 'call-process prg nil t nil args)
	(if choice-prog-exec-debug-p
	    (choice-prog-debug this
			       (format "execution output: <%s>" (buffer-string))))
	(when (not no-trim-p)
	  (goto-char (point-max))
	  (if (and (not (bobp)) (looking-at "^$"))
	      (delete-char -1)))))))

(cl-defmethod choice-prog-selections ((this choice-prog))
  "Return a list of possibilities for mnemonics for this program."
  (let ((output (choice-prog-exec-prog this (slot-value this 'selection-args))))
    (split-string output "\n")))

(cl-defmethod choice-prog-read-option ((this choice-prog)
				       &optional default history)
  "Read one of the possible options from the list generated by the program.
DEFAULT is used as the default input for the user input.
HISTORY is the history variable used for the user input."
  (let* ((prompt-history (or history (slot-value this 'prompt-history)))
	 (default (or default (and (boundp prompt-history)
				   (car (symbol-value prompt-history))))))
    (choice-program-complete (slot-value this 'choice-prompt)
			     (choice-prog-selections this)
			     t t	    ; return-as-string require-match
			     nil	    ; initial
			     prompt-history ; history
			     default
			     nil	; allow-empty-p
			     nil	; no-initial
			     t)))       ; add-prompt-default

(cl-defmethod choice-prog-command ((this choice-prog)
				   choice &optional dryrun-p)
  (let ((cmd-lst (remove nil
			 (list
			  (and (slot-value this 'interpreter)
			       (executable-find (slot-value this 'interpreter)))
			  (and (slot-value this 'program)
			       (executable-find (slot-value this 'program)))
			  (if dryrun-p (slot-value this 'dryrun-switch-name))
			  (slot-value this 'verbose-switch-form)
			  (slot-value this 'choice-switch-name)
			  choice)))
	cmd)
    (mapconcat #'identity cmd-lst " ")))

(cl-defmethod choice-prog-exec ((this choice-prog)
				choice &optional dryrun-p)
  "Run the program with a particular choice, which is prompted by the user.
This should be called by an interactive function, or by the function created by
the `choice-prog-create-exec-function' method."
  (let ((cmd (choice-prog-command this choice dryrun-p))
	buf)
    (cl-flet ((prog-exec
	       ()
	       (compilation-start cmd t #'(lambda (mode)
					    (slot-value this 'buffer-name)))))
      (if (slot-value this 'display-buffer)
	  (setq buf (prog-exec))
	(save-window-excursion
	  (setq buf (prog-exec)))))
    (message "Started: %s" cmd)
    buf))

(defun choice-prog-instances ()
  "Return all `choice-prog' instances."
  (mapcar #'symbol-value
	  choice-prog-instance-syms))

(defun choice-prog-create-exec-function (instance-var)
  "Create functions for a `choice-prog' instance.
INSTANCE-VAR is an instance of the `choice-prog' eieio class.
NAME overrides the `:program' slot if given."
  (let* ((this (symbol-value instance-var))
	 (name (intern (choice-prog-name this)))
	 (option-doc (format "\
CHOICE is given to the `%s' program with the `%s' option.
DRYRUN-P, if non-`nil' doesn't execute the command, but instead shows what it
would do if it were to be run.  This adds the `%s' option to the command line."
			     name
			     (slot-value this 'choice-switch-name)
			     (slot-value this 'dryrun-switch-name))))
    (let ((def
	   `(defun ,name (choice dryrun-p)
	      ,(if (slot-value this 'documentation)
		   (concat (slot-value this 'documentation) "\n\n" option-doc))
	      (interactive (list (choice-prog-read-option ,instance-var)
				 current-prefix-arg))
	      (choice-prog-exec ,instance-var choice dryrun-p))))
      (eval def))
    (add-to-list 'choice-prog-instance-syms instance-var)))

(provide 'choice-program)

;;; choice-program.el ends here
