;;; choice-program.el --- Parameter based program  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 - 2026 Paul Landes

;; Version: 0.16.0
;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: execution processes unix lisp
;; URL: https://github.com/plandes/choice-program
;; Package-Requires: ((emacs "27.1") (dash "2.19.1"))

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

(require 'dash)
(require 'eieio)
(require 'eieio-base)
(require 'choice-program-complete)

(defvar choice-program-exec-debug-p nil
  "*If non-nil, output debuging to buffer *Option Prog Debug*.")

(defvar choice-program-instance-syms nil
  "A list of choice-program instance variables.")

(defgroup choice-program nil
  "Parameter choice driven program execution."
  :group 'choice-program
  :prefix "choice-program-")

(defclass choice-program (eieio-named)
  ((program :initarg :program
	    :type string
	    :documentation "The conduit program to run.")
   (interpreter :initarg :interpreter
		:initform nil
		:type (or null string)
		:documentation "The interpreter (i.e. /bin/sh) or nil.")
   (selection-args :initarg :selection-args
		   :initform nil
		   :type (or null string list)
		   :documentation "List of arguments used to get the options.")
   (choice-prompt :initarg :choice-prompt
		  :initform "Choice"
		  :type string
		  :documentation "Name of the parameter choice list \
\(i.e. Mmenomic) when used for prompting.  This should always be capitalized.")
   (choice-switch-name :initarg :choice-switch-name
		       :initform nil
		       :type (or null string)
		       :documentation "Name of the parameter switch \
(i.e. -m).")
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
   (display-buffer-p :initarg :display-buffer-p
		     :initform t
		     :type boolean
		     :documentation "\
Whether or not to display the buffer on execution.")
   (kill-buffer-p :initarg :kill-buffer-p
		  :initform nil
		  :type boolean
		  :documentation "Kill the buffer on success.")
   (restore-windows-p :initarg :restore-windows-p
		      :initform nil
		      :type boolean
		      :documentation "Restore window positions on success."))
  :documentation
  "Represents a single `actionable' program instance.

Overridable methods:
- `choice-program--start': call backs for starting the program
- `choice-program--finish': call backs for ending the program")

(cl-defmethod initialize-instance ((this choice-program) &optional slots)
  "Initialize instance THIS with arguments SLOTS."
  (setq slots (plist-put slots :buffer-name
			 (or (plist-get slots :buffer-name)
			     (format "*%s Output*"
				     (capitalize (slot-value this 'program)))))
	slots (plist-put slots :object-name
			 (or (plist-get slots :object-name)
			     (plist-get slots :program))))
  (cl-call-next-method this slots))

(cl-defmethod choice-program-name ((this choice-program))
  "Return the name of THIS choice program launcher."
  (slot-value this 'object-name))

(cl-defmethod choice-program-debug ((_ choice-program) object)
  "Add a debugging message with debug parameter OBJECT."
  (with-current-buffer
      (get-buffer-create "*Option Prog Debug*")
    (goto-char (point-max))
    (insert (format (if (stringp object) "%s" "%S") object))
    (newline)))

(cl-defmethod eieio-object-value-string ((this choice-program) val)
  "Return a string representation of VAL overriden for THIS.
This is used for prettyprinting by `eieio-object-name-string'."
  (cond ((stringp val) val)
	((consp val) (->> (mapconcat #'(lambda (val)
					 (eieio-object-value-string this val))
				     val " ")
			  (format "(%s)")))
	(t (prin1-to-string val))))

(cl-defmethod eieio-object-value-slots ((_this choice-program))
  "Return a list of slot names used in `eieio-object-name-string'."
  '(selection-args buffer-name))

(cl-defmethod eieio-object-name-string ((this choice-program))
  "Return a string as a representation of the in memory instance of THIS."
  (->> (mapconcat #'(lambda (slot)
		      (let ((val (if (slot-boundp this slot)
				     (slot-value this slot)
				   (format "Unbound slot: %S" slot))))
			(eieio-object-value-string this val)))
		  (eieio-object-value-slots this)
		  " ")
       (concat (cl-call-next-method this) " ")))

(cl-defmethod choice-program-exec-prog ((this choice-program) args
					&optional no-trim-p)
  "Execute the program to get the action mnemonics (a.k.a. choices) for THIS.

ARGS the arguments sent to the program for execution.
NO-TRIM-P, if non-nil, don't remove the terminating from the program's output."
  (with-output-to-string
    (with-current-buffer
	standard-output
      (let* ((exec-name (slot-value this 'program))
	     (prg (or (executable-find exec-name)
		      (error "No such executable found: %s" exec-name)))
	     (inter (and (slot-value this 'interpreter)
			 (executable-find (slot-value this 'interpreter)))))
	(when inter
	  (setq args (append (list prg) args))
	  (setq prg inter))
	(if choice-program-exec-debug-p
	    (choice-program-debug this (format "execution: %s %s"
					       (slot-value this 'program)
					       (mapconcat 'identity args " "))))
	(apply 'call-process prg nil t nil args)
	(if choice-program-exec-debug-p
	    (choice-program-debug this
				  (format "execution output: <%s>" (buffer-string))))
	(when (not no-trim-p)
	  (goto-char (point-max))
	  (if (and (not (bobp)) (looking-at "^$"))
	      (delete-char -1)))))))

(cl-defmethod choice-program-selections ((this choice-program))
  "Return a list of possibilities for mnemonics for this program.
THIS is the instance"
  (let ((output (->> (slot-value this 'selection-args)
		     (choice-program-exec-prog this))))
    (split-string output "\n")))

(cl-defmethod choice-program-read-option ((this choice-program)
					  &optional default history choices)
  "Read one of the possible options from the list generated by the program.
THIS is the instance.
DEFAULT is used as the default input for the user input.
HISTORY is the history variable used for the user input.
CHOICES is the list of choices in place of getting it from the program."
  (let* ((prompt-history (or history (slot-value this 'prompt-history)))
	 (default (or default (and (boundp prompt-history)
				   (car (symbol-value prompt-history))))))
    (setq choices (or choices (choice-program-selections this)))
    (choice-program-complete (slot-value this 'choice-prompt)
			     choices
			     t t	    ; return-as-string require-match
			     nil	    ; initial
			     prompt-history ; history
			     default
			     nil	; allow-empty-p
			     nil	; no-initial
			     t)))       ; add-prompt-default

(cl-defmethod choice-program-command ((this choice-program)
				      choice &optional dryrun-p)
  "Create the command line as a list of arguments for THIS innstance.

CHOICE is the mnemonic choice, usually called the `action'.
DRYRUN-P logs like its doing something, but doesn't."
  (when (stringp choice)
    (setq choice (string-split choice)))
  (with-slots (interpreter program verbose-switch-form choice-switch-name) this
    (->> (list (and interpreter (executable-find interpreter))
	       (and program (or (executable-find program) program))
	       (if dryrun-p (slot-value this 'dryrun-switch-name))
	       verbose-switch-form
	       choice-switch-name)
	 (funcall (lambda (args)
		    (append args choice)))
	 (remove nil))))

(cl-defmethod choice-program--start ((_this choice-program) args)
  "Called when THIS begins the program with argument ARGS."
  (insert (format "$ %s\n\n" (mapconcat #'identity args " "))))

(cl-defmethod choice-program--finish ((_this choice-program) args proc)
  "Called when THIS has completed called with ARGS with ending process PROC."
  (let ((exit (process-exit-status proc)))
    (message (format "%s \"%s\""
		     (if (= exit 0)
			 "Success"
		       (format "Fail (%d) exit" exit))
		     (mapconcat #'identity args " ")))))

(cl-defmethod choice-program-exec ((this choice-program)
				   &optional choice dryrun-p)
  "Run the program with a choice on THIS prompted by the user.
This should be called by an interactive function, or by the function created by
the `choice-program-create-exec-function' method.

CHOICE is the choice to run the program, or the choice + arguments if a list.
DRYRUN-P logs like its doing something, but doesn't."
  (let ((args (choice-program-command this choice dryrun-p)))
    (unless (and (listp args) (stringp (car args)))
      (error "ARGS must be a list whose first element is the program string"))
    (with-slots (buffer-name display-buffer-p kill-buffer-p restore-windows-p)
	this
      (let ((buf (get-buffer-create buffer-name))
	    (winconf (current-window-configuration)))
	(with-current-buffer buf
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (choice-program--start this args)
	  ;(setq-local cursor-type nil)
	  (local-set-key (kbd "q") #'quit-window))
	(when display-buffer-p
	  (display-buffer buf))
	(let ((proc
               (make-process
		:name buffer-name
		:buffer buf
		:command args
		:noquery t
		:filter
		(lambda (p chunk)
		  (when (buffer-live-p (process-buffer p))
		    (with-current-buffer (process-buffer p)
		      (let ((inhibit-read-only t))
			(goto-char (point-max))
			(insert chunk)))))
		:sentinel
		(lambda (p _event)
		  (when (memq (process-status p) '(exit signal))
		    (choice-program--finish this args p)
		    (let ((b (process-buffer p)))
		      (if (and (eq (process-status p) 'exit)
				 (zerop (process-exit-status p)))
			;; success: restore windows and remove buffer
			(progn
			  (when (and restore-windows-p
				     (window-configuration-p winconf))
			    (set-window-configuration winconf))
			  (when (buffer-live-p b)
			    (if kill-buffer-p
				(kill-buffer b)
			      (with-current-buffer b
				(read-only-mode 1)))))
			(unless display-buffer-p
			  (display-buffer buf)))))))))
	  proc)))))

(cl-defmethod choice-program-exec-string ((this choice-program)
					  &optional choice dryrun-p)
  "Run THIS program with action CHOICE and return the output as a string.

This is meant to be used programmatically.
DRYRUN-P logs like its doing something, but doesn't."
  (->> (choice-program-command this choice dryrun-p)
       (funcall (lambda (arg)
		  (mapconcat #'identity arg " ")))
       shell-command-to-string))

(defun choice-program-instances ()
  "Return all `choice-program' instances."
  (mapcar #'symbol-value
	  choice-program-instance-syms))

(defun choice-program-create-exec-function (instance-var)
  "Create functions for a `choice-program' instance.

INSTANCE-VAR is an instance of the `choice-program' eieio class.
NAME overrides the `:program' slot if given."
  (let* ((this (symbol-value instance-var))
	 (name (intern (choice-program-name this)))
	 (option-doc (format "\
CHOICE is given to the `%s' program with the `%s' option.
DRYRUN-P, if non-`nil' doesn't execute the command, but instead shows what it
would do if it were to be run.  This adds the `%s' option to the command line."
			     name
			     (slot-value this 'choice-switch-name)
			     (slot-value this 'dryrun-switch-name))))
    (let ((def
	   `(defun ,name (&optional choice dryrun-p)
	      ,(if (slot-value this 'documentation)
		   (concat (slot-value this 'documentation) "\n\n" option-doc))
	      (interactive (list (choice-program-read-option ,instance-var)
				 current-prefix-arg))
	      (choice-program-exec ,instance-var choice dryrun-p))))
      (eval def))
    (add-to-list 'choice-program-instance-syms instance-var)))

(provide 'choice-program)

;;; choice-program.el ends here
