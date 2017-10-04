(require 'choice-program)

;;; synconf
(defvar synconf-the-instance
  (choice-prog :name "synconf"
	       :program "test-synconf"
	       :interpreter "/bin/sh"
	       :buffer-name "*Synchronized Output*"
	       :choice-prompt "Mnemonic"
	       :choice-switch-name "-m"
	       :selection-args '("-a" "listmnemonics")
	       :documentation
"Run a synchronize command.  The command is issued with the `synconf'
perl script.")
  "The synconf object instance.")

;;;###autoload
(defun synconf (&optional rest) (interactive))
(choice-prog-create-exec-function 'synconf-the-instance)
