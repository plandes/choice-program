;;; package --- Summary
;;; Commentary:
;;
;; Unit tests of choice-program.el
;;
;;; Code:

(require 'ert)
(require 'dash)
(require 'choice-program)

;;;###autoload
(defun synconf (&optional rest)
  "A no-op function that disards REST."
  (interactive))

;;; synconf
(defvar synconf-the-instance
  (choice-program :name "synconf"
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

(choice-program-create-exec-function
 'synconf-the-instance)

(let ((dir (concat (file-name-as-directory
		    (expand-file-name default-directory)) "test")))
  (add-to-list 'exec-path dir))

(ert-deftest test-choice-program-selections ()
  "Validate mnemonic options"
 (->> synconf-the-instance
      (choice-program-selections)
      (equal '("laptop" "usb"))
      should))

(ert-deftest test-choice-prompt ()
  "Validate prompt"
  (should (equal "Test (default def): "
		 (choice-program-default-prompt "Test" "def"))))

(provide 'choice-program-test)

;;; choice-program-test.el ends here
