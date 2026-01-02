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
  (choice-program :object-name "synconf"
		  :program "test-synconf"
		  :interpreter "/bin/sh"
		  :buffer-name "*Synchronized Output*"
		  :choice-prompt "Mnemonic"
		  :choice-switch-name ""
		  :selection-args '("-a" "listmnemonics")
		  :documentation
"Run a synchronize command.  The command is issued with the `synconf'
perl script.")
  "The synconf object instance.")

(defun choice-program-test-setup ()
  "Initialize the test."
  (choice-program-create-exec-function 'synconf-the-instance)
  (let ((dir (concat (file-name-as-directory
		      (expand-file-name default-directory))
		     "test")))
    (add-to-list 'exec-path dir)))

(ert-deftest choice-program-test-selections ()
  "Validate mnemonic options."
  (choice-program-test-setup)
  (->> synconf-the-instance
       (choice-program-selections)
       (equal '("laptop" "usb"))
       should))

(ert-deftest choice-program-test-prompt ()
  "Validate prompt."
  (choice-program-test-setup)
  (should (equal "Test (default def): "
		 (choice-program-complete-default-prompt "Test" "def"))))

(ert-deftest choice-program-invoke ()
  "Validate prompt."
  (choice-program-test-setup)
  (let ((this synconf-the-instance))
    (choice-program-exec this "-a listmnemonics")
    (sit-for 0.2)
    (let ((output (with-current-buffer
		      (get-buffer (slot-value this 'buffer-name))
		    (buffer-substring-no-properties (point-min) (point-max)))))
      (should (string-search "laptop" output))
      (should (string-search "usb" output)))))

(provide 'choice-program-test)

;;; choice-program-test.el ends here
