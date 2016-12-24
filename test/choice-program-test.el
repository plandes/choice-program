;;; package --- Summary
;;; Commentary:
;;
;; Unit tests of choice-programe.el
;;
;;; Code:

(require 'ert)
(require 'dash)

(load "test/synconf-settings.el")

(add-to-list 'exec-path "test")

(ert-deftest test-choice-program-selections ()
  "Validate mnemonic options"
 (->> synconf-the-instance
      (choice-prog-selections)
      (equal '("laptop" "usb"))
      should))

(ert-deftest test-choice-prompt ()
  "Validate prompt"
  (should (equal "Test (default def): "
		 (choice-program-default-prompt "Test" "def"))))
