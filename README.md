# Invoke Programs With Choice Options

[![MELPA badge][melpa-badge]][melpa-link]
[![MELPA stable badge][melpa-stable-badge]][melpa-stable-link]
[![Travis CI Build Status][travis-badge]][travis-link]

This is an Emacs module that invokes a command line program that requires an a
string of enumerations as input.  An example is the mnemonic option of the
[synconf] program.  The enumeration of choices offered by the command line
program are then read as a completing user input in Emacs.


## Usage

You must create an instance of the `choice-program` class and specify the program
with options.  For the [synconf] program
you'd add the following to your `~/.emacs` init file:
```elisp
(defvar synconf-the-instance
  (choice-program nil
                  :program "synconf"
                  :interpreter "perl"
                  :buffer-name "*Synchronized Output*"
                  :choice-prompt "Mnemonic"
                  :choice-switch-name "-m"
                  :selection-args '("-a" "listmnemonics")
                  :documentation
"Run a synchronize command.  The command is issued with the `synconf'
perl script.")
  "The synconf object instance.")

(choice-program-create-exec-function 'synconf-the-instance)
```

If you'd like add the configuration in another config file you can use add the
following:
```elisp
;;;###autoload
(defun synconf (&optional rest) (interactive))
(choice-program-create-exec-function 'synconf-the-instance)
```

This adds the configuration file to autoloads (pattern matching on the
interactive function `synconf`).  Then the subsequent call to
`choice-program-create-exec-function` clobbers the empty interactive definition
with that which invokes the command line program.


## Changelog

An extensive changelog is available [here](CHANGELOG.md).


## License

Copyright © 2017 Paul Landes

GNU Lesser General Public License, Version 2.0


<!-- links -->

[melpa-link]: https://melpa.org/#/choice-program
[melpa-stable-link]: https://stable.melpa.org/#/choice-program
[melpa-badge]: https://melpa.org/packages/choice-program-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/choice-program-badge.svg
[travis-link]: https://travis-ci.org/plandes/choice-program
[travis-badge]: https://travis-ci.org/plandes/choice-program.svg?branch=master
[synconf]: https://github.com/plandes/synconf
