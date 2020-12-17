# Change Log
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).


## [Unreleased]


## [0.13] - 2020-12-16
### Change
- Build refactoring only: better package-lint automation.


## [0.12] - 2020-12-16

### Changed
- Documentation: *package-lint* warning fixes.
- Rename `choice-program-complete-default-prompt` to fix *package-lint*
  warning.


## [0.11] - 2020-12-16
This release addresses the following error:
```
`object-print' is an obsolete generic function (as of 26.1); use `cl-print-object' instead.
```

### Changed
- Remove slot `:name` from `choice-program` and extend from `eieio-named`,
  which is now the (changed) convention.
- Replace `object-print` with `eieio-named` class's method
  `eieio-object-name-string`.

### Removed
- Removed methods `object-print` and `object-format`.



## [0.10] - 2020-12-14
### Changed
- Rename `choice-prog` class to `choice-program` to make compat with
  `package-lint`.
- Fixed tests and cleaned up test code.
- Switched from Travis GitHub CI actions.


## [0.9] - 2019-07-21
### Changed
- Sync versions.

### Added
- Added [zenbuild].


## [0.5] - 2019-06-15
### Added
- Feature to prevent program output from popping up.

### Changed
- Eieio updates for Emacs 26.


## [0.4] - 2017-10-04
### Changed
- Name of the program instance is given in the class constructor instead of
  `choice-prog-create-exec-function`.
  
### Added
- Get all registered program instances with `choice-prog-instances`.


## [0.3] - 2017-09-15
### Added
- Changelog

### Changed
- Dependencies and headers to be adhere more to convention.
- Fixed travis build using old (24.3) Emacs version from cask.


[Unreleased]: https://github.com/plandes/choice-program/compare/v0.13...HEAD
[0.13]: https://github.com/plandes/choice-program/compare/v0.12...v0.13
[0.12]: https://github.com/plandes/choice-program/compare/v0.11...v0.12
[0.11]: https://github.com/plandes/choice-program/compare/v0.10...v0.11
[0.10]: https://github.com/plandes/choice-program/compare/v0.9...v0.10
[0.9]: https://github.com/plandes/choice-program/compare/v0.5...v0.9
[0.5]: https://github.com/plandes/choice-program/compare/v0.4...v0.5
[0.4]: https://github.com/plandes/choice-program/compare/v0.3...v0.4
[0.3]: https://github.com/plandes/choice-program/compare/v0.2...v0.3

<!-- links -->
[zenbuild]: https://github.com/plandes/zenbuild
