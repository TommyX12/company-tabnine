# company-tabnine

[![MELPA](https://melpa.org/packages/company-tabnine-badge.svg)](https://melpa.org/#/company-tabnine)

[TabNine](https://tabnine.com/) is the all-language autocompleter. It uses machine learning to provide responsive, reliable, and relevant suggestions.

`company-tabnine` provides TabNine completion backend for [company-mode](https://github.com/company-mode/company-mode). **It takes care of TabNine binaries**, so installation is easy.

![screenshot](screenshot.png)

## Installation

1. Make sure [company-mode](https://github.com/company-mode/company-mode) is installed and configured.

2. Install `company-tabnine`. This package is part of [MELPA](https://melpa.org).

   - With [use-package](https://github.com/jwiegley/use-package)

	 Put the following in your config:

	 ```emacs
	 (use-package company-tabnine :ensure t)
	 ```
	 
   - With `package.el` (built-in)
   
	 Install the package:
	 ```emacs
	 M-x package-install RET company-tabnine RET
	 ```

	 Put the following in your config:
	 ```emacs
	 (require 'company-tabnine)
	 ```

3. Add `company-tabnine` to `company-backends`
   ```emacs
   (add-to-list 'company-backends #'company-tabnine)
   ```

4. Run `M-x company-tabnine-install-binary` to install the TabNine binary for your system.

## Usage

`company-tabnine` should work out of the box.

See `M-x customize-group RET company-tabnine RET` for customizations.

## Recommended Configuration

Below are some recommended `company-mode` configuration that works well with `company-tabnine`.

```emacs
;; Trigger completion immediately.
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)

;; Use the tab-and-go frontend.
;; Allows TAB to select and complete at the same time.
(company-tng-configure-default)
(setq company-frontends
      '(company-tng-frontend
        company-pseudo-tooltip-frontend
        company-echo-metadata-frontend))
```
