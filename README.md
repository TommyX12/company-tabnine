# company-tabnine

*TODO badge*

[TabNine](https://tabnine.com/) is the all-language autocompleter. It uses machine learning to provide responsive, reliable, and relevant suggestions.

`company-tabnine` provides TabNine completion backend for [company-mode](https://github.com/company-mode/company-mode). **It takes care of TabNine binaries**, so installation is easy.

![screenshot](screenshot.png)

## Installation

1. Make sure [company-mode](https://github.com/company-mode/company-mode) is installed and configured.

2. Install `company-tabnine`
   - With `use-package`

	 Put the following in your config:

	 ```emacs
	 (use-package company-tabnine :ensure t)
	 ```

   - Without `use-package`

	 Clone the repository, add to `load-path` in your config, and require the package:

	 ```emacs
	 (add-to-list 'load-path "path/to/repository")
	 (require 'company-tabnine)
	 ```

3. Add `company-tabnine` to `company-backends`
   ```emacs
   (add-to-list 'company-backends #'company-tabnine)
   ```

## Usage

`company-tabnine` should work out of the box.

See `M-x customize-group RET company-tabnine RET` for customizations.
