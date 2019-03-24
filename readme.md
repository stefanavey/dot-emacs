# dot-emacs

My Emacs configuration files.  I created this repos to organize my > 2,000 line *init.el* file into a logical structure and refactor code using `use-package`. This is setup so that it can be used on a new machine with only Emacs installed and all necessary packages will be installed automatically.

## Dependencies


- [Hack font](https://github.com/source-foundry/Hack) `brew cask install caskroom/fonts/font-hack`
- [Autoconf](https://www.gnu.org/software/autoconf) `brew install autoconf`


## How to Test

You can test out this configuration without modifying your own settings by following the steps below. **NOTE:** To be safe, you should make a copy of your `~/.emacs.d` before running the command below as additional packages or different packages versions may be installed in `~/.emacs.d/elpa`.

To just test out Emacs with this config, you can run Emacs with the `-q` flag (don't load init file) and than specify `-l` (load lisp code) to load the file `spa_emacs_init.el`.

``` shell

emacs -q -l dot-emacs/spa_emacs_init.el

```

## Usage

If you want to use this configuration every time you start Emacs, simply add this 1 line load statement to `~/.emacs.d/init.el`.

``` emacs-lisp

;; ~/.emacs.d/init.el

(load "path/to/dot-emacs/spa_emacs_init.el")

```
