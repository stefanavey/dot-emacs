# dot-emacs

My Emacs configuration files.  I created this repos to organize my > 2,000 line *init.el* file into a logical structure and refactor code using `use-package`. This is setup so that it can be used on a new machine with only Emacs installed and all necessary packages will be installed automatically.

## How to Test

You can test out this configuration without modifying your own init file by using the command below.

To just test out Emacs with this config, you can run Emacs with the `-q` flag and than specify `-l` to load the file `spa_emacs_init.el`.

``` shell

emacs -q -l dot-emacs/spa_emacs_init.el

```

## Usage

If you want to use this configuration every time you start Emacs, simply add this 1 line load statement to `~/.emacs.d/init.el`.

``` emacs-lisp

;; ~/.emacs.d/init.el

(load "path/to/dot-emacs/spa_emacs_init.el")

```
