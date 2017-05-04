# .emacs.d

#### Installation 
Remove or backup existing `.emacs.d/` then `git clone` to your homedir. 

### Good for beginners, pretty vanilla with some added features for sanity:

* Lots of comments in `init.el`.
* Light, boots fast.
* MELPA package repository configured.
* Has fuzzy search for both files and `M-x` commands. ( Use`C-s` and `C-r` to browse suggestions.)
* Can edit protected files with `M-x sudo-edit`.
* Won't clutter working directory with temp files.
* Prompts the user less, only when necessary.
* File viewer (Dired) hides dotfiles by default (`C-c h` to toggle.)
* Pressing backspace while searching works like one would expect.
* Works well in terminal. (`$ emacs -nw`)
* Environment variables are imported to `eshell`. (`$PATH` for example.)

Installed theme is [Tomorrow Night](https://github.com/chriskempson/tomorrow-theme) by Chris Kempson.

## TODO
* `use-package` maybeeee
