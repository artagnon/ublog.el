# µblog.el
Ultimately extensible microblogging client written in Emacs
Lisp. Twitter implemented. Laconica support expected in future.

## Features
* OAuth authentication
* DP fetch + render (with rounded corners)
* Modular architecture: 3 stages- fetching, parsing, and rending
* Configurable keybindings in a dedicated major mode
* Gravity-like interface
* Dedicated zbuffer-mode for editing 140-character status updates

## Usage
* git clone git://github.com/artagnon/ublog.el
* Put the ublog.el directory in your Emacs loadpath
* Put (require 'ublog) in your Emacs config file
* M-x twitter-init and switch to the *timeline* buffer to see the updates
* Read the list of keybindings in the ublog.el file

## For developers
* Go through TRAC and TODO to pick up bugfixes and enhancements you can help with

