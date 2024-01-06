#lang racket/base

#|
This file: gnucash-racket-gui.rkt

Purpose:   Define the GUI to access GnuCash data from a racket app.

Run:       Run this file to execute (main)

Main runs in two phases:

1) Configure: read settings and load data; display GUI window with error message(s)
              in case of failure.

2) Display views: load the main GUI; uses menus/buttons to load other views.
|#



