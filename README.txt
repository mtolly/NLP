Michael Tolly
CS 545 (Benjamin Snyder) Homework 2

I used the language Haskell for the programs, with gnuplot to produce the plot
images. All the numerical data is in the ANSWERS.txt file, and all the plots are
PNG images. Please ask if any more explanation of the code is needed.

The three files ZipfLaw.hs, NGrams.hs, and SentenceLength.hs are used for
questions 1, 2, and 3 respectively.

If you want to run the programs yourself: install the Haskell Platform, and
these packages: gnuplot, multiset, random, gamma. (Command
"cabal install gnuplot" installs a package.) Also make sure gnuplot is in your
path. Then (for example) run "ghci -i NGrams.hs" in the dir with the programs,
and then type a function name like "printEntropies". The functions to run are
given at the top of each file.
