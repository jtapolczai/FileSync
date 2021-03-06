# FileSync
An utility for synchronizing directories.

Have you ever had two huge directories with maybe 50 files of difference between them that you wanted to synchronize? I sure have! Here is a program that does just that: you pass it two directories and it synchronizes them in whatever way you desire: write only from the left to the right, write from the right to the left, copy into both directions, delete all non-common files, do some custom action based on hash or modification date in the case of files present in both places, etc.

Features
========

* Written in Haskell, using the latest zygohistomorphic prepromorphism technology.
* Create a difference report between two directories (or even two sets of directories).
* Perform inner, left, right, and full outer joins.
* Use custom strategies: perform all copyings/deletions at once, or ask for confirmation at the end - or write your own!
* Replicate a master directpry onto multiple slaves.

Usage
=====

* As a library: use the functions from
  ```haskell
     System.IO.FileSync.Sync
  ```
* As a standalone CLI executable: `cabal build FileSync-exe`.
  
