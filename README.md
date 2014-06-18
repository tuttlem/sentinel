# Sentinel

Prepare a `.sentinel` file in the directory that you are in and/or one in your home folder (which will act as a globally included sentinel file). Run sentinel in a terminal alongside your development editor, and it will respond to configured file changes.

Here's an example `.sentinel` file

```
*.hs:cabal build
```

This says, listen to any file that was modified matching the glob pattern `*.hs` and execute `cabal build`.

Pretty simple.

Things that I still need to do . . .

* Doesn't traverse directories recursivly
* Doesn't make duplicate actions distinct if the condition arises
* Still need to add a special variable to use a filename in a sentinel rule
