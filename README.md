# Sentinel

Prepare a `.sentinel` file in the directory that you are in and/or one in your home folder (which will act as a globally included sentinel file). Run sentinel in a terminal alongside your development editor, and it will respond to configured file changes.

Here's an example `.sentinel` file

```
*.hs:cabal build
*.pl:perl -c {fn}
```

This says, listen to any file that was modified matching the glob pattern `*.hs` and execute `cabal build`.

It also says that when a `*.pl` file changes, it'll run a syntax check or a `perl -c` on the file that changed. The variable `{fn}` allows sentinel to directly target that one file.

Pretty simple.

Things that I still need to do . . .

* hinotify doesn't traverse directories recursively. Need to build this tree at startup and work out how to manage directory structure changes at runtime.
* Doesn't make duplicate actions distinct if the condition arises.

