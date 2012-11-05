# fsutils

http://hackage.haskell.org/package/fsutils

fsutils is a collection of file system odds and ends that don't seem to be in
any easily accessible place. Included is a `walk` function similar to Python's
`os.walk`, `copyDir` for copying directories recursively, and `fileList` for
getting a depth-first list of files recursively in a directory, among other
things.

Also included (because I don't really want to have a separate library just for
it) is an `mtreeList` function that is similar to `tree-seq` from Clojure. A lot
of the file system functions included are written in terms of it.
