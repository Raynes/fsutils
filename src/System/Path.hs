module System.Path (mtreeList,
                    fileList)
where
                     
import Control.Monad (liftM)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))

-- | Checks if a path is '.' or '..'.
uselessPath :: FilePath -> Bool
uselessPath path = path == "." || path == ".."

-- | Remove useless paths from a list of paths.
filterUseless :: [FilePath] -> [FilePath]
filterUseless = filter $ not . uselessPath

-- | Returns a list of nodes in a tree via a depth-first walk.
mtreeList :: Monad m => (a -> m [a]) -> a -> m [a]
mtreeList children root = do
  xs <- children root
  let subChildren = map (mtreeList children) xs
  joined <- sequence subChildren
  return $ root : concat joined

-- | Recursively list the contents of a directory. Depth-first.
fileList :: FilePath -> IO [FilePath]
fileList root = mtreeList children root
  where children path = do
          directory <- doesDirectoryExist path
          if directory
            then liftM filterUseless (getDirectoryContents path) >>= return . map (path </>)
            else return []
