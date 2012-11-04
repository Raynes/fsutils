module System.Path ( mtreeList
                   , fileList
                   , walkDir
                   , copyDir
                   , replaceRoot
                   , removeRoot )
where
                     
import Control.Monad (liftM, filterM, forM_, mapM_)
import System.Directory
import System.FilePath ((</>), addTrailingPathSeparator)
import Data.List ((\\))

-- | Remove useless paths from a list of paths.
filterUseless :: [FilePath] -> [FilePath]
filterUseless = (\\ [".", ".."])

-- | Returns a list of nodes in a tree via a depth-first walk.
mtreeList :: Monad m => (a -> m [a]) -> a -> m [a]
mtreeList children root = do
  xs <- children root
  let subChildren = map (mtreeList children) xs
  joined <- sequence subChildren
  return $ root : concat joined

topFileList :: FilePath -> IO [FilePath]
topFileList path = liftM filterUseless (getDirectoryContents path) >>= return . map (path </>)

-- | Recursively list the contents of a directory. Depth-first.
fileList :: FilePath -> IO [FilePath]
fileList root = mtreeList children root
  where children path = do
          directory <- doesDirectoryExist path
          if directory
            then topFileList path
            else return []

-- | We can use this data type to represent the pieces of a directory.
data Directory = Directory
                 { -- | The path of the directory itself.
                   dirPath :: FilePath
                   -- | All subdirectories of this directory.
                 , subDirs :: [FilePath]
                   -- | All files contained in this directory.
                 , files   :: [FilePath]
                 }
               deriving (Show)

-- | Creates a Directory instance from a FilePath.
createDir :: FilePath -> IO Directory
createDir path = do
  contents <- topFileList path
  subdirs  <- filterM doesDirectoryExist contents
  files    <- filterM doesFileExist contents
  return (Directory path subdirs files)
  
-- | Walk a directory depth-first. Similar to Python's os.walk and fs.core/walk
-- from the fs Clojure library.
walkDir :: FilePath -> IO [Directory]
walkDir root = createDir root >>= mtreeList children
  where children path = do
          let dirs = subDirs path
          mapM createDir dirs

-- | Given a root (prefix), remove it from a path. This is useful
-- for getting the filename and subdirs of a path inside of a root.
removeRoot :: FilePath -> FilePath -> FilePath
removeRoot prefix path = drop (length $ addTrailingPathSeparator prefix) path

-- | Given a root path, a new root path, and a path to be changed,
-- removes the old root from the path and replaces it with to.
replaceRoot :: FilePath -> FilePath -> FilePath -> FilePath
replaceRoot root to path = to </> removeRoot root path

-- | Copy a directory recursively. Moves every file, creates every directory.
copyDir :: FilePath -> FilePath -> IO ()
copyDir from to = do
  createDirectoryIfMissing True to
  walked <- walkDir from
  forM_ walked $ \(Directory _ dirs files) -> do
    mapM_ (createDirectoryIfMissing True . (replaceRoot from to)) dirs
    mapM_ (\path -> copyFile path (replaceRoot from to path)) files 