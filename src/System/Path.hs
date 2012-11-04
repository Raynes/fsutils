module System.Path ( mtreeList
                   , fileList )
where
                     
import Control.Monad (liftM, filterM)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
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
