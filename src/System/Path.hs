import Control.Monad
import System.Directory
import System.FilePath ((</>))

uselessPath :: FilePath -> Bool
uselessPath path = path == "." || path == ".."

recurseDir :: FilePath -> IO [FilePath]
recurseDir path = do
  names <- liftM (filter $ not . uselessPath) $ getDirectoryContents path
  paths <- forM names $ \name -> do
    let dir = path </> name
    isDir <- doesDirectoryExist dir
    if isDir
      then recurseDir dir >>= return . ((:) dir)
      else return [dir]
  return (concat paths)
