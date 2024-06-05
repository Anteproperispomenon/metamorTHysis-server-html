module Metamorth.Server.HTML.FilePath
  ( checkAndOverwrite
  ) where

import Control.Monad

import Data.ByteString.Lazy qualified as BS

import System.Directory
import System.FilePath

-- | Overwrite a file if the contents don't match.
checkAndOverwrite :: BS.ByteString -> FilePath -> IO ()
checkAndOverwrite bs fp = do
  bl1 <- doesPathExist fp
  bl2 <- doesFileExist fp
  when (bl1 && (not bl2)) $ fail $
    "Couldn't write to path \"" ++ fp ++ "\"; the path is a directory, not a file."

  if (not bl1) 
    then do
      -- Make the directory if it doesn't exist.
      let dir = takeDirectory fp
      createDirectoryIfMissing True dir
      -- Write the actual file.
      BS.writeFile fp bs
    else do
      -- Check if the file matches (probably won't...)
      -- Doesn't actually do this anymore, since it
      -- could potentially be VERY inefficient if
      -- overwriting a huge file. Maybe if there were
      -- a way to only read in the first n bytes of
      -- a file.
      -- bs' <- OF.readFile fp
      -- if (bs' == bs)
      --   then return ()
      --   else do
      BS.writeFile fp bs

          