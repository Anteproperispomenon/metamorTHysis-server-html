module Metamorth.Server.HTML.Extra
  ( invertOrthMap
  ) where

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

-- | Invert a map. From metamorTHysis-cli
invertOrthMap :: (Ord inOrth, Ord txt) => M.Map txt inOrth -> M.Map inOrth (S.Set txt)
invertOrthMap = M.foldlWithKey (\mp' k val -> insertWithElse S.insert S.singleton val k mp') M.empty
-- invertOrthMap inMap = M.foldlWithKey (\mp' k val -> insertWithElse S.insert S.singleton val k mp') M.empty inMap

-- | Like `insertWith`, but the type of the value to be
--   inserted doesn't have to be the same as value already
--   inserted. Also taken from metamorTHysis-cli.
insertWithElse :: (Ord k) => (w -> v -> v) -> (w -> v) -> k -> w -> M.Map k v -> M.Map k v
insertWithElse op f k val
  = M.alter (\case {Nothing -> Just $ f val ; (Just y) -> Just $ op val y}) k
