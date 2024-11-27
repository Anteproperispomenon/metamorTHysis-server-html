module Metamorth.Server.HTML.Extra
  ( invertOrthMap
  , invertOrthMapAlt
  , invertOrthMapNew
  , mapMaybeFst
  ) where

import Control.Arrow (first)

import Data.Maybe (fromMaybe)
import Data.String (IsString)

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


-- -> M.Map String (oorth, String)

invertOrthMapNew :: forall outOrth txt. (Ord outOrth, Ord txt, IsString txt) => M.Map txt txt -> M.Map txt outOrth -> M.Map outOrth (S.Set txt, txt)
invertOrthMapNew dscMap = M.foldlWithKey (\mp' k val -> insertWithElse insertFst makeFirst val (k) mp') M.empty
  where
    insertFst :: txt -> (S.Set txt,txt) -> (S.Set txt,txt)
    insertFst x (st, dsc) 
      | dsc == "" = (S.insert x st, fromMaybe "" (M.lookup x dscMap))
      | otherwise = (S.insert x st, dsc)

    makeFirst :: txt -> (S.Set txt, txt)
    makeFirst x = (S.singleton x, dsc)
      where dsc = fromMaybe "" (M.lookup x dscMap)

invertOrthMapAlt :: (Ord outOrth, Ord txt) => M.Map txt (outOrth, String) -> M.Map outOrth (S.Set txt, String)
invertOrthMapAlt = M.foldlWithKey (\mp' k (val, dsc) -> insertWithElse insertFst makeFirst val (k, dsc) mp') M.empty
  where
    insertFst :: (Ord a{-, Semigroup b-}) => (a,b) -> (S.Set a,b) -> (S.Set a,b)
    insertFst (x,_z) (st, dsc) = (S.insert x st, dsc)

    makeFirst :: (a,b) -> (S.Set a, b)
    makeFirst (x,y) = (S.singleton x, y)

-- mapMaybe :: (a -> Maybe b) -> Map k a -> Map k b

mapMaybeFst :: (a -> Maybe b) -> M.Map k (a,c) -> M.Map k (b,c)
mapMaybeFst f = M.mapMaybe 
  (\(x,y) -> case f x of
     Nothing  -> Nothing
     (Just z) -> Just (z,y)
  )


