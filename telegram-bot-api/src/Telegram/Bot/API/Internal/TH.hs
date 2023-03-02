{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE CPP            #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Telegram.Bot.API.Internal.TH (makeDefault) where

import Language.Haskell.TH
import Control.Monad.State
import Data.Maybe (catMaybes)
import Control.Applicative(liftA2)

makeDefault :: Name -> Q [Dec]
makeDefault typeN = do
  info <- reify typeN
  case info of
    TyConI dec -> case dec of
      -- no sence to handle other declarations
      DataD _ _ _ _ [con] _  | Just x <- getConInfo con -> makeDefaultFromCon x
      NewtypeD _ _ _ _ con _ | Just x <- getConInfo con -> makeDefaultFromCon x
      _ -> error "declaration not supported"
    _ -> error "not a type constructor name"
  where
    defName = constructDefName typeN
    defNameP = varP defName

    makeDefaultFromCon (conN, tys) = let
      type' = constructType typeN tys
      expr = construcExpr conN tys
      -- ghc disallows quote of form [d| $name :: some type |]
      sig = pure <$> sigD defName type'

      in sig <> [d|
        $defNameP = $expr
      |]

constructDefName :: Name -> Name
constructDefName typeN = mkName ("def" <> trimReq typeStr)
  where
    typeStr = nameBase typeN

    trimReq "Request" = []
    trimReq (x:xs) = x : trimReq xs
    trimReq [] = []


construcExpr :: Name -> [Type] -> Q Exp
construcExpr conN tys = let
  mVars = flip evalState 0 $ traverse
    (\ty -> if isMaybeTy ty then pure Nothing else Just <$> newNameI)
    tys

  vars = catMaybes mVars

  argExps = map (\case
    Nothing -> conE 'Nothing
    Just na -> varE na) mVars

  in lamE (map varP vars) (foldl appE (conE conN) argExps)

constructType :: Name -> [Type] -> Q Type
constructType typeN tys = foldr arrAp baseTy (filter (not . isMaybeTy) tys)
  where
    baseTy = conT typeN
    arrAp a b = appT (appT arrowT (pure a)) b


-- Predicates over TH

getConInfo :: Con -> Maybe (Name, [Type])
getConInfo (NormalC name tys) = Just (name, map snd tys)
getConInfo (RecC name tys) = Just (name, map (\(_,_,x) -> x) tys)
getConInfo _= Nothing

isMaybeTy :: Type -> Bool
isMaybeTy (AppT (ConT m) _) = m == ''Maybe
isMaybeTy _ = False

-- State heplers

newInd :: State Int Int
newInd = do
  x <- get
  modify (+1)
  pure x

newNameI :: State Int Name
newNameI = do
  i <- newInd
  pure $ mkName ("a" <> show i)

-- Instance Monoid for TH of ghc < 8.6
#if !MIN_VERSION_template_haskell(2,17,0)

instance Semigroup a => Semigroup (Q a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Q a) where
  mempty = pure mempty

#endif
