{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
module Language.Haskell.Exts.Prisms where

import Language.Haskell.Exts.Syntax

import Data.List (foldl')

import Control.Monad
import Control.Applicative

import Control.Lens (makeWrapped, Prism', _Unwrapped)
import Control.Lens.TH

import qualified Language.Haskell.TH as TH

import Language.Haskell.Exts.TypeList

concat <$> mapM makePrisms types

$(concat <$>
  forM types (\name -> do
    TH.TyConI (TH.DataD _ _ binders Nothing constructors deriv) <- TH.reify name
    if length constructors >= 2 then
      forM constructors (\(TH.NormalC name tys) -> do
        let name' = TH.mkName $ "C_" ++ TH.nameBase name
        let ty = foldl' TH.AppT (TH.TupleT $ length tys) $ map snd tys
        let unbanged = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness
        return $ TH.NewtypeD [] name' binders Nothing (TH.NormalC name' [(unbanged, ty)]) deriv)
     else
      return []))

$(concat <$>
  forM types (\name -> do
    TH.TyConI (TH.DataD _ _ binders Nothing constructors deriv) <- TH.reify name
    if length constructors >= 2 then
      concat <$>
        forM constructors (\(TH.NormalC name tys) -> do
          Just name' <- TH.lookupTypeName $ "C_" ++ TH.nameBase name
          makeWrapped name')
    else
      return []))

$(concat <$>
  forM types (\tName -> do
    TH.TyConI (TH.DataD _ _ binders Nothing constructors deriv) <- TH.reify tName
    if length constructors >= 2 then
      concat <$>
        forM constructors (\(TH.NormalC cName tys) -> do
          Just ty <- TH.lookupTypeName $ "C_" ++ TH.nameBase cName
          Just prismTy <- TH.lookupTypeName "Prism'"
          Just compose <- TH.lookupValueName "."
          Just unwrapped <- TH.lookupValueName "_Unwrapped"
          Just prism <- TH.lookupValueName $ "_" ++ TH.nameBase cName

          let name' = TH.mkName $ "_" ++ TH.nameBase cName ++ "'"
          vars <- replicateM (length binders) $ TH.newName "v"
          let f x = foldl' TH.AppT x $ map TH.VarT vars
          let baseTy = f $ TH.ConT tName
          let cTy = f $ TH.ConT ty
          let binding = TH.SigD name' $ TH.ConT prismTy `TH.AppT` baseTy `TH.AppT` cTy
          let exp = TH.ParensE (TH.VarE compose) `TH.AppE` TH.VarE prism `TH.AppE` TH.VarE unwrapped
          let v = TH.ValD (TH.VarP name') (TH.NormalB exp) []
          return [binding, v])
    else
      return []))

type family RecordOf (a :: *) :: [*]

$(concat <$>
  forM types (\tName -> do
    TH.TyConI (TH.DataD _ _ binders Nothing constructors deriv) <- TH.reify tName
    if length constructors >= 2 then
      concat <$>
        forM constructors (\(TH.NormalC cName tys) -> do
          Just ty <- TH.lookupTypeName $ "C_" ++ TH.nameBase cName
          Just recordOf <- TH.lookupTypeName "RecordOf"
          let getName x =
                case x of
                  TH.PlainTV n -> n
                  TH.KindedTV n _ -> n
          let vars = map getName binders
          let f x = foldl' TH.AppT x $ map TH.VarT vars
          let lhs = f $ TH.ConT ty
          let list = foldr (\x y -> TH.AppT (TH.AppT TH.PromotedConsT x) y) TH.PromotedNilT $ map snd tys
          let decl = TH.TySynInstD recordOf $ TH.TySynEqn [lhs] list
          return [decl])
    else
      return []))
