{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeOperators     #-}
module Data.Outputable.Class (Outputable(..), GOutputable(..)) where

import           Data.Char
import           GHC.Generics
import           Text.PrettyPrint hiding ((<>))

wrapParens :: Bool -> Doc -> Doc
wrapParens False s = s
wrapParens True x  = lparen <> x <> rparen

class Outputable a where
  pprPrec :: Int -> a -> Doc
  default pprPrec :: (Generic a, GOutputable (Rep a)) => Int -> a -> Doc
  pprPrec n x = gpprPrec Pref False n (from x)

  ppr :: a -> Doc
  ppr = pprPrec 0

  pprList :: [a] -> Doc
  pprList =
    brackets . sep . punctuate comma . map ppr

data Type = Rec | Pref | Inf String
  deriving Eq

class GOutputable (f :: * -> *) where
  gpprPrec :: Type -> Bool -> Int -> f x -> Doc

  gppr :: f x -> Doc
  gppr = gpprPrec Pref False 0

  gpprList :: [f x] -> Doc
  gpprList = brackets . sep . punctuate comma . map gppr

  isNullary :: f x -> Bool

instance GOutputable V1 where
  gpprPrec _ _ _ _ = mempty
  isNullary = error "generic outputable (isNullary): unnecessary case"

instance GOutputable U1 where
  gpprPrec _ _ _ _ = mempty
  isNullary _ = True

instance (GOutputable f, Datatype c) => GOutputable (M1 D c f) where
  gpprPrec t p d (M1 a) = gpprPrec t p d a
  isNullary (M1 a) = isNullary a

instance (GOutputable f, Selector c) => GOutputable (M1 S c f) where
  gpprPrec t p d s@(M1 a)
    | selector == "" = gpprPrec t p d a
    | otherwise = fsep [text selector <+> char '=', nest 2 $ gpprPrec t p 0 a]
    where selector = selName s
  isNullary (M1 a) = isNullary a

instance (GOutputable f, Constructor c) => GOutputable (M1 C c f) where
  gpprPrec _ _ d c@(M1 a) =
    case fixity of
      Prefix -> wrapParens boolParens $ text name <+>
                if t == Rec
                then nest 1 $ braces $ nest 2 $ gpprPrec t boolParens 11 a
                else nest 2 $ gpprPrec t boolParens 11 a
      Infix _ m -> wrapParens (d > m) $ gpprPrec t (d > m) (m + 1) a
    where fixity = conFixity c
          boolParens = d > 10 && not (isNullary a)
          t | conIsRecord c = Rec
            | otherwise = case fixity of
                            Prefix    -> Pref
                            Infix _ _ -> Inf (conName c)
          name = checkInfix $ conName c
          checkInfix [] = []
          checkInfix (x:xs)
            | fixity == Prefix && (isAlphaNum x || x == '_') = x:xs
            | otherwise = "(" ++ (x:xs) ++ ")"
  isNullary (M1 a) = isNullary a

instance (Outputable f) => GOutputable (K1 t f) where
  gpprPrec _ _ d (K1 a) = pprPrec d a
  isNullary _ = False

instance (GOutputable f, GOutputable g) => GOutputable (f :+: g) where
  gpprPrec t p d (L1 a) = gpprPrec t p d a
  gpprPrec t p d (R1 a) = gpprPrec t p d a
  isNullary (L1 a) = isNullary a
  isNullary (R1 a) = isNullary a

instance (GOutputable f, GOutputable g) => GOutputable (f :*: g) where
  gpprPrec Rec p d (f :*: g) = sep $ punctuate comma [pfn, pgn]
    where pfn = gpprPrec Rec p d f
          pgn = gpprPrec Rec p d g

  gpprPrec t@(Inf s) p d (f :*: g) =
    pfn <+> text s <+> pgn
    where pfn = gpprPrec t p d f
          pgn = gpprPrec t p d g

  gpprPrec Pref p d (f :*: g) = sep [gpprPrec Pref p d f, gpprPrec Pref p d g]

  isNullary _ = False
