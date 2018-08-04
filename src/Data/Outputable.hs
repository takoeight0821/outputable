{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Outputable
  ( Outputable(..)
  ) where

import           Control.Applicative                (ZipList)
import           Data.Char                          (isAlphaNum)
import           Data.Complex                       (Complex)
import           Data.Functor.Identity              (Identity)
import           Data.Int                           (Int16, Int32, Int64, Int8)
import           Data.List.NonEmpty                 (NonEmpty)
import           Data.Monoid                        (All, Any, Dual, First,
                                                     Last, Product, Sum)
import qualified Data.Semigroup                     as S (First, Last, Max, Min,
                                                          Option, WrappedMonoid)
import           Data.Text                          (Text)
import qualified Data.Text.Lazy                     as TL (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Internal (Doc ())
import           Data.Version                       (Version)
import           Data.Void                          (Void)
import           Data.Word                          (Word16, Word32, Word64,
                                                     Word8)
import           GHC.Generics
import           Numeric.Natural                    (Natural)
import           System.Exit                        (ExitCode)

class Outputable a where
  pprPrec :: Int -> a -> Doc ann
  default pprPrec :: (Generic a, GOutputable (Rep a)) => Int -> a -> Doc ann
  pprPrec n x = gppr (from x) Pref n False

  ppr :: a -> Doc ann
  ppr = pprPrec 0

  pprList :: [a] -> Doc ann
  pprList = brackets . sep . punctuate comma . map ppr

wrapParens :: Bool -> Doc ann -> Doc ann
wrapParens False s = s
wrapParens True x  = lparen <> x <> rparen

data Type = Rec | Pref | Inf String
  deriving Eq

class GOutputable (f :: * -> *) where
  gppr :: f x -> Type -> Int -> Bool -> Doc ann
  isNullary :: f x -> Bool

instance GOutputable V1 where
  gppr _ _ _ _ = emptyDoc
  isNullary = error "generic outputable (isNullary): unnecessary case"

instance GOutputable U1 where
  gppr _ _ _ _ = emptyDoc
  isNullary _ = True

instance (GOutputable f, Datatype c) => GOutputable (M1 D c f) where
  gppr (M1 a) = gppr a
  isNullary (M1 a) = isNullary a

instance (GOutputable f, Selector c) => GOutputable (M1 S c f) where
  gppr s@(M1 a) t d p
    | selector == "" = gppr a t d p
    | otherwise = pretty selector <+> pretty '=' <+> align (gppr a t 0 p)
    where selector = selName s
  isNullary (M1 a) = isNullary a

instance (GOutputable f, Constructor c) => GOutputable (M1 C c f) where
  gppr c@(M1 a) _ d _ =
    case fixity of
      Prefix -> wrapParens boolParens $ pretty name
                <+> (if t == Rec then braces else id) (align (gppr a t 11 boolParens))
      Infix _ m -> wrapParens (d > m) $ gppr a t (m + 1) (d > m)
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
  gppr (K1 a) _ d _ = pprPrec d a
  isNullary _ = False

instance (GOutputable f, GOutputable g) => GOutputable (f :+: g) where
  gppr (L1 a) t d p = gppr a t d p
  gppr (R1 a) t d p = gppr a t d p
  isNullary (L1 a) = isNullary a
  isNullary (R1 a) = isNullary a

instance (GOutputable f, GOutputable g) => GOutputable (f :*: g) where
  gppr (f :*: g) Rec d p = pfn <> comma <+> pgn
    where pfn = gppr f Rec d p
          pgn = gppr g Rec d p

  gppr (f :*: g) t@(Inf s) d p =
    pfn <+> pretty s <+> align pgn
    where pfn = gppr f t d p
          pgn = gppr g t d p

  gppr (f :*: g) Pref n p = gppr f Pref n p <+> gppr g Pref n p

  isNullary _ = False

viaPretty :: Pretty a => Int -> a -> Doc ann
viaPretty _ x = pretty x

viaPrettyNum :: (Ord a, Num a, Pretty a) => Int -> a -> Doc ann
viaPrettyNum n x
  | n /= 0 && x < 0 = parens $ pretty x
  | otherwise = pretty x

instance Outputable Double where pprPrec = viaPrettyNum
instance Outputable Float where pprPrec = viaPrettyNum
instance Outputable Int where pprPrec = viaPrettyNum
instance Outputable Int8 where pprPrec = viaPrettyNum
instance Outputable Int16 where pprPrec = viaPrettyNum
instance Outputable Int32 where pprPrec = viaPrettyNum
instance Outputable Int64 where pprPrec = viaPrettyNum
instance Outputable Integer where pprPrec = viaPrettyNum
instance Outputable Natural where pprPrec = viaPretty
instance Outputable Word where pprPrec = viaPretty
instance Outputable Word8 where pprPrec = viaPretty
instance Outputable Word16 where pprPrec = viaPretty
instance Outputable Word32 where pprPrec = viaPretty
instance Outputable Word64 where pprPrec = viaPretty
instance Outputable TL.Text where pprPrec n x = dquotes $ viaPretty n x
instance Outputable Text where pprPrec n x = dquotes $ viaPretty n x
instance Outputable a => Outputable [a] where pprPrec _ = pprList

instance Outputable Char where
  pprPrec _ x = squotes $ pretty x
  pprList xs = dquotes $ pretty xs

instance Outputable Bool
instance Outputable Ordering
instance Outputable ()
instance Outputable DecidedStrictness
instance Outputable SourceStrictness
instance Outputable SourceUnpackedness
instance Outputable Associativity
instance Outputable Fixity
instance Outputable Any
instance Outputable All
instance Outputable ExitCode
instance Outputable Version
instance Outputable Void
instance Outputable a => Outputable (Maybe a)
instance Outputable p => Outputable (Par1 p)
instance Outputable a => Outputable (NonEmpty a)
instance Outputable a => Outputable (Product a)
instance Outputable a => Outputable (Sum a)
instance Outputable a => Outputable (Dual a)
instance Outputable a => Outputable (Last a)
instance Outputable a => Outputable (First a)
instance Outputable a => Outputable (Identity a)
instance Outputable a => Outputable (ZipList a)
instance Outputable a => Outputable (S.Option a)
instance Outputable m => Outputable (S.WrappedMonoid m)
instance Outputable a => Outputable (S.Last a)
instance Outputable a => Outputable (S.First a)
instance Outputable a => Outputable (S.Max a)
instance Outputable a => Outputable (S.Min a)
instance Outputable a => Outputable (Complex a)
instance (Outputable a, Outputable b) => Outputable (Either a b)
instance (Outputable a, Outputable b) => Outputable (a, b)
