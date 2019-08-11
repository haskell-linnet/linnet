{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Linnet.Internal.HList
  ( HList(..)
  , AdjoinHList(..)
  , FnToProduct(..)
  ) where

infixr 6 :::

data HList xs where
  HNil :: HList '[]
  (:::) :: a -> HList as -> HList (a ': as)

class Prepend a b ab | a b -> ab where
  prependHList :: HList a -> HList b -> HList ab

instance (Prepend' (PrependT a b) a b ab) => Prepend a b ab where
  prependHList = prependHList' @(PrependT a b)

class Prepend' (lists :: PrependHList) a b ab | a b -> ab where
  prependHList' :: HList a -> HList b -> HList ab

data PrependHList
  = LeftHNil
  | RightHNil
  | BothHNil
  | NoneHNil

type family PrependT a b where
  PrependT '[] '[] = 'BothHNil
  PrependT _ '[] = 'RightHNil
  PrependT '[] _ = 'LeftHNil
  PrependT _ _ = 'NoneHNil

instance Prepend' 'BothHNil '[] '[] '[] where
  prependHList' _ _ = HNil

instance Prepend' 'LeftHNil '[] b b where
  prependHList' _ b = b

instance Prepend' 'RightHNil a '[] a where
  prependHList' a _ = a

instance (Prepend as (b ': bs) cs) => Prepend' 'NoneHNil (a ': as) (b ': bs) (a ': cs) where
  prependHList' (a ::: as) (b ::: bs) = a ::: prependHList as (b ::: bs)

class AdjoinHList ls l | ls -> l where
  adjoin :: HList ls -> HList l

instance AdjoinHList' (NeedAdjoin ls) ls l => AdjoinHList ls l where
  adjoin = adjoin' @(NeedAdjoin ls)

class AdjoinHList' (needAdjoin :: Bool) ls l | ls -> l where
  adjoin' :: HList ls -> HList l

instance AdjoinHList' 'False '[] '[] where
  adjoin' _ = HNil

instance (AdjoinHList t out) => AdjoinHList' 'False (h ': t) (h ': out) where
  adjoin' (h ::: t) = h ::: adjoin t

instance (AdjoinHList t ao, Prepend a ao po) => AdjoinHList' 'True (HList a ': t) po where
  adjoin' (l ::: t) = prependHList l (adjoin t)

type family NeedAdjoin (l :: [*]) :: Bool where
  NeedAdjoin (HList a ': t) = 'True
  NeedAdjoin '[] = 'False
  NeedAdjoin (a ': t) = 'False

instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show a, Show (HList as)) => Show (HList (a ': as)) where
  show (a ::: as) = show a ++ " ::: " ++ show as

class FnToProduct fn ls out | fn ls -> out, ls out -> fn where
  fromFunction :: fn -> HList ls -> out

instance (v ~ fn) => FnToProduct fn '[] v where
  fromFunction v HNil = v

instance (FnToProduct fnOut tail out) => FnToProduct (input -> fnOut) (input ': tail) out where
  fromFunction fn (input ::: t) = fromFunction (fn input) t