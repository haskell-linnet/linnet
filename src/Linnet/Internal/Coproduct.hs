{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Linnet.Internal.Coproduct
  ( Coproduct(..)
  , CNil
  , AdjoinCoproduct(..)
  ) where

data CNil

data Coproduct a b where
  Inl :: a -> Coproduct a b
  Inr :: b -> Coproduct a b
  deriving (Eq)

class ExtendBy l r out | l r -> out where
  left :: r -> out
  right :: l -> out

instance (ExtendLeftBy l r out, ExtendRightBy l r out) => ExtendBy l r out where
  left = extendLeftBy @l
  right = extendRightBy @l @r

-- | Extend coproduct @cs'@ on left with coproduct @cs@, somewhat similar to prepending list @cs@ to @cs'@
class ExtendLeftBy l r out | l r -> out where
  extendLeftBy :: r -> out

instance (Reverse l revL, ExtendLeftBy' revL r out) => ExtendLeftBy l r out where
  extendLeftBy = extendLeft' @revL

class ExtendLeftBy' revL r out | revL r -> out where
  extendLeft' :: r -> out

instance ExtendLeftBy' CNil a a where
  extendLeft' = id

instance (ExtendLeftBy' t (Coproduct h r) out) => ExtendLeftBy' (Coproduct h t) r out where
  extendLeft' r = extendLeft' @t @(Coproduct h r) $ Inr r

-- | Extend coproduct @cs'@ on right with coproduct @cs@, somewhat similar to appending list @cs@ to @cs'@
class ExtendRightBy l r out | l r -> out where
  extendRightBy :: l -> out

instance ExtendRightBy l CNil l where
  extendRightBy = id

instance (ExtendRight l h out, ExtendRightBy out t out') => ExtendRightBy l (Coproduct h t) out' where
  extendRightBy l = extendRightBy @out @t $ extendRight @l @h l

class ExtendRight cs t out | cs t -> out where
  extendRight :: cs -> out

instance (ExtendRight' (ExtendRightT cs) cs t out) => ExtendRight cs t out where
  extendRight = extendRight' @(ExtendRightT cs) @cs @t

class ExtendRight' (isLast :: Bool) cs t out | cs t -> out where
  extendRight' :: cs -> out

instance ExtendRight' 'True (Coproduct h CNil) a (Coproduct h (Coproduct a CNil)) where
  extendRight' (Inl h) = Inl h
  extendRight' (Inr t) = Inr $ Inr t

instance (ExtendRight t a out) => ExtendRight' 'False (Coproduct h t) a (Coproduct h out) where
  extendRight' (Inl h) = Inl h
  extendRight' (Inr t) = Inr $ extendRight @t @a t

type family ExtendRightT a where
  ExtendRightT (Coproduct _ CNil) = 'True
  ExtendRightT _ = 'False

-- | Reverse coproduct
class Reverse cs c | cs -> c where
  reverseCoproduct :: cs -> c

instance (Reverse' CNil cs out) => Reverse cs out where
  reverseCoproduct cs = reverseCoproduct' @CNil $ Right cs

class Reverse' acc cs out | acc cs -> out where
  reverseCoproduct' :: Either acc cs -> out

instance Reverse' acc CNil acc where
  reverseCoproduct' e =
    case e of
      Left acc -> acc

instance (Reverse' (Coproduct a acc) b out) => Reverse' acc (Coproduct a b) out where
  reverseCoproduct' e =
    reverseCoproduct' $
    case e of
      Left acc      -> Left $ Inr acc
      Right (Inl h) -> Left $ Inl h
      Right (Inr t) -> Right t

-- | Flatten nested coproduct
class AdjoinCoproduct cs c | cs -> c where
  adjoinCoproduct :: cs -> c

instance (AdjoinCoproduct' (AdjoinCoproductT cs) cs c) => AdjoinCoproduct cs c where
  adjoinCoproduct = adjoinCoproduct' @(AdjoinCoproductT cs)

class AdjoinCoproduct' (isNested :: Bool) cs c | cs -> c where
  adjoinCoproduct' :: cs -> c

instance AdjoinCoproduct' 'False CNil CNil where
  adjoinCoproduct' = id

instance (h ~ h', AdjoinCoproduct t out) => AdjoinCoproduct' 'False (Coproduct h' t) (Coproduct h out) where
  adjoinCoproduct' (Inl h) = Inl h
  adjoinCoproduct' (Inr t) = Inr $ adjoinCoproduct t

instance (AdjoinCoproduct t out, ExtendBy h out out') => AdjoinCoproduct' 'True (Coproduct h t) out' where
  adjoinCoproduct' (Inl h) = right @h @out h
  adjoinCoproduct' (Inr t) = left @h $ adjoinCoproduct t

type family AdjoinCoproductT a where
  AdjoinCoproductT (Coproduct (Coproduct _ _) _) = 'True
  AdjoinCoproductT _ = 'False
