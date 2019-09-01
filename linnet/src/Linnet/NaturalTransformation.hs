{-# LANGUAGE MultiParamTypeClasses #-}

module Linnet.NaturalTransformation where

-- | Type class that defines transformation F a -> G a used by Linnet to convert custom monads to WAI IO
class NaturalTransformation f g where
  mapK :: f a -> g a

instance NaturalTransformation IO IO where
  mapK = id
