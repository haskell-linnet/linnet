{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Linnet.Decode
  ( Decode(..)
  , DecodePath(..)
  , DecodeEntity(..)
  ) where

import           Control.Arrow              (left, right)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Conversion as BC
import qualified Data.ByteString.Lazy       as BL
import           Data.Either.Combinators
import qualified Data.Text                  as T
import           Data.Text.Read             (decimal, double, rational, signed)
import           GHC.Base                   (Symbol)
import           Linnet.Errors

class Decode (ct :: Symbol) a where
  decode :: BL.ByteString -> Either LinnetError a

class DecodePath a where
  decodePath :: T.Text -> Maybe a

instance DecodePath T.Text where
  decodePath = Just

instance DecodePath Integer where
  decodePath t = rightToMaybe $ fst <$> signed decimal t

instance DecodePath Int where
  decodePath t = rightToMaybe $ fst <$> signed decimal t

instance DecodePath Double where
  decodePath t = rightToMaybe $ fst <$> signed double t

instance DecodePath Float where
  decodePath t = rightToMaybe $ fst <$> signed rational t

instance DecodePath Rational where
  decodePath t = rightToMaybe $ fst <$> signed rational t

class DecodeEntity a where
  decodeEntity :: B.ByteString -> Either LinnetError a
  default decodeEntity :: (BC.FromByteString a) =>
    B.ByteString -> Either LinnetError a
  decodeEntity = left (DecodeEntityError . BC.toByteString') . BC.runParser BC.parser

instance DecodeEntity B.ByteString

instance DecodeEntity T.Text

instance DecodeEntity Integer

instance DecodeEntity Int

instance DecodeEntity Double

instance DecodeEntity Float where
  decodeEntity = right realToFrac . decodeEntity @Double

instance DecodeEntity Rational where
  decodeEntity = right fromIntegral . decodeEntity @Integer
