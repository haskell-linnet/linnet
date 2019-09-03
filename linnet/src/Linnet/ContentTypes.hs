{-# LANGUAGE DataKinds #-}

module Linnet.ContentTypes
  ( TextHtml
  , TextPlain
  , ApplicationJson
  ) where

import Data.Data (Proxy)

-- | Content-Type literal for @text/html@ encoding
type TextHtml = Proxy "text/html"

-- | Content-Type literal for @text/plain@ encoding
type TextPlain = Proxy "text/plain"

-- | Content-Type literal for @application/json@ encoding
type ApplicationJson = Proxy "application/json"
