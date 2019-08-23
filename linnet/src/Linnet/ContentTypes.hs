{-# LANGUAGE DataKinds #-}

module Linnet.ContentTypes
  ( TextHtml
  , TextPlain
  , ApplicationJson
  ) where

-- | Content-Type literal for @text/html@ encoding
type TextHtml = "text/html"

-- | Content-Type literal for @text/plain@ encoding
type TextPlain = "text/plain"

-- | Content-Type literal for @application/json@ encoding
type ApplicationJson = "application/json"
