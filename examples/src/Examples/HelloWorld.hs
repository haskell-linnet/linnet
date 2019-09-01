{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Examples.HelloWorld
  ( app
  , helloWorld
  ) where

import           Control.Exception        (SomeException)
import           Data.Function            ((&))
import           Data.Text                (Text, append)
import           Linnet
import           Network.Wai              (Application)

instance Encode TextPlain SomeException where
  encode _ = mempty

helloWorld = get (p' "hello" // path @Text) ~>> (\name -> return $ ok ("Hello, " `append` name))

app :: Application
app = bootstrap @TextPlain helloWorld & compile & toApp @IO
