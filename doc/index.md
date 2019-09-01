---
title: Home
---

Linnet is a slim wrapper around WAI [Application](http://hackage.haskell.org/package/wai-3.2.2.1/docs/Network-Wai.html#t:Application)
with `Endpoint m a` as a main abstraction that takes HTTP request as an input and returns `m a` as an output.

Simple but powerful combinator nature of `Endpoint` together with provided transformers enables building of complex
HTTP API at a high degree of maintainability and efficiency.

## Hello world

Here is a simple definition of endpoint that returns `Hello, $name`

```haskell top
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeSynonymInstances   #-}

import Control.Exception (SomeException)
import Data.Function ((&))
import Data.Text (Text, append)
import Linnet
import Network.Wai (Application)

-- Linnet makes no assumption on how to encode exceptions.
-- It's necessary to define encoder of exceptions for used content-types.
-- Here it returns no content
instance Encode TextPlain SomeException where
  encode _ = mempty

helloName :: Endpoint IO Text
helloName = get(p' "hello" // path @Text) ~>>
               (\name -> return $ ok ("Hello, " `append` name))

app :: Application
app = bootstrap @TextPlain helloName & compile & toApp
```

To run the application using re-imported [Warp](http://hackage.haskell.org/package/warp-3.3.0/docs/Network-Wai-Handler-Warp.html#v:run):
```haskell
main :: IO ()
main = run 9000 app
```

Now, to the call:
```bash
curl -v http://localhost:9000/hello/linnet
```

## Additional sources

- [API documentation](http://hackage.haskell.org/package/linnet)