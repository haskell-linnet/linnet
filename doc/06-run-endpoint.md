---
title: Run Endpoint
---

# Run Endpoint

## Bootstrap

After application `Endpoint` is combined and defined, it's time to convert it to WAI
application and run the server. Module `Linnet.Bootstrap` contains set of functions for this purpose.    


```haskell top
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeSynonymInstances   #-}

import Control.Exception (SomeException)
import Data.Function     ((&))
import Data.Text
import Linnet
import Network.Wai

instance Encode TextPlain SomeException where
  encode _ = mempty

endpoint :: Endpoint IO Text
endpoint = get(pathEmpty) ~>> (return . ok $ "Hello, world")

app :: Application
app = bootstrap @TextPlain endpoint & compile & toApp id
```

Linnet re-exports Warp's `run` function to run WAI application:
```haskell
main :: IO ()
main = run 9000 app
```

Here is what happened in the code above:
* Defined endpoint was "bootsrapped" for Content-Type `@Text/Plain`. Result of this
endpoint will be encoded with `Encode TextPlain`
* `Bootstrap` was "compiled" into `ReaderT Request m Response`. It could be useful to take a break
here to add some middleware filters without leaving context of monad `m` by transforming compiled `ReaderT`.
* `ReaderT Request m Response` was transformed to WAI application and run on Warp server

## Serving multiple Content-Types

Often, server shall return different content depending on the expected Content-Type. Serving html page by encoding
it as JSON is likely incorrect behavior. Function `serve` in combination with `Bootstrap` allows to serve multiple
endpoints with different Content-Types:

```haskell
indexHtml :: Endpoint IO Text
indexHtml = get(p'"index.html") ~>> (return . ok $ "<html><body>This is HTML</body></html>")

-- Need to define how to render exceptions in HTML encoding
instance Encode TextHtml SomeException where
  encode _ = mempty

multipleContentTypesApp :: Application
multipleContentTypesApp =
  bootstrap @TextPlain endpoint &
  serve @TextHtml indexHtml &
  compile &
  toApp id
```

## Custom monad

Last step of converting `Endpoint` into `Application` is to call function `toApp` that has a following signature:

```haskell
toApp :: (forall a. m a -> IO a) -> ReaderT Request m Response -> Application
```

The first argument is a natural transformation `m ~> IO` and the second is compiled `ReaderT`.
It allows to use custom monad until the very end of request resolution and could be useful to run logger,
create request-local context etc. This example demonstrates how to make it work.