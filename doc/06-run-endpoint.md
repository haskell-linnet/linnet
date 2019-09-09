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
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}

import Control.Exception (SomeException)
import Data.Function     ((&))
import Data.Text
import Linnet
import Linnet.Internal.Coproduct ((:+:))
import Network.Wai

instance Encode TextPlain SomeException where
  encode _ = mempty

endpoint :: Endpoint IO Text
endpoint = get(pathEmpty) ~>> (return . ok $ "Hello, world")

app :: Application
app = bootstrap @TextPlain endpoint & compile & toApp
```

Linnet re-exports Warp's `run` function to run WAI application:
```haskell
main :: IO ()
main = run 9000 app
```

Here is what happened in the code above:
* Defined endpoint was "bootsrapped" for Content-Type `@Text/Plain`. Result of this
endpoint will be encoded with `Encode TextPlain`
* `Bootstrap` was "compiled" into `ReaderT (WriterT Trace m) Request (Either SomeException Response)` or `Compiled m` for shortness.
It could be useful to take a break here to add some middleware filters without leaving context of monad `m` by transforming compiled `ReaderT`.
* `Compiled m` was transformed to WAI application and run on Warp server

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
  toApp
```

## Content-Type negotiation

It's also possible to serve different Content-Types depending on client's `Accept` header as it's described in
[RFC 2295](https://tools.ietf.org/html/rfc2295): 

```haskell
contentTypeNegotiationApp :: Application
contentTypeNegotiationApp =
  bootstrap @(TextPlain :+: ApplicationJson) endpoint & compile & toApp
```

Based on the client request and `Accept` header values, Linnet picks the best matching encoder.
Important to mention that in the example above `application/json` content would be returned in case of
failed negotiation.

If there is a need to enable 406 error signaling failed negotiation instead of falling back to the last option,
`NotAcceptable406` comes in handy:

```haskell
negotiateOr406App :: Application
negotiateOr406App =
  bootstrap @(TextPlain :+: ApplicationJson :+: NotAcceptable406) endpoint & compile & toApp
```

## Custom monad

Last step of converting `Endpoint` into `Application` is to call function `toApp` that has a following signature:

```haskell
toApp :: (NaturalTransformation m IO) => Compiled m -> Application
```

The constraint exposed is a natural transformation `m ~> IO` to "run" custom monad as `IO`.
It allows to use this monad until the very end of request resolution and could be useful to run logger,
create request-local context etc.  
[This example](https://github.com/haskell-linnet/linnet/blob/master/examples/src/Examples/Middleware.hs) demonstrates how to make it work.

# Related topics
- [Endpoint](01-endpoint.html)
- [Matching a request](02-request-match.html)
- [Endpoint Tracing](09-tracing.html)