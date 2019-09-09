---
title: Error Handling
---

# Error handling

As it was mentioned [earlier](04-encode-decode.html) in the [documentation](02-request-match.html), it's quite natural
for exceptions to occur. Linnet provides a way to handle errors in the preferable way.

## Monad exceptions

Functions `handle`, `handleAll`, and `try` of module `Endpoint` allow to set fallback output
in case of exception inside of monad `m (Output a)`: 

```haskell top
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

import           Control.Monad.Reader
import qualified Control.Monad.Catch       as Catch
import           Control.Exception         (Exception, SomeException)
import           Data.Function             ((&))
import           Data.Functor              ((<&>))
import           Linnet
import           Linnet.Endpoint
import           Linnet.ToResponse
import           Network.HTTP.Types.Status (internalServerError500)
import           Network.Wai

data TestException = TestException deriving (Show, Eq)

instance Exception TestException

failedEndpoint :: Endpoint IO Int
failedEndpoint = liftOutputM (Catch.throwM TestException)

restoredEndpoint :: Endpoint IO Int
restoredEndpoint = failedEndpoint & handle (\(e :: TestException) -> return $ internalServerError e) 

restoredAllEndpoint :: Endpoint IO Int
restoredAllEndpoint = failedEndpoint & handleAll (const . return $ ok 42)

triedEndpoint :: Endpoint IO (Either TestException Int)
triedEndpoint = failedEndpoint & try @TestException 
```

## Render them all

[Encode & Decode](04-encode-decode.html) page describes the need for `Encode ct SomeException` instance being available
in compile-time to render errors. **The catch is that it's used ONLY for rendering failed `Output`!** That's it, only
errors that are encapsulated inside of `Output.ErrorPayload` are rendered that way.

If it happens that monad `m` wasn't handled anyhow, exception is stored in the `Left` part of `Compiled m` result.
In case if it's not handled afterwards, it's propagated to the server and the only thing that
client would probably get is the empty Internal Server Error page.

One possible solution to render _all_ errors is to set up middleware that handles errors in context of `Compiled m` and
returns a response. It's even possible to re-use instance `ToResponse ct SomeException` for that: 

```haskell top
instance Encode ct SomeException where
    encode _ = "Wow, so exception"

compiled :: Compiled IO
compiled = bootstrap @TextPlain failedEndpoint & compile

-- the "failed" Either of ReaderT is fed into `toResponse` of `ToResponse` type class
-- thanks to `MonadCatch (Exception SomeException)` instance
handled :: Compiled IO
handled = compiled <&> Catch.handleAll (Right . (toResponse @TextPlain internalServerError500 []))

-- now compile it into WAI application
app = toApp handled
```

# Related topics
- [Endpoint](01-endpoint.html)
- [Run Endpoint](06-run-endpoint.html)
