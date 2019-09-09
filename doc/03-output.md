---
title: Output
---

# Endpoint's Output

In fact, [matched `Endpoint`](01-endpoint.html) returns not just an `m a`, but `m (Output a)`.  

`Output a` is a typed representation of response that is encoded into WAI [Response](http://hackage.haskell.org/package/wai-3.2.2.1/docs/Network-Wai.html#t:Response)
using combination of `Encode` and `ToResponse` type classes.  
Just as response, `Output` carries the information about HTTP status, headers and response body or an error.

Linnet provides helpful functions to create and modify output, named after corresponding HTTP codes:

```haskell top
import Control.Exception
import Linnet
import Network.HTTP.Types.Status
import Network.Wai

data ExampleException = ExampleException deriving (Eq, Show)

instance Exception ExampleException

okOutput :: Output Bool
okOutput = ok(True)

badOutput :: Output Bool
badOutput = badRequest(ExampleException)

errorOutput :: Output Bool
errorOutput = internalServerError(ExampleException)
```

Check out `Outputs` module for the rest of functions.

# Responding with Response

Linnet supports responding with WAI Response out of the box. To do so, pack the response
into some `Output` data:

```haskell top

responseOutput :: Output Response
responseOutput = ok(responseLBS status404 [] mempty)
```

Mind that response status is overridden with `Output` status.

# Related topics
- [Endpoint](01-endpoint.html)