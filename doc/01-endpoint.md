---
title: Endpoint
---

## What is Endpoint

The foundation of Linnet library is `Endpoint` data type:

```haskell
data Endpoint (m :: * -> *) a =
  Endpoint
    { runEndpoint :: Input -> EndpointResult m a
    , toString    :: Text
    }
```

In fact, it's an abstraction over function that takes a request wrapped
in `Input` and either matches with some output `m a` or doesn't match the request at all:

```haskell
data EndpointResult (m :: * -> *) a
  = Matched
      { matchedReminder :: Input
      , matchedTrace    :: Trace
      , matchedOutput   :: m (Output a)
      }
  | NotMatched
```

## Endpoint combinators

Linnet exposes two operators that allow to combine endpoints using _AND/OR_ logic.

```haskell top hide
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeApplications       #-}

import Data.Text (Text, append)
import Linnet
import Linnet.Internal.HList
import Linnet.Internal.Coproduct
```

**Product operator: (//)**
```haskell top
product :: Endpoint IO (HList '[])
product = get(p'"hello" // p'"world")
```

Operator `//` is used to create product of two or more endpoints that all must _sequentially
match_ to produce a result.  
The resulting type is an endpoint of `HList` containing types of all combined
endpoints.  
It's particularly useful for matching path of request against an endpoint, but not limited by that.

**Coproduct (union) operator: (|+|)**
```haskell top
coproduct :: Endpoint IO (Coproduct (HList '[]) (Coproduct (HList '[Int]) CNil))
coproduct = get(p'"users") |+| delete(p'"users" // path @Int)
```

Operator `|+|` is used to create coproduct (or union) of two or more endpoints where _at least one endpoint
must match_ the request to produce a result.
The resulting type is an endpoint of `Coproduct` containing types of all combined
endpoints.  
Example above shows how to combine together endpoints of different HTTP methods together.

## Transformation

While Linnet employs interesting data types such as `HList` and `Coproduct` to ensure type-safety of each endpoint used,
it's a rare situation to interact with them directly.

There are multiple options available to transform endpoints, and usually this transformation itself is a business logic
of application.

**Endpoint as Applicative**

Both `Functor` and `Applicative` type classes are defined for `Endpoint`, natively exposing simple transformations:

```haskell top
lifted :: Endpoint IO Int
lifted = pure 42

multiplied = (* 2) <$> lifted 
```

**Map over HList**

But often there is a need to handle multiple parameters (product of them). It's still possible
to pattern match an `HList`, but also quite unpractical. That's why there is `~>>` operator available:

```haskell top
nameAndSurname :: Endpoint IO Text
nameAndSurname = (path @Text // path @Text) ~>> (\name surname -> return $ ok (name `append` surname)) 
```

Example above demonstrates how lambda function of two parameters is applied to endpoint of `HList '[Text, Text]`.
Resulting type of this lambda always should be some `m (Output a)`.

**Single-type endpoint**

If endpoint is just a single type like `Endpoint m Int`, beside usual `fmap` there is an operator `~>`
that allows to change default `Ok` output to something different:

```haskell top
nameEndpoint :: Endpoint IO Text
nameEndpoint = get(param @Text "name") ~> (\name -> return $ created ("Name: " `append` name))
```

Strictly speaking, `~>` operator is just an inverted alias of `mapOutputM` function.

# Related topics
- [Matching a request](02-request-match.html)
- [Encode & Decode](04-encode-decode.html)
- [Run Endpoint](06-run-endpoint.html)