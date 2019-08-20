---
title: Matching a request
---

# Matching a request

Linnet provides plenty of functions & endpoints for request matching to
combine together with help of `(//)` and `(|+|)` operators.

## Methods matching

Following functions are available for matching against request HTTP method.  
In case of method mismatch, resulting endpoint always returns `NotMatched`:

```haskell top
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE OverloadedStrings      #-}

import Linnet
import Linnet.Endpoint (root)
import Network.Wai

getEndpoint :: Endpoint IO Request
getEndpoint = get(root)

postEndpoint :: Endpoint IO Request
postEndpoint = post(root)

putEndpoint :: Endpoint IO Request
putEndpoint = put(root)

patchEndpoint :: Endpoint IO Request
patchEndpoint = patch(root)

deleteEndpoint :: Endpoint IO Request
deleteEndpoint = delete(root)

-- etc
```

Check out `Methods` module API for more functions

## Paths

Matching of request's path is vital part of routing logic. Usually, path endpoints are combined
together using product `(//)` operator:

**Constant segment**

`pathConst` or `p'` alias allows to match a single segment of request against predefined constant:

```haskell top
boolConstEndpoint :: Endpoint IO Bool
boolConstEndpoint = get(p' "foo" // p' "bar") ~>> (return $ ok True)
```

Mind that `{-# LANGUAGE OverloadedStrings #-}` extension should be enabled. 

**Dynamic segment**

`path` endpoint decodes a single segment of request path that is represented as value of resulting endpoint: 

```haskell top
pathVariableEndpoint :: Endpoint IO Int
pathVariableEndpoint = (* 2) <$> get(path @Int)
```

Here `{-# LANGUAGE TypeApplications #-}` pragma adds the support of `@Type` syntax to explicitly declare expected type.

**Consume all segments**

Endpoint `paths` consumes the reminder of input's path and always matches the request as far as types match:

```haskell top
pathsEndpoint :: Endpoint IO [Int]
pathsEndpoint = (map (* 2)) <$> get(paths @Int)
```

Again, `TypeApplications` should be enabled.

**Empty path**

`pathEmpty` matches iff the reminder path is empty. Could be handy to match root requests with empty path:

```haskell top
rootRequest :: Endpoint IO ()
rootRequest = get(pathEmpty) ~>> (return $ ok ())
``` 

## Params

## Headers

## Bodies

## Cookies