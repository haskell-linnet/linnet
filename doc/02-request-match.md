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
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeSynonymInstances   #-}


import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import           Data.Text
import           Data.List.NonEmpty
import           Linnet
import           Linnet.Endpoint            (root)
import           Network.Wai

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

**Variable segment**

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
pathsEndpoint = (fmap (* 2)) <$> get(paths @Int)
```

Again, `TypeApplications` should be enabled.

**Empty path**

`pathEmpty` matches iff the reminder path is empty. Could be handy to match root requests with empty path:

```haskell top
rootRequest :: Endpoint IO ()
rootRequest = get(pathEmpty) ~>> (return $ ok ())
``` 

## Params

Linnet provides set of endpoints for extracting query parameters from the request. As their type is still `Endpoint m a`,
it's usual to combine them together with other endpoints using sequential combination of `(//)`.

> **Notice**  
> All the endpoints for extracting query parameters below _always match_ the request but might raise an error in context of `m` monad, therefore
> require `MonadThrow` constraint.

**Single required parameter**

`param` endpoint extracts and decode value from request query part:

```haskell top
paramEndpoint :: Endpoint IO Int
paramEndpoint = get(param @Int "paramName")
```

Again, `{-# LANGUAGE OverloadedStrings #-}` extension should be enabled.  
In case if parameter is missing or malformed, `LinnetError` is be thrown in context of `m`.

**Single optional parameter**

```haskell top
paramMaybeEndpoint :: Endpoint IO (Maybe Int)
paramMaybeEndpoint = get(paramMaybe @Int "paramName")
```

In case if parameter is missing, `Nothing` is returned. Throws an exception on malformed value.

**List of values**

`params` endpoint retrieves list of values of **repeating** parameters:

```haskell top
paramsEndpoint :: Endpoint IO [Int]
paramsEndpoint = get(params @Int "paramName")
```

In case if parameter is missing, empty list is returned. Throws an exception on malformed value.

**Non-empty list of values**

`paramsNel` is similar to `params` endpoint but throws an exception in case of missing parameter:

```haskell top
paramsNelEndpoint :: Endpoint IO (NonEmpty Int)
paramsNelEndpoint = get(paramsNel @Int "paramName")
```

## Headers

Just as in case of params, headers are composable with the rest of the endpoints using operators `(//)` or `(|+|)`.
In Linnet, everything is `Endpoint`. 

> **Notice**  
> All the endpoints for extracting headers below _always match_ the request but might raise an error in context of `m` monad, therefore
> require `MonadThrow` constraint.

**Required header extraction**

`header` allows to extract and decode value of specific request header:

```haskell top
headerEndpoint :: Endpoint IO Text
headerEndpoint = get(header @Text "My-Header")
```

`{-# LANGUAGE OverloadedStrings #-}` extension should be enabled.  
In case if header is missing or malformed, `LinnetError` is be thrown in context of `m`.

**Optional header extraction**

```haskell top
headerMaybeEndpoint :: Endpoint IO (Maybe Text)
headerMaybeEndpoint = get(headerMaybe @Text "My-Header")
```

In case if header is missing, `Nothing` is returned. Throws an exception on malformed value.

## Bodies

Body endpoints take a special treatment in Linnet ecosystem. There are multiple key features:  

* All `body*` endpoints have `Decode ct a` constraint that is used to decode request body of Content-Type `ct` 
into the specific value of type `a`
* All `body*` endpoints require non-chunked request with predetermined `Content-Length` size, otherwise they might _not
match_ the request

```haskell top
-- Example of Decode instance for text/plain Content-Type and strict ByteString type
instance Decode TextPlain BS.ByteString where
    decode = Right . BL.toStrict
```


**Decoding request body**

```haskell top
bodyEndpoint :: Endpoint IO BS.ByteString
bodyEndpoint = post(body @TextPlain @BS.ByteString)
```

* In case if `Content-Length` of request is being chunked or missing, endpoint isn't _matched_.
* In case if `Content-Length` is 0, `LinnetError` is raised.
* In case if `Decode` can't decode a malformed body of a request, `LinnetError` is raised.

**Decoding optional body**

```haskell top
bodyMaybeEndpoint :: Endpoint IO (Maybe BS.ByteString)
bodyMaybeEndpoint = post(bodyMaybe @TextPlain @BS.ByteString)
```

* In case if `Content-Length` of request is being chunked or missing, endpoint isn't _matched_.
* In case if `Content-Length` is 0, `Nothing` is returned
* In case if `Decode` can't decode a malformed body of a request, `LinnetError` is raised.

Linnet also exposes couple of useful aliases to avoid type application for `Content-Type` every time:

```haskell
jsonBody @a      == body @ApplicationJson @a
jsonBodyMaybe @a == bodyMaybe @ApplicationJson @a
textBody @a      == body @TextPlain @a
textBodyMaybe @a == bodyMaybe @TextPlain @a
```

## Cookies

In case of cookies, similar to `headers*` endpoints are available: 

**Extract cookie**
```haskell top
cookieEndpoint :: Endpoint IO Int
cookieEndpoint = get(cookie @Int "cookieName")
```

**Extract optional cookie**
```haskell top
cookieMaybeEndpoint :: Endpoint IO (Maybe Int)
cookieMaybeEndpoint = get(cookieMaybe @Int "cookieName")
```

Mind that `cookieName` is case-sensitive.

# Related topics
- [Endpoint](01-endpoint.html)
- [Working with JSON](07-json.html)
- [Streaming](08-streaming.html)