---
title: Encode & Decode
---

# Encode

To convert [Output a](03-output.html) to WAI `Response` Linnet employs `Encode` type class:

```haskell
import Data.ByteString.Lazy (ByteString)

class Encode (ct :: Symbol) a where
    encode :: a -> ByteString
```

Content-Type of response is encoded as phantom type `ct`, so compiler always picks up the encoder
for the specific Content-Type if it exists. Then, type `a` is converted as lazy `ByteString` that represents
body of a response.

## Encode SomeException

Linnet doesn't make an assumption on how to encode exceptions occurred during request. It's up to user to decide
on representation of exceptions if there should be any. The simplest case is to return an empty body:

```haskell top
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

import Control.Exception
import Linnet

instance Encode TextPlain SomeException where
    encode _ = mempty
``` 

In a nutshell, this type class could be treated as a single point of exceptions handling where specific errors
are rendered in a way they should be. **Only errors inside of Output are encoded this way**.
Information on the general error handling is available in the corresponding section.

## Decode

`Decode` type class is responsible for decoding request's body into some type `a`:

```haskell
import Data.ByteString.Lazy (ByteString)

class Decode (ct :: Symbol) a where
    decode :: ByteString -> Either LinnetError a
```

And again, Content-Type of expected request is encoded as phantom type `ct` to ensure that correct decoder exists in
compile-time.