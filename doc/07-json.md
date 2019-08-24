---
title: Working with JSON
---

# Working with JSON

Package [linnet-aeson](http://hackage.haskell.org/package/linnet-aeson) enables the support of
JSON requests and responses by providing orphan instances of `Encode ApplicationJson` and `Decode ApplicationJson` for
any type that has [aeson's](http://hackage.haskell.org/package/aeson) `FromJSON` and `ToJSON` instances.

Add the following import to derive corresponding instances whenever it's needed:

```haskell
import Linnet.Aeson
```

Usually, it's a place of invocation of `Bodies.bodyJson` for decoding a request
and `Bootstrap.compile` for encoding a response.