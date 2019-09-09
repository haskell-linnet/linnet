---
title: Streaming
---

# Streaming

[linnet-conduit](http://hackage.haskell.org/package/linnet-conduit) package enables use of
`ConduitT` streams for both chunked requests and responses.

```haskell
import Linnet.Conduit
```

Import above exposes:
- orphan instance of `ToResponse ct (ConduitT () a m ())`
- orphan instance of `ToResponse ApplicationJson (ConduitT () a m ())` that intersperse JSON chunks
with newline symbol
- `streamBody :: Endpoint m (ConduitT BL.ByteString BL.ByteString m ())` endpoint

First two instances practically allow to return `ConduitT () a m ()` from endpoints in case if `Encode ct a` is defined.
Linnet runs stream on itself converting it into streaming response with the help of `NatureTransformation m IO` type class.

# Related topics
- [Matching a request](02-request-match.html)
- [Encode & Decode](04-encode-decode.html)