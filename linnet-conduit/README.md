# Linnet Conduit

This package adds support of streaming requests & responses in Linnet library
using [conduit](http://hackage.haskell.org/package/conduit).


```haskell
import Linnet.Conduit
```

Import above exposes:
- orphan instance of `ToResponse ct (ConduitT () a m ())`
- orphan instance of `ToResponse ApplicationJson (ConduitT () a m ())` that intersperse JSON chunks
with newline symbol
- `streamBody :: Endpoint m (ConduitT BL.ByteString BL.ByteString m ())` endpoint