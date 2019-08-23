# Linnet Aeson

This package adds support of JSON requests & responses in Linnet library
using [aeson](http://hackage.haskell.org/package/aeson).

Add following import to put orphan instances of `Encode` and `Decode` in scope:
```haskell
import Linnet.Aeson
```

Each type that has `FromJSON` and `ToJSON` instances automatically gets corresponding
instances of `Decode ApplicationJson` and `Encode ApplicationJson`.