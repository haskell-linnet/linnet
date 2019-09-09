---
title: Endpoint Tracing
---

# Endpoint Tracing

On closer inspection, turns out that `Linnet.Compiled m` is an alias to:
```haskell
 type Compiled m = ReaderT (WriterT Trace m) Request (Either SomeException Response) 
```

Beside returning an `Either` that could be exception or response, the result of reader is also `WriterT Trace m` monad.  
The `Trace` here is a list `[Text]` that carries the information about matched `path*` endpoints. This could be useful
in measuring stats and/or logging of each endpoint for later analysis:

```haskell top
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class          (MonadIO(..))
import Control.Monad.Trans.Reader      (ReaderT(..))
import Control.Monad.Trans.Writer.Lazy (listen)
import Data.Time.Clock.System          (SystemTime(..), getSystemTime)
import Data.Text                       (unpack, intercalate)
import Linnet

naiveStats :: Compiled IO -> Compiled IO
naiveStats compiled =
    ReaderT $
    (\req -> do
        let now = liftIO $ systemNanoseconds <$> getSystemTime
        start <- now
        (result, trace) <- listen $ runReaderT compiled req
        stop <- now
        liftIO $ putStrLn $ "Time taken by " ++ (unpack (intercalate "/" trace)) ++ " is: " ++ show (stop - start)
        return result
    )
``` 



# Related topics
- [Run Endpoint](06-run-endpoint.html)