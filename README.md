Linnet
------

Linnet [ˈlɪnɪt] is a lightweight Haskell library for building HTTP API on top of [WAI](http://hackage.haskell.org/package/wai). Library design is heavily inspired by Scala [Finch](https://github.com/finagle/finch).

Check out [linnet.io](http://linnet.io) for documentation.

Badges
------
[![Travis (.com) branch](https://img.shields.io/travis/com/haskell-linnet/linnet/master?style=flat-square)](https://travis-ci.com/haskell-linnet/linnet) [![Gitter](https://img.shields.io/gitter/room/haskell-linnet/community?style=flat-square)](https://gitter.im/haskell-linnet/community) ![Hackage](https://img.shields.io/hackage/v/linnet?style=flat-square)

Hello world
-----------

Here is an example of running simple application using Warp server:
```haskell
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeSynonymInstances   #-}

import Control.Exception (SomeException)
import Data.Function ((&))
import Data.Text (Text, append)
import Linnet
import Network.Wai.Handler.Warp (run)

-- It's necessary to define encoding of exceptions for content-type "text/plain". Here it returns no content
instance Encode TextPlain SomeException where
 encode _ = mempty

helloWorld = get(p' "hello" // path @Text) ~>> (\name -> return $ ok ("Hello, " `append` name))

main :: IO ()
main = run 9000 app
       where app = bootstrap @TextPlain helloWorld & compile & toApp id
```

Now try to call your application with:
```
curl -v http://localhost:9000/hello/linnet
```

Maintainers
-------
* [Sergey Kolbasov](https://github.com/sergeykolbasov)

License
-------
Licensed under the **[Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0)** (the "License");
you may not use this software except in compliance with the License.

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
