{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Examples.Middleware
  ( app
  ) where

import           Colog                     (HasLog (..), LogAction, Message,
                                            WithLog, cmap, fmtMessage, logInfo,
                                            logTextStdout, richMessageAction,
                                            usingLoggerT)
import           Control.Exception         (SomeException)
import           Control.Monad.Catch       (MonadCatch, MonadThrow)
import           Control.Monad.Fail        (MonadFail)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (MonadReader (..), ReaderT (..))
import           Data.ByteString           (ByteString)
import qualified Data.CaseInsensitive      as CI
import           Data.Function             ((&))
import           Data.Text                 (Text, append, pack)
import           Data.Text.Encoding        (decodeUtf8)
import           Linnet
import           Network.HTTP.Types.Status (unauthorized401)
import           Network.Wai               (Application, Request (..), Response,
                                            responseLBS)
import           Network.Wai.Handler.Warp  (run)

instance Encode TextPlain SomeException where
  encode _ = mempty

type Middleware m = ReaderT Request m Response -> ReaderT Request m Response

auth :: (MonadReader (RequestEnv m) m) => Middleware m
auth downstream =
  ReaderT
    (\req ->
       case lookup (CI.mk "Authorization") $ requestHeaders req of
         Just "secret" -> local (authorizedEnv "secret") (runReaderT downstream req)
         _ -> pure $ responseLBS unauthorized401 [] mempty)

logging :: (WithLog env Message m) => Middleware m
logging downstream =
  ReaderT
    (\req -> do
       logInfo $ pack ("Request: " <> show req)
       runReaderT downstream req)

helloWorld :: (MonadReader (RequestEnv m) m, MonadCatch m, MonadFail m) => Endpoint m Text
helloWorld =
  get (p' "hello" // path @Text) ~>>
  (\name -> do
     AuthorizedRequestEnv _ key <- ask
     return $ ok ("Hello, " `append` name `append` ". Your secret is: " `append` decodeUtf8 key))

app :: Application
app = bootstrap @TextPlain helloWorld & compile & filters & toApp (\m -> runReaderT (unReq m) emptyRequestEnv)
  where
    filters = logging . auth -- execution order is inversed here, as the auth middleware is called inside of logging middleware

-- | Request-local environment that is either "empty" or "authorized"
data RequestEnv m
  = EmptyRequestEnv
      { requestLogAction :: !(LogAction m Message)
      }
  | AuthorizedRequestEnv
      { requestLogAction :: !(LogAction m Message)
      , secretKey        :: !ByteString
      }

-- | Constructor for initial request env
emptyRequestEnv :: (MonadIO m) => RequestEnv m
emptyRequestEnv = EmptyRequestEnv {requestLogAction = cmap fmtMessage logTextStdout}

-- | Transformer to @AuthorizedRequestEnv@
authorizedEnv :: ByteString -> RequestEnv m -> RequestEnv m
authorizedEnv secret env = AuthorizedRequestEnv {requestLogAction = requestLogAction env, secretKey = secret}

-- | Request-local monad that is just a newtype around ReaderT
newtype Req a =
  Req
    { unReq :: ReaderT (RequestEnv Req) IO a
    }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (RequestEnv Req), MonadThrow, MonadCatch, MonadFail)

instance HasLog (RequestEnv m) Message m where
  getLogAction = requestLogAction
  setLogAction newLogAction context = context {requestLogAction = newLogAction}
