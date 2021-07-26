module Control.Carrier.Frontend.Bootstrap where

import           Control.Algebra
import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Effect.Frontend
import           Data.Functor
import qualified Data.HashMap.Strict           as HM
import           Data.Kind                     (Type)
import           Data.Maybe
import           Data.Text
import           Data.Text.Lazy                (toStrict)
import qualified Data.Vector                   as V
import           GHC.Generics                  (Generic)
import           Text.Blaze.Html               (Html)
import           Text.Blaze.Html.Renderer.Text
import           Text.Karver

import           Lib

data BootstrapFrontend = BootstrapFrontend
  { threadsTemplate  :: !Text
  , commentsTemplate :: !Text }
  deriving (Generic)

newtype BootstrapC m a = MkBootstrapC { runBootstrap :: m a }
  deriving (Functor, Applicative, Monad)

instance (Has (Lift IO) sig m, Has (Reader BootstrapFrontend) sig m) => Algebra (Frontend :+: sig) (BootstrapC m) where
  alg hdl sig ctx = case sig of
    R other -> MkBootstrapC $ alg (runBootstrap . hdl) other ctx

    L (AllThreadsPage threads) -> do
      tt <- asks threadsTemplate
      let
        content = HM.fromList
          [ ("threads", List $ V.fromList (fmap (Literal . name) threads))
          , ("title", Literal "Threads") ]
      pure $ renderTemplate content tt <$ ctx

    L (ThreadPage threadName comments) -> do
      ct <- asks commentsTemplate
      let
        parseComment (comment :: Comment Html) = Object $ HM.fromList
          [ ("id", (pack . show . id_) comment)
          , ("text", (toStrict . renderHtml . text) comment)
          , ("replyTo", (pack . maybe "Nothing" show . replyToId) comment)
          , ("date", date comment) ]
        content = HM.fromList
          [ ("title", Literal threadName)
          , ("comments", List $ V.fromList $ parseComment <$> comments) ]
      pure $ renderTemplate content ct <$ ctx
