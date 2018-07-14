module Codec.Audio.Opus.Encoder.Conduit
  ( encoderC, encoderLazyC
  , encoderSink
  ) where

import           Codec.Audio.Opus.Encoder
import           Conduit
import           Control.Lens.Operators
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Lazy     as BL
import           Data.Conduit.Combinators
import           Prelude                  (($))

encoderC :: (HasStreamConfig cfg, MonadResource m) =>
  cfg -> ConduitT ByteString ByteString m ()
encoderC cfg = withEncoder (cfg ^. streamConfig) $
  \e -> mapM (opusEncode e cfg)

encoderLazyC :: (HasStreamConfig cfg, MonadResource m) =>
  cfg -> ConduitT ByteString BL.ByteString m ()
encoderLazyC cfg = withEncoder (cfg ^. streamConfig) $
  \e -> mapM (opusEncodeLazy e cfg)

encoderSink :: (HasStreamConfig cfg, MonadResource m) =>
  cfg -> ConduitT ByteString o m BL.ByteString
encoderSink cfg = withEncoder (cfg ^. streamConfig) $
  \e -> foldMapM (opusEncodeLazy e cfg)

withEncoder :: (HasEncoderConfig cfg, MonadResource m) =>
  cfg -> (Encoder -> ConduitT i o m r) -> ConduitT i o m r
withEncoder cfg = bracketP (opusEncoderCreate cfg) opusEncoderDestroy
