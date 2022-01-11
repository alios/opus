module Codec.Audio.Opus.Decoder.Conduit
  ( decoderC, decoderLazyC
  , decoderSink
  ) where

import           Codec.Audio.Opus.Decoder
import           Conduit
import           Control.Lens.Operators
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Lazy     as BL
import           Data.Conduit.Combinators
import           Prelude                  (($))

decoderC :: (HasDecoderStreamConfig cfg, MonadResource m) =>
  cfg -> ConduitT ByteString ByteString m ()
decoderC cfg = withDecoder (cfg ^. deStreamDecoderConfig) $
  \d -> mapM (opusDecode d cfg)

decoderLazyC :: (HasDecoderStreamConfig cfg, MonadResource m) =>
  cfg -> ConduitT ByteString BL.ByteString m ()
decoderLazyC cfg = withDecoder (cfg ^. deStreamDecoderConfig) $
  \d -> mapM (opusDecodeLazy d cfg)

decoderSink :: (HasDecoderStreamConfig cfg, MonadResource m) =>
  cfg -> ConduitT ByteString o m BL.ByteString
decoderSink cfg = withDecoder (cfg ^. deStreamDecoderConfig) $
  \d -> foldMapM (opusDecodeLazy d cfg)

withDecoder :: (HasDecoderConfig cfg, MonadResource m) =>
  cfg -> (Decoder -> ConduitT i o m r) -> ConduitT i o m r
withDecoder cfg = bracketP (opusDecoderCreate cfg) opusDecoderDestroy
