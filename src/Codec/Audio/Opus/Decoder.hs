{-# LANGUAGE FlexibleContexts #-}

module Codec.Audio.Opus.Decoder
  ( -- * Decoder
    Decoder, OpusException(..)
    -- ** create
  , withOpusDecoder, opusDecoderCreate, opusDecoderDestroy
    -- ** run
  , opusDecode, opusDecodeLazy
    -- * re-exports
  , module Codec.Audio.Opus.Types
  ) where

import           Codec.Audio.Opus.Internal.Opus
import           Codec.Audio.Opus.Types
import           Control.Lens.Fold
import           Control.Lens.Operators
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import           Foreign

-- | Decoder State
newtype Decoder = Decoder (ForeignPtr DecoderT, ForeignPtr ErrorCode)
  deriving (Eq, Ord, Show)

-- | allocates and initializes a decoder state.
opusDecoderCreate :: (HasDecoderConfig cfg, MonadIO m) => cfg -> m Decoder
opusDecoderCreate cfg = liftIO $ do
  let cs = if isStereo then 2 else 1
      sr = cfg ^. (decoderConfig . samplingRate)
      isStereo = cfg ^. decoderIsStereo
  err <- mallocForeignPtr
  d <- withForeignPtr err (c_opus_decoder_create sr cs)
  d' <- newForeignPtr cp_opus_decoder_destroy d
  let enc = Decoder (d', err)
  opusLastError enc >>= maybe (pure enc) throwM



-- | Decode an Opus frame.
opusDecode
  :: (HasDecoderStreamConfig cfg, MonadIO m)
  => Decoder -- ^ 'Decoder' state
  -> cfg     -- ^ max data bytes
  -> ByteString -- ^ input signal (interleaved if 2 channels)
  -> m ByteString
opusDecode d cfg i =
  let fs = cfg ^. deStreamFrameSize
      fec = cfg ^. deStreamDecodeFec
      conf = cfg ^. deStreamDecoderConfig
      chans = if conf ^. decoderIsStereo then 2 else 1
      pcm_length = fs * chans
  in liftIO $
  BS.useAsCStringLen i $ \(i', ilen) ->
    allocaArray pcm_length $ \os ->
      runDecoderAction d $ \d' -> do
        r <- c_opus_decode d' i' (fromIntegral ilen) os
          (fromIntegral fs) (fromIntegral fec)
        let l = fromIntegral r
        if l < 0 then do
          let mbException = ErrorCode l ^? _ErrorCodeException 
          case mbException of
              Nothing -> throwM OpusInvalidPacket
              Just x  -> throwM x
        else do
          -- multiply by 2 because "os" is CShort i.e. Int16
          -- but CStringLen expects a CChar which is Int8
          BS.packCStringLen $ (castPtr os, (fromIntegral l) * 2)

opusDecodeLazy :: (HasDecoderStreamConfig cfg, MonadIO m)
  => Decoder -- ^ 'Decoder' state
  -> cfg
  -> ByteString -- ^ input signal (interleaved if 2 channels)
  -> m BL.ByteString
opusDecodeLazy d cfg = fmap BL.fromStrict . opusDecode d cfg

withOpusDecoder :: (HasDecoderConfig cfg) => MonadResource m
  => cfg
  -> (Decoder -> IO ())
  -> m Decoder
withOpusDecoder cfg a =
  snd <$> allocate (opusDecoderCreate cfg) a


-- | Frees an 'Decoder'. Is normaly called automaticly
--   when 'Decoder' gets out of scope
opusDecoderDestroy :: MonadIO m => Decoder -> m ()
opusDecoderDestroy (Decoder (d, err)) = liftIO $
  finalizeForeignPtr d >> finalizeForeignPtr err


-- | get last error from decoder
opusLastError :: MonadIO m => Decoder -> m (Maybe OpusException)
opusLastError (Decoder (_, fp)) =
  liftIO $ preview _ErrorCodeException <$> withForeignPtr fp peek

type DecoderAction  a = Ptr DecoderT -> IO a

-- | Run an 'DecoderAction'.
withDecoder' :: MonadIO m =>
  Decoder -> DecoderAction a -> m (Either OpusException a)
withDecoder' e@(Decoder (fp_a, _)) m = liftIO $
  withForeignPtr fp_a $ \a -> do
    r <- m a
    le <- opusLastError e
    pure $ maybe (Right r) Left le

-- | Run an 'DecoderAction'. Might throw an 'OpusException'
runDecoderAction :: (MonadIO m, MonadThrow m) =>
  Decoder -> DecoderAction a -> m a
runDecoderAction d m = withDecoder' d m >>= either throwM pure
