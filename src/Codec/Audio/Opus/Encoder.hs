{-# LANGUAGE FlexibleContexts #-}

module Codec.Audio.Opus.Encoder
  ( -- * Encoder
    Encoder, OpusException(..)
    -- ** create
  , withOpusEncoder, opusEncoderCreate, opusEncoderDestroy
    -- ** run
  , opusEncode, opusEncodeLazy
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

-- | Encoder State
newtype Encoder = Encoder (ForeignPtr EncoderT, ForeignPtr ErrorCode)
  deriving (Eq, Ord, Show)

-- | allocates and initializes an encoder state.
opusEncoderCreate :: (HasEncoderConfig cfg, MonadIO m) => cfg -> m Encoder
opusEncoderCreate cfg = liftIO $ do
  let cs = if isStereo then 2 else 1
      sr = cfg ^. (encoderConfig . samplingRate)
      isStereo = cfg ^. encoderIsStereo
      cm = cfg ^. (encoderConfig . codingMode)
  err <- mallocForeignPtr
  e <- withForeignPtr err (c_opus_encoder_create sr cs cm)
  e' <- newForeignPtr cp_opus_encoder_destroy e
  let enc = Encoder (e', err)
  opusLastError enc >>= maybe (pure enc) throwM



-- | Encode an Opus frame.
opusEncode
  :: (HasStreamConfig cfg, MonadIO m)
  => Encoder -- ^ 'Encoder' state
  -> cfg     -- ^ max data bytes
  -> ByteString -- ^ input signal (interleaved if 2 channels)
  -> m ByteString
opusEncode e cfg i =
  let fs = cfg ^. streamFrameSize
      n = cfg ^. streamOutSize
  in liftIO $
  BS.useAsCStringLen i $ \(i',_) -> do
    print $ "cstring made from bs"
    allocaArray n $ \os -> do
      print $ "allocated array of size " <> (show n)
      runEncoderAction e $ \e' -> do
        print "encoder is in hand"
        r <- c_opus_encode e' (castPtr i') (fromInteger . toInteger $ fs) os
          (fromInteger . toInteger $ n)
        print "encoded"
        let l = fromInteger . toInteger $ r
            ol = (os, l)
        if l < 0 then throwM OpusInvalidPacket else
          BS.packCStringLen ol

opusEncodeLazy :: (HasStreamConfig cfg, MonadIO m)
  => Encoder -- ^ 'Encoder' state
  -> cfg
  -> ByteString -- ^ input signal (interleaved if 2 channels)
  -> m BL.ByteString
opusEncodeLazy e cfg = fmap BL.fromStrict . opusEncode e cfg

withOpusEncoder :: (HasEncoderConfig cfg) => MonadResource m
  => cfg
  -> (Encoder -> IO ())
  -> m Encoder
withOpusEncoder cfg a =
  snd <$> allocate (opusEncoderCreate cfg) a


-- | Frees an 'Encoder'. Is normaly called automaticly
--   when 'Encoder' gets out of scope
opusEncoderDestroy :: MonadIO m => Encoder -> m ()
opusEncoderDestroy (Encoder (e, err)) = liftIO $
  finalizeForeignPtr e >> finalizeForeignPtr err


-- | get last error from encoder
opusLastError :: MonadIO m => Encoder -> m (Maybe OpusException)
opusLastError (Encoder (_, fp)) =
  liftIO $ preview _ErrorCodeException <$> withForeignPtr fp peek

type EncoderAction  a = Ptr EncoderT -> IO a

-- | Run an 'EncoderAction'.
withEncoder' :: MonadIO m =>
  Encoder -> EncoderAction a -> m (Either OpusException a)
withEncoder' e@(Encoder (fp_a, _)) m = liftIO $
  withForeignPtr fp_a $ \a -> do
    r <- m a
    le <- opusLastError e
    pure $ maybe (Right r) Left le

-- | Run an 'EncoderAction'. Might throw an 'OpusException'
runEncoderAction :: (MonadIO m, MonadThrow m) =>
  Encoder -> EncoderAction a -> m a
runEncoderAction e m = withEncoder' e m >>= either throwM pure
