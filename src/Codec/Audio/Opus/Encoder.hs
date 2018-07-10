{-# LANGUAGE FlexibleContexts #-}

module Codec.Audio.Opus.Encoder
  ( -- * Encoder
    Encoder, OpusException(..)
    -- ** create
  , opusEncoderCreate, opusEncoderDestroy
    -- ** run
  , EncoderAction, runEncoderAction
    -- * re-exports
  , module Codec.Audio.Opus.Types
  ) where

import           Codec.Audio.Opus.Internal.Opus
import           Codec.Audio.Opus.Types
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Foreign

newtype Encoder = Encoder (ForeignPtr EncoderT, ForeignPtr ErrorCode)
  deriving (Eq, Ord, Show)



-- | get last error from encoder
opusLastError :: MonadIO m => Encoder -> m (Maybe OpusException)
opusLastError (Encoder (_, fp)) =
  liftIO $ errorCodeException <$> withForeignPtr fp peek

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


-- | allocates and initializes an encoder state.
opusEncoderCreate
  :: (MonadIO m)
  => SamplingRate  -- ^ sampling rate of input signal
  -> Bool          -- ^ stereo mode? ('True' => 2 channels, 'False' => 1 channel)
  -> CodingMode    -- ^ Coding mode. (See 'app_voip', 'app_audio', 'app_lowdelay')
  -> m Encoder
opusEncoderCreate sr isStereo cm = liftIO $ do
  let cs = if isStereo then 2 else 1
  err <- mallocForeignPtr
  e <- withForeignPtr err (c_opus_encoder_create sr cs cm)
  e' <- newForeignPtr cp_opus_encoder_destroy e
  let enc = Encoder (e', err)
  opusLastError enc >>= maybe (pure enc) throwM


-- | Frees an 'Encoder'. Is normaly called automaticly
--   when 'Encoder' gets out of scope
opusEncoderDestroy :: MonadIO m => Encoder -> m ()
opusEncoderDestroy (Encoder (e, err)) = liftIO $
  finalizeForeignPtr e >> finalizeForeignPtr err
