{-# LANGUAGE TemplateHaskell #-}

module Codec.Audio.Opus.Types
 ( -- * Sampling Rate
   SamplingRate, HasSamplingRate(..)
 , opusSR8k, opusSR12k, opusSR16k, opusSR24k, opusSR48k
   -- * Coding Mode
 , CodingMode, HasCodingMode(..), app_voip, app_audio, app_lowdelay
   -- * Exception
 , OpusException(..), ErrorCode, _ErrorCodeException
   -- * EncoderConfig
 , FrameSize
 , EncoderConfig, HasEncoderConfig(..), _EncoderConfig
   -- * StreamConfig
 , StreamConfig, HasStreamConfig(..), _StreamConfig
 ) where

import           Codec.Audio.Opus.Internal.Opus

import           Control.Lens.Prism
import           Control.Lens.TH
import           Control.Monad.Catch
import           Data.Typeable                  (Typeable)


data OpusException
  = OpusBadArg
  | OpusBufferToSmall
  | OpusInternalError
  | OpusInvalidPacket
  | OpusUnimplemented
  | OpusInvalidState
  | OpusAllocFail
  deriving (Eq, Show, Typeable)

instance Exception OpusException

_ErrorCodeException :: Prism' ErrorCode OpusException
_ErrorCodeException = prism' errorCodeException' errorCodeException


errorCodeException' :: OpusException -> ErrorCode
errorCodeException' OpusBadArg        = opus_bad_arg
errorCodeException' OpusBufferToSmall = opus_buffer_too_small
errorCodeException' OpusInternalError = opus_internal_error
errorCodeException' OpusInvalidPacket = opus_invalid_packet
errorCodeException' OpusUnimplemented = opus_unimplemented
errorCodeException' OpusInvalidState  = opus_invalid_state
errorCodeException' OpusAllocFail     = opus_alloc_fail


errorCodeException :: ErrorCode -> Maybe OpusException
errorCodeException a
  | a == opus_bad_arg = Just OpusBadArg
  | a == opus_buffer_too_small = Just OpusBufferToSmall
  | a == opus_internal_error = Just OpusInternalError
  | a == opus_invalid_packet = Just OpusInvalidPacket
  | a == opus_unimplemented = Just OpusUnimplemented
  | a == opus_invalid_state = Just OpusInvalidState
  | a == opus_alloc_fail = Just OpusAllocFail
  | otherwise = Nothing


makeClassy ''SamplingRate
makeClassy ''CodingMode

data EncoderConfig = EncoderConfig
  { _encoderSamplingRate :: SamplingRate  -- ^ sampling rate of input signal
  , _encoderIsStereo     :: Bool -- ^ stereo mode? ('True' => 2 channels, 'False' => 1 channel)
  , _encoderCodingMode   :: CodingMode    -- ^ Coding mode. (See 'app_voip', 'app_audio', 'app_lowdelay')
  } deriving (Eq, Show)

makeClassy 'EncoderConfig
makePrisms 'EncoderConfig

instance HasSamplingRate EncoderConfig where
  samplingRate = encoderSamplingRate

instance HasCodingMode EncoderConfig where
  codingMode = encoderCodingMode

type FrameSize = Int


data StreamConfig = StreamConfig
  { _streamEncoderConfig :: EncoderConfig
  , _streamFrameSize     :: FrameSize
  , _streamOutSize       :: Int
  } deriving (Eq, Show)

makeClassy ''StreamConfig
makePrisms ''StreamConfig

instance HasEncoderConfig StreamConfig where
  encoderConfig = streamEncoderConfig

instance HasSamplingRate StreamConfig where
  samplingRate = encoderConfig . samplingRate

instance HasCodingMode StreamConfig where
  codingMode = encoderConfig . codingMode
