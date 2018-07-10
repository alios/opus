module Codec.Audio.Opus.Types
 ( -- * Sampling Rate
   SamplingRate, opusSR8k, opusSR12k, opusSR16k, opusSR24k, opusSR48k
   -- * Coding Mode
 , CodingMode, app_voip, app_audio, app_lowdelay
   -- * Exception
 , OpusException(..), ErrorCode, errorCodeException
 ) where

import           Codec.Audio.Opus.Internal.Opus
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
