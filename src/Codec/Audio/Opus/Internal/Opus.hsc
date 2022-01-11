{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.Opus.Internal.Opus where

import           Foreign
import           Foreign.C.Types
import           Foreign.C.String

#include <opus.h>

newtype ErrorCode = ErrorCode { unErrorCode :: CInt }
    deriving (Eq,Show)

instance Storable ErrorCode where
  sizeOf (ErrorCode e) = sizeOf e
  alignment (ErrorCode e) = alignment e
  peek p = ErrorCode <$> peek (castPtr p)
  poke p = poke (castPtr p) . unErrorCode

#{enum ErrorCode, ErrorCode
  , opus_ok               = OPUS_OK
  , opus_bad_arg          = OPUS_BAD_ARG
  , opus_buffer_too_small = OPUS_BUFFER_TOO_SMALL
  , opus_internal_error   = OPUS_INTERNAL_ERROR
  , opus_invalid_packet   = OPUS_INVALID_PACKET
  , opus_unimplemented    = OPUS_UNIMPLEMENTED
  , opus_invalid_state    = OPUS_INVALID_STATE
  , opus_alloc_fail       = OPUS_ALLOC_FAIL
  }


newtype CodingMode = CodingMode { unCodingMode :: CInt }
    deriving (Eq)


#{enum CodingMode, CodingMode
 , app_voip = OPUS_APPLICATION_VOIP
 , app_audio = OPUS_APPLICATION_AUDIO
 , app_lowdelay = OPUS_APPLICATION_RESTRICTED_LOWDELAY
 }

instance Show CodingMode where
  show a
    | app_voip == a = "voip coding"
    | app_audio == a = "audio coding"
    | app_lowdelay == a = "lowdelay coding"
    | otherwise = "unknown coding"

type OpusInt = Int32

newtype SamplingRate = SamplingRate { unSamplingRate :: Int }
    deriving (Eq)

-- | sampling rate 8kHz
opusSR8k :: SamplingRate
opusSR8k = SamplingRate 8000

-- | sampling rate 12kHz
opusSR12k :: SamplingRate
opusSR12k = SamplingRate 12000

-- | sampling rate 16kHz
opusSR16k :: SamplingRate
opusSR16k = SamplingRate 16000

-- | sampling rate 24kHz
opusSR24k :: SamplingRate
opusSR24k = SamplingRate 24000

-- | sampling rate 48kHz
opusSR48k :: SamplingRate
opusSR48k = SamplingRate 48000


instance Show SamplingRate where
  show (SamplingRate r) = mconcat [show $ r `div` 1000, "kHz"]

data EncoderT
data DecoderT


-- | allocates and initializes an encoder state.
foreign import ccall unsafe "opus.h opus_encoder_create"
    c_opus_encoder_create
      :: SamplingRate -- ^ sampling rate of input signal (Hz) This must be one of 8000, 12000, 16000, 24000, or 48000.
      -> Int32    -- ^ Number of channels (1 or 2) in input signal
      -> CodingMode -- ^ Coding mode. (See 'app_voip', 'app_audio', 'app_lowdelay')
      -> Ptr ErrorCode -- ^ 'ErrorCode' pointer
      -> IO (Ptr EncoderT)

-- | Frees an 'EncoderT'
foreign import ccall unsafe "opus.h &opus_encoder_destroy"
    cp_opus_encoder_destroy
      :: FunPtr (Ptr EncoderT -> IO ())


foreign import ccall unsafe "opus.h opus_encode"
    c_opus_encode
      :: Ptr EncoderT  -- ^ encoder state
      -> Ptr CShort    -- ^ input signal
      -> Int32         -- ^ frame size
      -> CString       -- ^ output payload
      -> Int32         -- ^ max data bytes
      -> IO Int32      -- ^ number of bytes written or negative in case of error

-- | allocates and initializes a decoder state.
foreign import ccall unsafe "opus.h opus_decoder_create"
    c_opus_decoder_create
      :: SamplingRate -- ^ sampling rate, same as encoder_create
      -> Int32 -- ^ Number of channels in input signal
      -> Ptr ErrorCode -- ^ 'ErrorCode' pointer
      -> IO (Ptr DecoderT)

-- | Frees a 'DecoderT'
foreign import ccall unsafe "opus.h &opus_decoder_destroy"
    cp_opus_decoder_destroy
      :: FunPtr (Ptr DecoderT -> IO ())

foreign import ccall unsafe "opus.h opus_decode"
    c_opus_decode
      :: Ptr DecoderT -- ^ Decoder state
      -> Ptr CChar    -- ^ Byte array of compressed data
      -> Int32        -- ^ Exact number of bytes in the payload
      -> Ptr CShort   -- ^ decoded audio data
      -> Int32        -- ^ max duration of the frame in samples that can fit
      -> CInt         -- ^ flag to request that any in-band forward error correction data be decoded. If no such data is available, the frame is decoded as if it were lost.
      -> IO Int32     -- ^ Number of decoded samples, or negative in case of error

