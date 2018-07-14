module Codec.Audio.Opus.Encoder.Conduit
  ( encoderC, encoderLazyC
  , encoderSink
  ) where

import           Codec.Audio.Opus.Encoder
import           Conduit
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Lazy     as BL
import           Data.Conduit.Combinators
import           Prelude                  (Bool, Int, ($))

encoderC :: MonadResource m
  => SamplingRate
  -> Bool
  -> CodingMode
  -> FrameSize
  -> Int
  -> ConduitT ByteString ByteString m ()
encoderC sr isStereo cm fs n = withEncoder sr isStereo cm $
  \e -> mapM (opusEncode e fs n)

encoderLazyC :: MonadResource m
  => SamplingRate
  -> Bool
  -> CodingMode
  -> FrameSize
  -> Int
  -> ConduitT ByteString BL.ByteString m ()
encoderLazyC sr isStereo cm fs n = withEncoder sr isStereo cm $
  \e -> mapM (opusEncodeLazy e fs n)


encoderSink ::
  MonadResource m =>
  SamplingRate
  -> Bool
  -> CodingMode
  -> FrameSize
  -> Int
  -> ConduitT ByteString o m BL.ByteString
encoderSink sr isStereo cm fs n = withEncoder sr isStereo cm $
  \e -> foldMapM (opusEncodeLazy e fs n)

withEncoder ::
  MonadResource m =>
  SamplingRate
  -> Bool
  -> CodingMode
  -> (Encoder -> ConduitT i o m r)
  -> ConduitT i o m r
withEncoder sr isStereo cm =
  bracketP (opusEncoderCreate sr isStereo cm)
  opusEncoderDestroy
