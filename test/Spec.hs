module Main(main) where

import           Codec.Audio.Opus.Encoder
import           Control.Lens.Operators
-- import qualified Data.ByteString          as BS
import           Test.Hspec


cfgs :: [EncoderConfig]
cfgs = [_EncoderConfig # (sr, s, c) | sr <- srs, s <- ss, c <- cs]
  where
    srs = [opusSR48k, opusSR24k, opusSR16k, opusSR12k, opusSR8k ]
    ss = [True, False]
    cs = [app_voip, app_audio, app_lowdelay]

seqWithCfgs :: Monad m => (EncoderConfig -> m a) -> m ()
seqWithCfgs a = sequence_ (a <$> cfgs)

testEncoderCreate :: HasEncoderConfig cfg => cfg -> SpecWith ()
testEncoderCreate cfg =
  let n = mconcat [ "create valid ", show $ cfg ^. encoderConfig, " encoder"]
  in it n $
     opusEncoderCreate cfg >>= (`shouldSatisfy` const True)



main :: IO ()
main = hspec $ do
  describe "opusEncoderCreate" $
    seqWithCfgs testEncoderCreate


{-

-}
{-
  let testEncoderEncodeWith = testEncoderEncode <$> srs <*> ss <*> cs
  describe "opusEncode" $ do
r <-
      sequence_ $ testEncoderEncodeWith <*> pure "empty input" <*> pure mempty >>= (`shouldSatisfy` const True)
    return ()
-}
