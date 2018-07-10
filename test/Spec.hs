module Main(main) where

import           Codec.Audio.Opus.Encoder
import           Test.Hspec


srs = [opusSR48k, opusSR24k, opusSR16k, opusSR12k, opusSR8k ]
ss = [True, False]
cs = [app_voip, app_audio, app_lowdelay]

testEncoder sr s a =
  let n = mconcat
        [ "create valid ", show sr, " "
        , if s then "stereo" else "mono", " "
        , show a, " encoder"]
  in it n $
     opusEncoderCreate sr s a >>= (`shouldSatisfy` (const True))

main :: IO ()
main = hspec $ do
  describe "opusEncoderCreate" $ do
    sequence_ $ testEncoder <$> srs <*> ss <*> cs
