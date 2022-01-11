module Main(main) where

import           Codec.Audio.Opus.Encoder
import           Codec.Audio.Opus.Decoder
import           Control.Lens.Operators
import           Control.Exception
import           Control.Monad (guard, forM_)
import qualified Data.ByteString          as B
import           Data.Bits
import           Data.List
import           Data.Word (Word8)
import           System.Exit
import           System.Process
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


onlyIfOpusCompareExists :: IO () -> IO ()
onlyIfOpusCompareExists action = do
  result <- try $ readProcessWithExitCode "./opus_compare" [] ""
  case (result :: Either IOException (ExitCode, String, String)) of
    Right (ExitFailure 1, _, _) -> action
    _ -> fail "opus_compare executable not found"

decodeFile :: Decoder -> B.ByteString -> IO B.ByteString
decodeFile decoder bytes = do
  decoder <- opusDecoderCreate decoderCfg
  loop bytes
  where

    -- | Convert four unsigned bytes to a 32-bit integer. fromIntegral is
    -- applied to each byte before shifting to not lose any bits.
    charToInt :: [Word8] -> Int
    charToInt (b1:b2:b3:b4:[]) = (fromIntegral b1) `shiftL` 24 .|. (fromIntegral b2) `shiftL` 16 .|. (fromIntegral b3) `shiftL` 8 .|. (fromIntegral b4)
    charToInt _ = error "wrong length to convert to int"

    decoderCfg :: DecoderConfig
    decoderCfg = _DecoderConfig # (opusSR48k, False)

    maxPacket, maxFrameSize :: Int
    maxPacket = 1500
    maxFrameSize = 48000 * 2

    loop bytes
      | B.length bytes < 8 = pure mempty
      | otherwise = do
        let inputLen = charToInt $ B.unpack $ B.take 4 bytes
        guard $ inputLen <= maxPacket && inputLen >= 0 -- invalid payload length

        let inputEncFinalRange = charToInt $ B.unpack $ B.take 4 $ B.drop 4 bytes
        let (inputData, remaining) = B.splitAt inputLen $ B.drop 8 bytes
        guard $ inputLen == B.length inputData -- ran out of input, expecting inputLen but got B.length bytes

        guard $ inputLen /= 0 -- lost packets are not supported for now in this test

        -- assumptions: no lost packets. no inband fec.
        let outputSamples = maxFrameSize
        decoded <- opusDecode decoder (_DecoderStreamConfig # (decoderCfg, outputSamples, 0)) inputData
        (decoded <>) <$> loop remaining

main :: IO ()
main = hspec $ do
  describe "opusEncoderCreate" $
    seqWithCfgs testEncoderCreate
  around_ onlyIfOpusCompareExists $ do
    describe "opus mono test vectors" $ do
      forM_ ["01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"] $ \file -> do
        it ("Testing testvector " <> file) $ do
          decoder <- opusDecoderCreate (_DecoderConfig # (opusSR48k, False))
          B.readFile ("opus_newvectors/testvector" <> file <> ".bit") >>= decodeFile decoder >>= B.writeFile "tmp.out"
          -- Use readProcessWithExitCode to account for the fact that opus_compare
          -- returns a non-zero exit code if the comparing fails.
          (exitcode1, stdout1, error1) <- readProcessWithExitCode "./opus_compare"
            ["-r", "48000"
            , "opus_newvectors/testvector" <> file <> ".dec"
            , "tmp.out"
            ] ""
          (exitcode2, stdout2, error2) <- readProcessWithExitCode "./opus_compare"
            ["-r", "48000"
            , "opus_newvectors/testvector" <> file <> "m.dec"
            , "tmp.out"
            ] ""
          shouldSatisfy (error1, error2) $ \(a, b) -> "PASSES" `isInfixOf` a || "PASSES" `isInfixOf` b


{-

-}
{-
  let testEncoderEncodeWith = testEncoderEncode <$> srs <*> ss <*> cs
  describe "opusEncode" $ do
r <-
      sequence_ $ testEncoderEncodeWith <*> pure "empty input" <*> pure mempty >>= (`shouldSatisfy` const True)
    return ()
-}
