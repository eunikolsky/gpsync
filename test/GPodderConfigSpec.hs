{-# LANGUAGE QuasiQuotes #-}

module GPodderConfigSpec (spec) where

import Data.Aeson
import GPodderConfig
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "GPodderConfig" $ do
    it "is parsed from json" $
      let file =
            [r|{
              "auto": {
                "retries": 3
              },
              "check_connection": true,
              "device_sync": {
                "device_folder": "/mnt/player/podcasts/"
              }
            }|]
          expected = GPodderConfig{gcSyncTargetDir = "/mnt/player/podcasts/"}
      in eitherDecode file `shouldBe` Right expected
