import Test.HUnit
import Messages
import Data.Binary (get, put)
import Data.Binary.Put
import Data.Binary.Get
import Data.ByteString.Char8 (pack)
import Tracker
import Bencode

-- Bencode.hs
bencodeTests :: Test
bencodeTests = test 
           [ -- parseInt
             parseTest "i2e" ~?= (BenInt 2)
           , parseTest "i45e" ~?= (BenInt 45)
           , parseTest "i456456345623454654356354e" ~?= (BenInt 456456345623454654356354)
           , parseTest "i-9e" ~?= (BenInt (-9))
           , parseTest "i-23e" ~?= (BenInt (-23))
           , parseTest "i+34e" ~?= (BenInt 34)
             -- parseString
           , parseTest "0:" ~?= (BenString "")
           , parseTest "1:a" ~?= (BenString "a")
           , parseTest "1:1" ~?= (BenString "1")
           , parseTest "5:hello" ~?= (BenString "hello")
           , parseTest "5:ab34c" ~?= (BenString "ab34c")
           , parseTest "5:-$@#!" ~?= (BenString "-$@#!")
           , parseTest "3:3:1" ~?= (BenString "3:1")
           , parseTest "1:xdiscarded" ~?= (BenString "x")
             -- parse List 
           , parseTest "le" ~?= (BenList [])
           , parseTest "llee" ~?= (BenList [BenList []])
           , parseTest "llleee" ~?= (BenList [BenList [BenList []]])
           , parseTest "li2e3:abce" ~?= (BenList [BenInt 2, BenString "abc"])
           , parseTest "lli2eee" ~?= (BenList [BenList [BenInt 2]])
           , parseTest "lli2eeei2e5:disca" ~?= (BenList [BenList [BenInt 2]])
             -- parse dict
           , parseTest "de" ~?= (BenDict [])
           , parseTest "d1:a1:x1:b1:ye" ~?= (BenDict [(BenString "a", BenString "x"), (BenString "b", BenString  "y")])
           ]
           where parseTest = parseOne . pack 

-- Message.hs
messageTests :: Test
messageTests = test [ f have    ~?= have
                   , f bf      ~?= bf
                   , f req     ~?= req
                   , f cancel  ~?= cancel
                   , f port    ~?= port
                   , f2 hshake ~?= hshake
                   ]
            where f      = runGet get . runPut . put
                  f2     = runGet get . runPut . put
                  have   = Have 12
                  bf     = BitField $ (take 6 $ repeat True) ++ [False, True]
                  req    = Request 0 0 10
                  cancel = Cancel 0 0 10
                  port   = Port 5881
                  hshake = HandShake "BitTorrent Protocol" peerIdHash peerIdHash

main :: IO ()
main = runTestTT bencodeTests 
     >> runTestTT messageTests
     >> return ()