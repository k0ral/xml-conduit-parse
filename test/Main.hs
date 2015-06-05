{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Control.Monad.Trans.Resource

import           Data.Conduit
import           Data.Conduit.List            (sourceList)
import           Data.Conduit.Parser
import           Data.Conduit.Parser.XML
import           Data.Default
import           Data.Foldable

import qualified Language.Haskell.HLint       as HLint (hlint)
import           Test.Tasty
import           Test.Tasty.HUnit
-- import           Test.Tasty.QuickCheck


main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ unitTests
  -- , properties
  , hlint
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ combinatorsCase
  , choiceCase
  , manyCase
  , orCase
  ]


combinatorsCase :: TestTree
combinatorsCase = testCase "Combinators" $ do
  (world, c) <- runResourceT . runConduit $ sourceList input =$= parseText def =$= runConduitParser parser
  world @?= "true"
  c @?= "combine <all> &content"
  where input =
          [ "<?xml version='1.0'?>"
          , "<!DOCTYPE foo []>\n"
          , "<hello world='true'>"
          , "<?this should be ignored?>"
          , "<child1 xmlns='mynamespace'/>"
          , "<!-- this should be ignored -->"
          , "<child2>   </child2>"
          , "<child3>combine &lt;all&gt; <![CDATA[&content]]></child3>\n"
          , "</hello>"
          ]
        parser = tagName "hello" (textAttr "world") $ \world -> do
          tagNoAttr "{mynamespace}child1" $ return ()
          tagNoAttr "child2" $ return ()
          x <- tagNoAttr "child3" textContent
          return (world, x)


choiceCase :: TestTree
choiceCase = testCase "Choice" $ do
  x <- runResourceT . runConduit $ sourceList input =$= parseText def =$= runConduitParser parser
  x @?= (2 :: Int)
  where input =
          [ "<?xml version='1.0'?>"
          , "<!DOCTYPE foo []>\n"
          , "<hello>"
          , "<success/>"
          , "</hello>"
          ]
        parser = tagNoAttr "hello" $ asum
          [ tagNoAttr "failure" $ return 1
          , tagNoAttr "success" $ return 2
          ]


manyCase :: TestTree
manyCase = testCase "Many" $ do
  x <- runResourceT . runConduit $ sourceList input =$= parseText def =$= runConduitParser parser
  length x @?= 5
  where input =
          [ "<?xml version='1.0'?>"
          , "<!DOCTYPE foo []>\n"
          , "<hello>"
          , "<success/>"
          , "<success/>"
          , "<success/>"
          , "<success/>"
          , "<success/>"
          , "</hello>"
          ]
        parser = tagNoAttr "hello" . many . tagNoAttr "success" $ return ()

orCase :: TestTree
orCase = testCase "<|>" $ do
  x <- runResourceT . runConduit $ sourceList input =$= parseText def =$= runConduitParser parser
  x @?= (2 :: Int)
  where input =
          [ "<?xml version='1.0'?>"
          , "<!DOCTYPE foo []>\n"
          , "<hello>"
          , "<success/>"
          , "</hello>"
          ]
        parser = tagNoAttr "hello" $ tagNoAttr "failure" (return 1) <|> tagNoAttr "success" (return 2)


hlint :: TestTree
hlint = testCase "HLint check" $ do
  result <- HLint.hlint [ "test/", "Data/" ]
  null result @?= True
