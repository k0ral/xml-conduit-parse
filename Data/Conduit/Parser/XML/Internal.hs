{-# LANGUAGE OverloadedStrings #-}
-- | Low-level primitives.
module Data.Conduit.Parser.XML.Internal (module Data.Conduit.Parser.XML.Internal) where

-- {{{ Imports
import           Control.Applicative
import           Control.Monad.Catch

import           Data.Conduit.Parser
import           Data.Map                as Map
import           Data.Text               (Text)
import           Data.XML.Types

import           Text.Parser.Combinators
-- }}}

type AttributeMap = Map Name [Content]

-- | Parse an 'EventBeginDocument'.
beginDocument :: (MonadCatch m) => ConduitParser Event m ()
beginDocument = named "XML begin document" $ do
  event <- await
  case event of
   EventBeginDocument -> return ()
   _ -> unexpected $ "Expected XML begin document, got: " ++ show event

-- | Parse an 'EventEndDocument'.
endDocument :: (MonadCatch m) => ConduitParser Event m ()
endDocument = named "XML end document" $ do
  event <- await
  case event of
   EventEndDocument -> return ()
   _ -> unexpected $ "Expected XML end document, got: " ++ show event

-- | Parse an 'EventBeginDoctype'.
beginDoctype :: (MonadCatch m) => ConduitParser Event m (Text, Maybe ExternalID)
beginDoctype = named "XML begin doctype" $ do
  event <- await
  case event of
   EventBeginDoctype doctype externalID -> return (doctype, externalID)
   _ -> unexpected $ "Expected XML begin doctype, got: " ++ show event

-- | Parse an 'EventEndDoctype'.
endDoctype :: (MonadCatch m) => ConduitParser Event m ()
endDoctype = named "XML end doctype" $ do
  event <- await
  case event of
   EventEndDoctype -> return ()
   _ -> unexpected $ "Expected XML end doctype, got: " ++ show event

-- | Parse an 'EventInstruction'.
instruction :: (MonadCatch m) => ConduitParser Event m Instruction
instruction = named "XML instruction" $ do
  event <- await
  case event of
   EventInstruction i -> return i
   _ -> unexpected $ "Expected XML instruction, got: " ++ show event

-- | Parse an 'EventBeginElement'.
beginElement :: (MonadCatch m) => ConduitParser Event m (Name, AttributeMap)
beginElement = named "XML begin element" $ do
  event <- await
  case event of
   EventBeginElement n a -> return (n, Map.fromList a)
   _ -> unexpected $ "Expected XML begin element, got: " ++ show event

-- | Parse an 'EventEndElement'.
endElement :: (MonadCatch m) => ConduitParser Event m Name
endElement = named "XML end element" $ do
  event <- await
  case event of
   EventEndElement n -> return n
   _ -> unexpected $ "Expected XML end element, got: " ++ show event

-- | Parse a 'ContentEntity' (within an 'EventContent').
contentEntity :: (MonadCatch m) => ConduitParser Event m Text
contentEntity = named "XML entity content" $ do
  event <- await
  case event of
   EventContent (ContentEntity t) -> return t
   _ -> unexpected $ "Expected XML content entity, got: " ++ show event

-- | Parse a 'ContentText' (within an 'EventContent').
contentText :: (MonadCatch m) => ConduitParser Event m Text
contentText = named "XML text content" $ do
  event <- await
  case event of
   EventContent (ContentText t) -> return t
   _ -> unexpected $ "Expected XML textual content, got: " ++ show event

-- | Parse an 'EventComment'.
comment :: (MonadCatch m) => ConduitParser Event m Text
comment = named "XML comment" $ do
  event <- await
  case event of
   EventComment t -> return t
   _              -> unexpected $ "Expected XML comment, got: " ++ show event

-- | Parse an 'EventCDATA'.
cdata :: (MonadCatch m) => ConduitParser Event m Text
cdata = named "XML CDATA" $ do
  event <- await
  case event of
   EventCDATA t -> return t
   _            -> unexpected $ "Expected XML CDATA, got: " ++ show event

-- | Parse a textual 'EventContent' or an 'EventCDATA'.
text :: (MonadCatch m) => ConduitParser Event m Text
text = mconcat <$> some (contentText <|> cdata)
