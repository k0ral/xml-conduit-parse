{-# LANGUAGE OverloadedStrings #-}
-- | High-level primitives to parse a stream of XML 'Event's.
module Data.Conduit.Parser.XML
  ( -- * XML parsers
    -- ** Tags
    tag
  , tagName
  , tagPredicate
  , tagNoAttr
  , tagIgnoreAttrs
  , anyTag
    -- ** Attributes
  , AttributeMap
  , AttrParser()
  , attr
  , textAttr
  , anyAttr
  , ignoreAttrs
    -- ** Content
  , content
  , textContent
    -- * Re-exports
    -- ** Event producers
  , Reexport.parseBytes
  , Reexport.parseBytesPos
  , parseText
  , Reexport.parseTextPos
  , Reexport.detectUtf
  , Reexport.parseFile
  , Reexport.parseLBS
    -- ** Parser settings
  , Reexport.ParseSettings()
  , Reexport.DecodeEntities
  , Reexport.psDecodeEntities
  , Reexport.psRetainNamespaces
    -- ** Entity decoding
  , Reexport.decodeXmlEntities
  , Reexport.decodeHtmlEntities
    -- ** Exceptions
  , Reexport.XmlException(..)
  ) where

-- {{{ Imports
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch

import           Data.Char
import           Data.Conduit
import           Data.Conduit.Parser
import           Data.Conduit.Parser.XML.Internal
import           Data.Map                         as Map hiding (map, null)
import           Data.Text                        as Text (Text, all, unpack)
import           Data.XML.Types

import           Text.Parser.Combinators
import qualified Text.XML.Stream.Parse            as Reexport
-- }}}

-- | Parse an XML tag, depending on its name and attributes.
-- This is the most generic tag parser.
--
-- Comments, instructions and whitespace are ignored.
tag :: MonadCatch m
    => (Name -> Maybe a)               -- ^ Tag name parser.
    -> (a -> AttrParser b)             -- ^ Attributes parser. It should consume all available attributes.
    -> (b -> ConduitParser Event m c)  -- ^ Children parser. It should consume all elements between the opening and closing tags.
    -> ConduitParser Event m c
tag checkName attrParser f = do
  skipMany ignored
  (name, attributes) <- beginElement
  a <- maybe (unexpected $ "Invalid element name: " ++ show name) return $ checkName name
  b <- either (unexpected . show) return $ runAttrParser' (attrParser a) attributes
  result <- f b
  skipMany ignored
  endName <- endElement
  when (endName /= name) . unexpected $ "Invalid closing tag: expected </" ++ unpack (nameLocalName name) ++ ">, got </" ++ unpack (nameLocalName endName) ++ ">"
  return result

  where ignored = beginDocument <|> endDocument <|> void beginDoctype <|> void endDoctype <|> void instruction <|> void comment <|> spaceContent
        spaceContent :: (MonadCatch m) => ConduitParser Event m ()
        spaceContent = do
          t <- contentText
          unless (Text.all isSpace t) . unexpected $ "Unexpected textual content: " ++ unpack t

        runAttrParser' parser attributes = case runAttrParser parser attributes of
          Left e -> Left e
          Right (a, x) -> if null a then Right x else Left . toException $ Reexport.UnparsedAttributes (Map.toList a)

-- | Like 'tag', but use a predicate to select tag names.
tagPredicate :: MonadCatch m => (Name -> Bool) -> AttrParser a -> (a -> ConduitParser Event m b) -> ConduitParser Event m b
tagPredicate p attrParser = tag (guard . p) (const attrParser)

-- | Like 'tag', but match a single tag name.
tagName :: MonadCatch m => Name -> AttrParser a -> (a -> ConduitParser Event m b) -> ConduitParser Event m b
tagName name = tagPredicate (== name)

-- | Like 'tagName', but expect no attributes at all.
tagNoAttr :: MonadCatch m => Name -> ConduitParser Event m a -> ConduitParser Event m a
tagNoAttr name f = tagName name (return ()) $ const f

-- | Like 'tagName', but ignore all attributes.
tagIgnoreAttrs :: MonadCatch m => Name -> ConduitParser Event m a -> ConduitParser Event m a
tagIgnoreAttrs name f = tagName name ignoreAttrs $ const f

-- | Parse an XML tag, whatever its name and attributes.
--
-- Comments, instructions and whitespace are ignored.
anyTag :: MonadCatch m => (Name -> [(Name, [Content])] -> ConduitParser Event m a) -> ConduitParser Event m a
anyTag handler = tag Just (\name -> (,) name <$> many anyAttr) (uncurry handler)

-- | Parse a tag content as 'Text'.
--
-- This parser fails if the tag is empty.
-- To get 'mempty' instead of failing, use @textContent \<|\> mempty@.
textContent :: MonadCatch m => ConduitParser Event m Text
textContent = do
  skipMany ignored
  mconcat <$> sepEndBy1 text ignored
  where ignored = beginDocument <|> endDocument <|> void beginDoctype <|> endDoctype <|> void instruction <|> void comment

-- | Parse a tag content using a custom parsing function.
content :: MonadCatch m => (Text -> Maybe a) -> ConduitParser Event m a
content parse = maybe (unexpected "Invalid content.") return . parse =<< textContent

newtype AttrParser a = AttrParser { runAttrParser :: AttributeMap -> Either SomeException (AttributeMap, a) }

instance Monad AttrParser where
  return a = AttrParser $ \attributes -> Right (attributes, a)
  (AttrParser p) >>= f = AttrParser $ p >=> (\(attributes', a) -> runAttrParser (f a) attributes')

instance Functor AttrParser where
  fmap = liftM

instance Applicative AttrParser where
  pure = return
  (<*>) = ap

-- | Attribute parsers can be combined with ('<|>'), 'some', 'many', 'optional', 'choice', etc.
instance Alternative AttrParser where
  empty = AttrParser $ const $ Left $ toException $ Reexport.XmlException "AttrParser.empty" Nothing
  AttrParser f <|> AttrParser g = AttrParser $ \x -> either (const $ g x) Right (f x)

instance MonadThrow AttrParser where
  throwM = AttrParser . const . throwM

-- | Parse a single textual attribute.
textAttr :: Name -> AttrParser Text
textAttr name = AttrParser $ \attrs -> maybe raiseError (returnValue attrs) (Map.lookup name attrs)
  where raiseError = Left . toException $ Reexport.XmlException ("Missing attribute: " ++ show name) Nothing
        returnValue attrs contents = Right (Map.delete name attrs, contentsToText contents)

-- | Parse a single attribute using a specific name and a custom parsing function for its value.
attr :: Name -> (Text -> Maybe a) -> AttrParser a
attr name fvalue = do
  value <- textAttr name
  maybe (throwM $ Reexport.XmlException ("Invalid attribute: " ++ show name) Nothing) return (fvalue value)

-- | Parse a single attribute, whatever its name or value.
anyAttr :: AttrParser (Name, [Content])
anyAttr = AttrParser $ \attrs -> case keys attrs of
  k:_ -> Right (Map.delete k attrs, (k, findWithDefault mempty k attrs))
  _ -> Left . toException $ Reexport.XmlException "Expecting one more attribute." Nothing

-- | Consume all remaining unparsed attributes.
ignoreAttrs :: AttrParser ()
ignoreAttrs = AttrParser . const $ Right (mempty, ())

contentsToText :: [Content] -> Text
contentsToText =
    mconcat . map toText
  where
    toText (ContentText t)   = t
    toText (ContentEntity e) = mconcat ["&", e, ";"]

-- | Alias for 'Reexport.parseText''
parseText :: (MonadThrow m) => Reexport.ParseSettings -> Conduit Text m Event
parseText = Reexport.parseText'
