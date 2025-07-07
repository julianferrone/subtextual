{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Subtextual.Html (renderBlock, renderDoc) where

import qualified Data.Text as Text
import Data.Text.Lazy (toStrict)
import Lucid (ToHtml (toHtml))
import qualified Lucid
import qualified Subtextual.Core as Core

------------------------------------------------------------
--                     Inlines to HTML                    --
------------------------------------------------------------

newtype SubtextHtml a = SubtextHtml a

subtextHtml :: a -> SubtextHtml a
subtextHtml = SubtextHtml

unSubtextHtml :: SubtextHtml a -> a
unSubtextHtml (SubtextHtml x) = x

instance Lucid.ToHtml (SubtextHtml Core.DocumentName) where
  toHtml = Lucid.toHtml . Core.unDocumentName . unSubtextHtml
  toHtmlRaw = Lucid.toHtmlRaw . Core.unDocumentName . unSubtextHtml

instance Lucid.ToHtml (SubtextHtml Core.Inline) where
  toHtml (SubtextHtml (Core.PlainText p)) = (Lucid.span_ . Lucid.toHtml) p
  toHtml (SubtextHtml (Core.BareUrl url)) = Lucid.a_ [Lucid.href_ url] $ Lucid.toHtml url
  toHtml (SubtextHtml (Core.AngledUrl url)) = Lucid.a_ [Lucid.href_ url] $ Lucid.toHtml url
  toHtml (SubtextHtml (Core.SlashLink dn)) =
    Lucid.a_
      [ Lucid.href_ . Core.unDocumentName $ dn,
        Lucid.class_ "slashlink"
      ]
      . Lucid.toHtml . subtextHtml
      $ dn

  toHtmlRaw (SubtextHtml (Core.PlainText p)) = (Lucid.span_ . Lucid.toHtmlRaw) p
  toHtmlRaw (SubtextHtml (Core.BareUrl url)) = Lucid.a_ [Lucid.href_ url] $ Lucid.toHtmlRaw url
  toHtmlRaw (SubtextHtml (Core.AngledUrl url)) = Lucid.a_ [Lucid.href_ url] $ Lucid.toHtmlRaw url
  toHtmlRaw (SubtextHtml (Core.SlashLink dn)) =
    Lucid.a_
      [ Lucid.href_ . Core.unDocumentName $ dn,
        Lucid.class_ "slashlink"
      ]
      . Lucid.toHtmlRaw . subtextHtml
      $ dn

instance Lucid.ToHtml (SubtextHtml [Core.Inline]) where
  toHtml (SubtextHtml is) = mconcat . map (Lucid.toHtml . subtextHtml) $ is
  toHtmlRaw (SubtextHtml is) = mconcat . map (Lucid.toHtmlRaw . subtextHtml) $ is

------------------------------------------------------------
--                      Block to HTML                     --
------------------------------------------------------------

instance Lucid.ToHtml (SubtextHtml Core.Block) where
  toHtml (SubtextHtml (Core.Paragraph paragraph)) = (Lucid.p_ . Lucid.toHtml . subtextHtml) paragraph
  toHtml (SubtextHtml (Core.Heading heading)) = (Lucid.h2_ . Lucid.toHtml) heading
  toHtml (SubtextHtml (Core.Bullet bullet)) = (Lucid.li_ . Lucid.toHtml . subtextHtml) bullet
  toHtml (SubtextHtml (Core.Quote quote)) = (Lucid.blockquote_ . Lucid.toHtml . subtextHtml) quote
  toHtml (SubtextHtml Core.Blank) = mempty
  toHtml (SubtextHtml (Core.Tag tag)) = (Lucid.div_ [Lucid.class_ "tag"] . Lucid.toHtml) tag
  toHtml (SubtextHtml (Core.KeyValue key value)) =
    Lucid.div_
      [Lucid.class_ "keyvalue"]
      ( Lucid.div_ [Lucid.class_ "key"] (Lucid.toHtml key)
          <> Lucid.div_ [Lucid.class_ "value"] (Lucid.toHtml value)
      )
  toHtml (SubtextHtml (Core.Triple subject predicate object)) =
    (Lucid.div_ [Lucid.class_ "triple"] . mconcat)
      [ Lucid.div_ [Lucid.class_ "subject"] (Lucid.toHtml subject),
        Lucid.div_ [Lucid.class_ "predicate"] (Lucid.toHtml predicate),
        Lucid.div_ [Lucid.class_ "object"] (Lucid.toHtml object)
      ]

  toHtmlRaw (SubtextHtml (Core.Paragraph paragraph)) = (Lucid.p_ . Lucid.toHtmlRaw . subtextHtml) paragraph
  toHtmlRaw (SubtextHtml (Core.Heading heading)) = (Lucid.h2_ . Lucid.toHtmlRaw) heading
  toHtmlRaw (SubtextHtml (Core.Bullet bullet)) = (Lucid.li_ . Lucid.toHtmlRaw . subtextHtml) bullet
  toHtmlRaw (SubtextHtml (Core.Quote quote)) = (Lucid.blockquote_ . Lucid.toHtmlRaw . subtextHtml) quote
  toHtmlRaw (SubtextHtml Core.Blank) = mempty
  toHtmlRaw (SubtextHtml (Core.Tag tag)) = (Lucid.div_ [Lucid.class_ "tag"] . Lucid.toHtmlRaw) tag
  toHtmlRaw (SubtextHtml (Core.KeyValue key value)) =
    Lucid.div_
      [Lucid.class_ "keyvalue"]
      ( Lucid.div_ [Lucid.class_ "key"] (Lucid.toHtmlRaw key)
          <> Lucid.div_ [Lucid.class_ "value"] (Lucid.toHtmlRaw value)
      )
  toHtmlRaw (SubtextHtml (Core.Triple subject predicate object)) =
    (Lucid.div_ [Lucid.class_ "triple"] . mconcat)
      [ Lucid.div_ [Lucid.class_ "subject"] (Lucid.toHtmlRaw subject),
        Lucid.div_ [Lucid.class_ "predicate"] (Lucid.toHtmlRaw predicate),
        Lucid.div_ [Lucid.class_ "object"] (Lucid.toHtmlRaw object)
      ]

------------------------------------------------------------
--                    Document to HTML                    --
------------------------------------------------------------

data Group a
  = Single a
  | Bullets [a]

documentHtml :: [Core.Block] -> Lucid.Html ()
documentHtml = mconcat . map groupHtml . group'
  where
    groupHtml :: Group Core.Block -> Lucid.Html ()
    groupHtml (Single b) = Lucid.toHtml . subtextHtml $ b
    groupHtml (Bullets bs) = Lucid.ul_ $ (mconcat . map (Lucid.toHtml . subtextHtml)) bs

    group' :: [Core.Block] -> [Group Core.Block]
    group' doc = group doc []

    group :: [Core.Block] -> [Group Core.Block] -> [Group Core.Block]
    group [] done = (reverse . map reverseGroup) done
    group (Core.Bullet b : todo) (Bullets bs : done) = group todo $ Bullets (Core.Bullet b : bs) : done
    group (Core.Bullet b : todo) done = group todo $ Bullets [Core.Bullet b] : done
    group (b : todo) done = group todo $ Single b : done

    reverseGroup :: Group a -> Group a
    reverseGroup (Single s) = Single s
    reverseGroup (Bullets bs) = Bullets $ reverse bs

----------     Subtext to HTML-formatted Text     ----------

renderBlock :: Core.Block -> Text.Text
renderBlock = toStrict . Lucid.renderText . toHtml . subtextHtml

renderDoc :: [Core.Block] -> Text.Text
renderDoc = toStrict . Lucid.renderText . documentHtml