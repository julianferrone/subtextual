{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Subtextual.Html (renderBlock, renderDoc) where

import qualified Data.Text as Text
import Data.Text.Lazy (toStrict)
import qualified Lucid
import qualified Subtextual.Core as Core

------------------------------------------------------------
--                     Inlines to HTML                    --
------------------------------------------------------------

inlineHtml :: Core.Inline -> Lucid.Html ()
inlineHtml (Core.PlainText p) = (Lucid.span_ . Lucid.toHtml) p
inlineHtml (Core.BareUrl url) = Lucid.a_ [Lucid.href_ url] $ Lucid.toHtml url
inlineHtml (Core.AngledUrl url) = Lucid.a_ [Lucid.href_ url] $ Lucid.toHtml url
inlineHtml (Core.SlashLink dn) =
  Lucid.a_
    [ Lucid.href_ dn',
      Lucid.class_ "slashlink"
    ]
    . Lucid.toHtml
    $ dn'
  where
    dn' = Core.unDocumentName dn

inlinesHtml :: [Core.Inline] -> Lucid.Html ()
inlinesHtml = mconcat . map inlineHtml

------------------------------------------------------------
--                      Block to HTML                     --
------------------------------------------------------------

blockHtml :: Core.Block -> Lucid.Html ()
blockHtml (Core.Paragraph paragraph) = (Lucid.p_ . inlinesHtml) paragraph
blockHtml (Core.Heading heading) = (Lucid.h2_ . Lucid.toHtml) heading
blockHtml (Core.Bullet bullet) = (Lucid.li_ . inlinesHtml) bullet
blockHtml (Core.Quote quote) = (Lucid.blockquote_ . inlinesHtml) quote
blockHtml Core.Blank = mempty
blockHtml (Core.Tag tag) = (Lucid.div_ [Lucid.class_ "tag"] . Lucid.toHtml) tag
blockHtml (Core.KeyValue key value) =
  Lucid.div_
    [Lucid.class_ "keyvalue"]
    ( Lucid.div_ [Lucid.class_ "key"] (Lucid.toHtml key)
        <> Lucid.div_ [Lucid.class_ "value"] (Lucid.toHtml value)
    )
blockHtml (Core.Triple subject predicate object) =
  (Lucid.div_ [Lucid.class_ "triple"] . mconcat)
    [ Lucid.div_ [Lucid.class_ "subject"] (Lucid.toHtml subject),
      Lucid.div_ [Lucid.class_ "predicate"] (Lucid.toHtml predicate),
      Lucid.div_ [Lucid.class_ "object"] (Lucid.toHtml object)
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
    groupHtml (Single b) = blockHtml b
    groupHtml (Bullets bs) = Lucid.ul_ $ (mconcat . map blockHtml) bs

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
renderBlock = toStrict . Lucid.renderText . blockHtml

renderDoc :: [Core.Block] -> Text.Text
renderDoc = toStrict . Lucid.renderText . documentHtml