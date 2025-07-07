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
inlineHtml (Core.PlainText p) = (span_ . toHtml) p
inlineHtml (Core.BareUrl url) = a_ [href_ url] $ toHtml url
inlineHtml (Core.AngledUrl url) = a_ [href_ url] $ toHtml url
inlineHtml (Core.SlashLink dn) =
  a_
    [ href_ dn',
      class_ "slashlink"
    ]
    . toHtml
    $ dn'
  where
    dn' = unDocumentName dn

inlinesHtml :: [Core.Inline] -> Lucid.Html ()
inlinesHtml = mconcat . map inlineHtml

------------------------------------------------------------
--                      Block to HTML                     --
------------------------------------------------------------

blockHtml :: Core.Block -> Lucid.Html ()
blockHtml (Core.Paragraph paragraph) = (p_ . inlinesHtml) paragraph
blockHtml (Core.Heading heading) = (h2_ . toHtml) heading
blockHtml (Core.Bullet bullet) = (li_ . inlinesHtml) bullet
blockHtml (Core.Quote quote) = (blockquote_ . inlinesHtml) quote
blockHtml Core.Blank = mempty
blockHtml (Core.Tag tag) = (div_ [class_ "tag"] . toHtml) tag
blockHtml (Core.KeyValue key value) =
  div_
    [class_ "keyvalue"]
    ( div_ [class_ "key"] (toHtml key)
        <> div_ [class_ "value"] (toHtml value)
    )
blockHtml (Core.Triple subject predicate object) =
  (div_ [class_ "triple"] . mconcat)
    [ div_ [class_ "subject"] (toHtml subject),
      div_ [class_ "predicate"] (toHtml predicate),
      div_ [class_ "object"] (toHtml object)
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
    groupHtml (Bullets bs) = ul_ $ (mconcat . map blockHtml) bs

    group' :: [Core.Block] -> [Group Core.Block]
    group' doc = group doc []

    group :: [Core.Block] -> [Group Core.Block] -> [Group Core.Block]
    group [] done = (reverse . map reverseGroup) done
    group (Core.Bullet b : todo) (Bullets bs : done) = group todo $ Bullets (Bullet b : bs) : done
    group (Core.Bullet b : todo) done = group todo $ Bullets [Bullet b] : done
    group (b : todo) done = group todo $ Single b : done

    reverseGroup :: Group a -> Group a
    reverseGroup (Single s) = Single s
    reverseGroup (Bullets bs) = Bullets $ reverse bs

----------     Subtext to HTML-formatted Text     ----------

renderBlock :: Core.Block -> Text.Text
renderBlock = toStrict . Lucid.renderText . blockHtml

renderDoc :: [Core.Block] -> Text.Text
renderDoc = toStrict . Lucid.renderText . documentHtml