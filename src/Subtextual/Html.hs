{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Subtextual.Html (renderBlock, renderDoc) where

import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Lucid
import Subtextual.Core

------------------------------------------------------------
--                     Inlines to HTML                    --
------------------------------------------------------------

inlineHtml :: Inline -> Html ()
inlineHtml (PlainText p) = (span_ . toHtml) p
inlineHtml (BareUrl url) = a_ [href_ url] $ toHtml url
inlineHtml (AngledUrl url) = a_ [href_ url] $ toHtml url
inlineHtml (SlashLink dn) =
  a_
    [ href_ dn',
      class_ "slashlink"
    ]
    . toHtml
    $ dn'
  where
    dn' = unDocumentName dn

inlinesHtml :: [Inline] -> Html ()
inlinesHtml = mconcat . map inlineHtml

------------------------------------------------------------
--                      Block to HTML                     --
------------------------------------------------------------

blockHtml :: Block -> Html ()
blockHtml (Paragraph paragraph) = (p_ . inlinesHtml) paragraph
blockHtml (Heading heading) = (h2_ . toHtml) heading
blockHtml (Bullet bullet) = (li_ . inlinesHtml) bullet
blockHtml (Quote quote) = (blockquote_ . inlinesHtml) quote
blockHtml Blank = mempty
blockHtml (Tag tag) = (div_ [class_ "tag"] . toHtml) tag
blockHtml (KeyValue key value) =
  div_
    [class_ "keyvalue"]
    ( div_ [class_ "key"] (toHtml key)
        <> div_ [class_ "value"] (toHtml value)
    )
blockHtml (Triple subject predicate object) =
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

documentHtml :: [Block] -> Html ()
documentHtml = mconcat . map groupHtml . group'
  where
    groupHtml :: Group Block -> Html ()
    groupHtml (Single b) = blockHtml b
    groupHtml (Bullets bs) = ul_ $ (mconcat . map blockHtml) bs

    group' :: [Block] -> [Group Block]
    group' doc = group doc []

    group :: [Block] -> [Group Block] -> [Group Block]
    group [] done = (reverse . map reverseGroup) done
    group (Bullet b : todo) (Bullets bs : done) = group todo $ Bullets (Bullet b : bs) : done
    group (Bullet b : todo) done = group todo $ Bullets [Bullet b] : done
    group (b : todo) done = group todo $ Single b : done

    reverseGroup :: Group a -> Group a
    reverseGroup (Single s) = Single s
    reverseGroup (Bullets bs) = Bullets $ reverse bs

----------     Subtext to HTML-formatted Text     ----------

renderBlock :: Block -> T.Text
renderBlock = toStrict . renderText . blockHtml

renderDoc :: [Block] -> T.Text
renderDoc = toStrict . renderText . documentHtml