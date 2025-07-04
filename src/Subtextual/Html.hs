{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Subtextual.Html (renderABlock, renderDoc) where

import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Lucid
import Subtextual.Core

------------------------------------------------------------
--                     Inlines to HTML                    --
------------------------------------------------------------

inline :: Inline -> Html ()
inline (PlainText p) = (span_ . toHtml) p
inline (BareUrl url) = a_ [href_ url] $ toHtml url
inline (AngledUrl url) = a_ [href_ url] $ toHtml url
inline (SlashLink (DocumentName dn)) = a_ [href_ dn, class_ "slashlink"] $ toHtml dn

inlines :: [Inline] -> Html ()
inlines = mconcat . map inline

------------------------------------------------------------
--                      AuthorBlock to HTML                     --
------------------------------------------------------------

block :: AuthorBlock -> Html ()
block (AParagraph paragraph) = (p_ . inlines) paragraph
block (AHeading heading) = (h2_ . toHtml) heading
block (ABullet bullet) = (li_ . inlines) bullet
block (AQuote quote) = (blockquote_ . inlines) quote
block ABlank = mempty
block (ATag tag) = (div_ [class_ "tag"] . toHtml) tag
block (AKeyValue key value) =
  div_
    [class_ "keyvalue"]
    ( div_ [class_ "key"] (toHtml key)
        <> div_ [class_ "value"] (toHtml value)
    )
block (ATriple subject predicate object) =
  (div_ [class_ "triple"] . mconcat)
    [ div_ [class_ "subject"] (toHtml subject),
      div_ [class_ "predicate"] (toHtml predicate),
      div_ [class_ "object"] (toHtml object)
    ]

------------------------------------------------------------
--                    AuthorDocument to HTML                    --
------------------------------------------------------------

data Group a
  = Single a
  | ABullets [a]

document :: AuthorDocument -> Html ()
document = mconcat . map groupHtml . group'
  where
    groupHtml :: Group AuthorBlock -> Html ()
    groupHtml (Single b) = block b
    groupHtml (ABullets bs) = ul_ $ (mconcat . map block) bs

    group' :: AuthorDocument -> [Group AuthorBlock]
    group' doc = group doc []

    group :: AuthorDocument -> [Group AuthorBlock] -> [Group AuthorBlock]
    group [] done = (reverse . map reverseGroup) done
    group (ABullet b : todo) (ABullets bs : done) = group todo $ ABullets (ABullet b : bs) : done
    group (ABullet b : todo) done = group todo $ ABullets [ABullet b] : done
    group (b : todo) done = group todo $ Single b : done

    reverseGroup :: Group a -> Group a
    reverseGroup (Single s) = Single s
    reverseGroup (ABullets bs) = ABullets $ reverse bs

----------     Subtext to HTML-formatted Text     ----------

renderABlock :: AuthorBlock -> T.Text
renderABlock = toStrict . renderText . block

renderDoc :: AuthorDocument -> T.Text
renderDoc = toStrict . renderText . document