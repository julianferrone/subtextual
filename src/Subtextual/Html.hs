{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
module Subtextual.Html (renderBlock, renderDoc) where

import Subtextual.Core
import Lucid
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)

------------------------------------------------------------
--                     Inlines to HTML                    --
------------------------------------------------------------

inline :: Inline -> Html ()
inline (PlainText p) = (span_ . toHtml) p
inline (BareUrl url) = a_ [href_ url] $ toHtml url
inline (AngledUrl url) = a_ [href_ url] $ toHtml url
inline (SlashLink sl) = a_ [href_ sl, class_ "slashlink"] $ toHtml sl

inlines :: [Inline] -> Html ()
inlines = mconcat . map inline

------------------------------------------------------------
--                      Block to HTML                     --
------------------------------------------------------------

block :: Block -> Html ()
block (Paragraph paragraph) = (p_ . inlines) paragraph
block (Heading heading) = (h2_ . toHtml) heading
block (Bullet bullet) = (li_ . inlines) bullet
block (Quote quote) = (blockquote_ . inlines) quote
block Blank = mempty
block (Tag tag) = (div_ [class_ "tag"] . toHtml) tag
block (KeyValue key value) = 
    div_ 
        [class_ "keyvalue"] 
        ( div_ [class_ "key"] (toHtml key) 
            <> div_ [class_ "value"] (toHtml value)
        ) 
block (Triple subject predicate object) = 
    (div_ [class_ "triple"] . mconcat) [
            div_ [class_ "subject"] (toHtml subject),
            div_ [class_ "predicate"] (toHtml predicate),
            div_ [class_ "object"] (toHtml object)
        ]

------------------------------------------------------------
--                    Document to HTML                    --
------------------------------------------------------------

data Group a =
    Single a
    | Bullets [a]

document :: Document -> Html ()
document = mconcat . map groupHtml . group' where
    groupHtml :: Group Block -> Html ()
    groupHtml (Single b) = block b
    groupHtml (Bullets bs) = ul_ $ (mconcat . map block) bs

    group' :: Document -> [Group Block]
    group' doc = group doc []

    group :: Document -> [Group Block] -> [Group Block]
    group [] done = (reverse . map reverseGroup) done
    group (Bullet b : todo) (Bullets bs : done) = group todo $ Bullets (Bullet b : bs) : done
    group (Bullet b : todo) done = group todo $ Bullets [Bullet b] : done
    group (b : todo) done = group todo $ Single b : done

    reverseGroup :: Group a -> Group a
    reverseGroup (Single s) = Single s
    reverseGroup (Bullets bs) = Bullets $ reverse bs

----------     Subtext to HTML-formatted Text     ----------

renderBlock :: Block -> T.Text
renderBlock = toStrict . renderText . block

renderDoc :: Document -> T.Text
renderDoc = toStrict . renderText . document