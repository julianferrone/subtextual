module Subtextual.Html () where
{-# LANGUAGE ExtendedDefaultRules #-}

import Subtextual.Core
import Lucid

------------------------------------------------------------
--                     Inlines to HTML                    --
------------------------------------------------------------

inline :: Inline -> Html ()
inline (PlainText p) = (span_ . toHtml) p
inline (BareUrl url) = a_ [href_ url] $ toHtml url
inline (AngledUrl url) = a_ [href_ url] $ toHtml url

inlines :: [Inline] -> Html ()
inlines = mconcat . map inline

------------------------------------------------------------
--                      Block to HTML                     --
------------------------------------------------------------

block :: Block -> Html ()
block (Paragraph p) = (p_ . inlines) p
block (Heading h) = (h2_ . toHtml) h
block (Bullet b) = (li_ . inlines) b
block (Quote q) = (blockquote_ . inlines) q
block Blank = mempty