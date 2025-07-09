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
      . Lucid.toHtml
      . subtextHtml
      $ dn

  toHtmlRaw (SubtextHtml (Core.PlainText p)) = (Lucid.span_ . Lucid.toHtmlRaw) p
  toHtmlRaw (SubtextHtml (Core.BareUrl url)) = Lucid.a_ [Lucid.href_ url] $ Lucid.toHtmlRaw url
  toHtmlRaw (SubtextHtml (Core.AngledUrl url)) = Lucid.a_ [Lucid.href_ url] $ Lucid.toHtmlRaw url
  toHtmlRaw (SubtextHtml (Core.SlashLink dn)) =
    Lucid.a_
      [ Lucid.href_ . Core.unDocumentName $ dn,
        Lucid.class_ "slashlink"
      ]
      . Lucid.toHtmlRaw
      . subtextHtml
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
--                    Resolved to Html                    --
------------------------------------------------------------

instance Lucid.ToHtml (SubtextHtml Core.Resolved) where
  toHtml (SubtextHtml (Core.Present b)) = Lucid.toHtml . subtextHtml $ b
  toHtml (SubtextHtml (Core.ResourceNotFound dn)) =
    Lucid.div_
      [Lucid.class_ "resourceNotFound"]
      ( (Lucid.toHtml . Text.pack) "Could not resolve resource: "
          <> (Lucid.toHtml . subtextHtml) dn
      )
  toHtml (SubtextHtml (Core.HeadingNotFound dn h)) =
    Lucid.div_
      [Lucid.class_ "headingNotFound"]
      ( mconcat
          [ (Lucid.toHtml . Text.pack) "Could not resolve heading: ",
            Lucid.toHtml h,
            (Lucid.toHtml . Text.pack) " under resource: ",
            (Lucid.toHtml . subtextHtml) dn
          ]
      )

  toHtmlRaw (SubtextHtml (Core.Present b)) = Lucid.toHtmlRaw . subtextHtml $ b
  toHtmlRaw (SubtextHtml (Core.ResourceNotFound dn)) =
    Lucid.div_
      [Lucid.class_ "resourceNotFound"]
      ( (Lucid.toHtmlRaw . Text.pack) "Could not resolve resource: "
          <> (Lucid.toHtmlRaw . subtextHtml) dn
      )
  toHtmlRaw (SubtextHtml (Core.HeadingNotFound dn h)) =
    Lucid.div_
      [Lucid.class_ "headingNotFound"]
      ( mconcat
          [ (Lucid.toHtmlRaw . Text.pack) "Could not resolve heading: ",
            Lucid.toHtmlRaw h,
            (Lucid.toHtmlRaw . Text.pack) " under resource: ",
            (Lucid.toHtmlRaw . subtextHtml) dn
          ]
      )

------------------------------------------------------------
--                    Document to HTML                    --
------------------------------------------------------------

data Group a
  = Single a
  | Multiple [a]

instance Functor Group where
  fmap f (Single x) = Single $ f x
  fmap f (Multiple xs) = Multiple $ fmap f xs

group :: (a -> Bool) -> [a] -> [Group a]
group shouldGroup = finalize . foldr step ([], Nothing)
  where
    step x (groups, acc) = case (shouldGroup x, acc) of
      (True, Just ms) -> (groups, Just (x : ms))
      (True, Nothing) -> (groups, Just [x])
      (False, Just ms) -> (Single x : Multiple ms : groups, Nothing)
      (False, Nothing) -> (Single x : groups, Nothing)

    finalize (groups, Just ms) = Multiple ms : groups
    finalize (groups, Nothing) = groups

instance (ToHtml a) => ToHtml (Group a) where
  toHtml (Single s) = Lucid.toHtml s
  toHtml (Multiple ms) = Lucid.ul_ $ (mconcat . map Lucid.toHtml) ms

  toHtmlRaw (Single s) = Lucid.toHtmlRaw s
  toHtmlRaw (Multiple ms) = Lucid.ul_ $ (mconcat . map Lucid.toHtmlRaw) ms

isResolvedBullet :: Core.Resolved -> Bool
isResolvedBullet (Core.Present (Core.Bullet _)) = True
isResolvedBullet _ = False

instance ToHtml (SubtextHtml (Core.Document Core.Resolved)) where
  toHtml = mconcat . fmap (Lucid.toHtml . fmap subtextHtml) . group isResolvedBullet . Core.content . unSubtextHtml
  toHtmlRaw = mconcat . fmap (Lucid.toHtmlRaw . fmap subtextHtml) . group isResolvedBullet . Core.content . unSubtextHtml

----------     Subtext to HTML-formatted Text     ----------

renderBlock :: Core.Block -> Text.Text
renderBlock = toStrict . Lucid.renderText . Lucid.toHtml . subtextHtml

renderDoc :: Core.Document Core.Resolved -> Text.Text
renderDoc = toStrict . Lucid.renderText . Lucid.toHtml . subtextHtml