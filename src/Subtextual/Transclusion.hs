module Subtextual.Transclusion (Corpus) where

import Subtextual.Core

import Data.List
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe (mapMaybe)

------------------------------------------------------------
--                   Corpus of Documents                  --
------------------------------------------------------------

data Corpus
  = Corpus
  { corpusDocuments :: Map.Map DocumentName Document,
    corpusDocumentSections :: Map.Map DocumentName (Map.Map T.Text Document)
  }

----------          Looking up Documents          ----------

lookupWholeDocument :: DocumentName -> Corpus -> Maybe Document
lookupWholeDocument name = Map.lookup name . corpusDocuments

lookupDocumentSections :: DocumentName -> Corpus -> Maybe (Map.Map T.Text Document)
lookupDocumentSections name = Map.lookup name . corpusDocumentSections

lookupTransclusion :: Transclusion -> Corpus -> Maybe Document
lookupTransclusion (Transclusion name (HeadingSection section)) corpus =
   lookupDocumentSections name corpus
   >>= Map.lookup section
lookupTransclusion (Transclusion name _) corpus = lookupWholeDocument name corpus

----------        Excerpting from Documents       ----------

excerpt :: TransclusionOptions -> Document -> Document
excerpt WholeDocument = id
excerpt (FirstLines length) = take length
excerpt (Lines start length) = take length . drop start
-- The reason this is left as `id` is because we should be grabbing the
-- heading section direct from the Corpus, which will have the sections
-- pre-analysed
excerpt (HeadingSection headingName) = id

resolveTransclusion :: Corpus -> Transclusion -> Maybe Document
resolveTransclusion corpus transclusion =
  (excerpt . opts) transclusion
    <$> lookupTransclusion transclusion corpus

resolveToBlock :: Corpus -> Either Block Transclusion -> Maybe Document
resolveToBlock _ (Left block) = Just [block]
resolveToBlock corpus (Right transclusion) = resolveTransclusion corpus transclusion


resolveTransclusions :: Corpus -> [Either Block Transclusion] -> Document
resolveTransclusions corpus = mconcat . mapMaybe (resolveToBlock corpus)