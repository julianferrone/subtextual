module Subtextual.Transclusion (Corpus) where

import Data.List
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Graph as G
import qualified Data.Text as T
import Subtextual.Core

------------------------------------------------------------
--                   Corpus of Documents                  --
------------------------------------------------------------

data Corpus
  = Corpus
  { corpusDocuments :: Map.Map DocumentName [Block],
    corpusDocumentSections :: Map.Map DocumentName (Map.Map T.Text [Block])
  }

----------          Looking up Documents          ----------

lookupWholeDocument :: DocumentName -> Corpus -> Maybe [Block]
lookupWholeDocument name = Map.lookup name . corpusDocuments

lookupDocumentSections :: DocumentName -> Corpus -> Maybe (Map.Map T.Text [Block])
lookupDocumentSections name = Map.lookup name . corpusDocumentSections

lookupTransclusion :: Transclusion -> Corpus -> Maybe [Block]
lookupTransclusion (Transclusion name (HeadingSection section)) corpus =
  lookupDocumentSections name corpus
    >>= Map.lookup section
lookupTransclusion (Transclusion name _) corpus = lookupWholeDocument name corpus

----------        Excerpting from Documents       ----------

excerpt :: TransclusionOptions -> [Block] -> [Block]
excerpt WholeDocument = id
excerpt (FirstLines length) = take length
excerpt (Lines start length) = take length . drop start
-- The reason this is left as `id` is because we should be grabbing the
-- heading section direct from the Corpus, which will have the sections
-- pre-analysed
excerpt (HeadingSection headingName) = id

resolveTransclusion :: Corpus -> Transclusion -> Maybe [Block]
resolveTransclusion corpus transclusion =
  (excerpt . opts) transclusion
    <$> lookupTransclusion transclusion corpus

resolveToBlock :: Corpus -> Authored -> [Readable]
resolveToBlock _ (Raw block) = singleton . Present $ block
resolveToBlock corpus (ToResolve transclusion) = 
  case resolveTransclusion corpus transclusion of
    Just blocks -> Present <$> blocks
    Nothing -> singleton . TransclusionMissing . target $ transclusion

resolveTransclusions :: Corpus -> [Authored] -> [Readable]
resolveTransclusions corpus = mconcat . fmap (resolveToBlock corpus)

------------------------------------------------------------
--                  Transclusion Ordering                 --
------------------------------------------------------------

referenced :: DocumentName -> [Authored] -> (DocumentName, [DocumentName])
referenced name doc = (name, target <$> catToResolve doc)