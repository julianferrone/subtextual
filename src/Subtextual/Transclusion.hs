module Subtextual.Transclusion (Corpus) where

import qualified Data.Graph as Graph
import Data.List
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Tree as Tree
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

resolveToBlock :: Corpus -> Authored -> [Resolved]
resolveToBlock _ (Raw block) = singleton . Present $ block
resolveToBlock corpus (ToResolve transclusion) =
  case resolveTransclusion corpus transclusion of
    Just blocks -> Present <$> blocks
    Nothing -> singleton . TransclusionMissing . target $ transclusion

resolveTransclusions :: Corpus -> [Authored] -> [Resolved]
resolveTransclusions corpus = mconcat . fmap (resolveToBlock corpus)

------------------------------------------------------------
--                  Transclusion Ordering                 --
------------------------------------------------------------

----------           Graph Construction           ----------

referencedDocs :: Document Authored -> Document DocumentName
referencedDocs = liftD (fmap target . catToResolve)

-- This is just to prepare for constructing a graph
docGraphNode :: Document DocumentName -> (DocumentName, DocumentName, [DocumentName])
docGraphNode doc = (title doc, title doc, content doc)

docReferencesGraph ::
  [Document DocumentName] ->
  ( Graph.Graph,
    Graph.Vertex -> (DocumentName, DocumentName, [DocumentName]),
    DocumentName -> Maybe Graph.Vertex
  )
docReferencesGraph = Graph.graphFromEdges . fmap docGraphNode

----------        Check if Graph is Cyclic        ----------

cycles :: Graph.Graph -> [[Graph.Tree Graph.Vertex]]
cycles =
  filter (not . null) -- Multiple nodes in SCC = nonempty list
    . fmap Tree.subForest -- If there's only 1 node in a strongly connected
    -- component, there's no cycle. If there's multiple
    -- nodes, those will show up as children of the first
    -- node
    . Graph.scc

isCyclic :: Graph.Graph -> Bool
isCyclic = null . cycles

sortDag :: Graph.Graph -> Maybe [Graph.Vertex]
sortDag g = if isCyclic g then Nothing else Just . Graph.topSort $ g

sortedDocReferencesGraph ::
  [Document DocumentName] ->
  Maybe
    ( Graph.Graph,
      Graph.Vertex -> (DocumentName, DocumentName, [DocumentName]),
      DocumentName -> Maybe Graph.Vertex
    )
sortedDocReferencesGraph docs = sorted
  where
    ( graph,
      nodeLookup,
      vertexLookup
      ) = docReferencesGraph docs

    sorted =
      if isCyclic graph
        then Nothing
        else Just (graph, nodeLookup, vertexLookup)