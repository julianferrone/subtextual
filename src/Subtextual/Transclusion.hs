module Subtextual.Core.Transclusion (Corpus) where

import qualified Data.Graph as Graph
import Data.List
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import qualified Data.Tree as Tree
import qualified Subtextual.Core as Core

------------------------------------------------------------
--                   Corpus of Documents                  --
------------------------------------------------------------

data Corpus
  = Corpus
  { corpusDocuments :: Map.Map Core.DocumentName [Core.Block],
    corpusDocumentSections :: Map.Map Core.DocumentName (Map.Map Text.Text [Core.Block])
  }

----------          Looking up Documents          ----------

lookupWholeDocument :: Core.DocumentName -> Corpus -> Maybe [Core.Block]
lookupWholeDocument name = Map.lookup name . corpusDocuments

lookupDocumentSections :: Core.DocumentName -> Corpus -> Maybe (Map.Map Text.Text [Core.Block])
lookupDocumentSections name = Map.lookup name . corpusDocumentSections

lookupTransclusion :: Core.Transclusion -> Corpus -> Maybe [Core.Block]
lookupTransclusion (Core.Transclusion name (Core.HeadingSection section)) corpus =
  lookupDocumentSections name corpus
    >>= Map.lookup section
lookupTransclusion (Core.Transclusion name _) corpus = lookupWholeDocument name corpus

----------        Excerpting from Documents       ----------

excerpt :: Core.TransclusionOptions -> [Core.Block] -> [Core.Block]
excerpt Core.WholeDocument = id
excerpt (Core.FirstLines length) = take length
excerpt (Core.Lines start length) = take length . drop start
-- The reason this is left as `id` is because we should be grabbing the
-- heading section direct from the Corpus, which will have the sections
-- pre-analysed
excerpt (Core.HeadingSection headingName) = id

resolveTransclusion :: Corpus -> Core.Transclusion -> Maybe [Core.Block]
resolveTransclusion corpus transclusion =
  (excerpt . Core.opts) transclusion
    <$> lookupTransclusion transclusion corpus

resolveToBlock :: Corpus -> Core.Authored -> [Core.Resolved]
resolveToBlock _ (Core.Raw block) = singleton . Core.Present $ block
resolveToBlock corpus (Core.ToResolve transclusion) =
  case resolveTransclusion corpus transclusion of
    Just blocks -> Core.Present <$> blocks
    Nothing -> singleton . Core.ResourceNotFound . Core.target $ transclusion

resolveTransclusions :: Corpus -> [Core.Authored] -> [Core.Resolved]
resolveTransclusions corpus = mconcat . fmap (resolveToBlock corpus)

------------------------------------------------------------
--                  Core.Transclusion Ordering                 --
------------------------------------------------------------

----------           Graph Construction           ----------

referencedDocs :: Core.Document Core.Authored -> Core.Document Core.DocumentName
referencedDocs = Core.liftD (fmap Core.target . Core.catToResolve)

-- This is just to prepare for constructing a graph
docGraphNode :: Core.Document Core.DocumentName -> (Core.DocumentName, Core.DocumentName, [Core.DocumentName])
docGraphNode doc = (Core.title doc, Core.title doc, Core.content doc)

docReferencesGraph ::
  [Core.Document Core.DocumentName] ->
  ( Graph.Graph,
    Graph.Vertex -> (Core.DocumentName, Core.DocumentName, [Core.DocumentName]),
    Core.DocumentName -> Maybe Graph.Vertex
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
  [Core.Document Core.DocumentName] ->
  Maybe
    ( Graph.Graph,
      Graph.Vertex -> (Core.DocumentName, Core.DocumentName, [Core.DocumentName]),
      Core.DocumentName -> Maybe Graph.Vertex
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