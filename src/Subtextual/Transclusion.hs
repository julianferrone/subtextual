module Subtextual.Transclusion
  ( Corpus,
    fromDocuments,
    toDocuments,
    lookupDocument,
    excerpt,
    resolveCorpus,
    GraphContainsCycles,
  )
where

import qualified Data.Graph as Graph
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Tree as Tree
import qualified Subtextual.Core as Core

------------------------------------------------------------
--                   Corpus of Documents                  --
------------------------------------------------------------

newtype Corpus a = Corpus {unCorpus :: Map.Map Core.DocumentName [a]} deriving (Eq, Ord, Show)

corpus :: Map.Map Core.DocumentName [a] -> Corpus a
corpus = Corpus

emptyCorpus :: Corpus a
emptyCorpus = corpus Map.empty

fromDocuments :: [Core.Document a] -> Corpus a
fromDocuments = corpus . Map.fromList . fmap (\d -> (Core.title d, Core.content d))

toDocuments :: Corpus a -> [Core.Document a]
toDocuments = fmap (uncurry Core.document) . Map.assocs . unCorpus

------------------------------------------------------------
--                   Inserting Documents                  --
------------------------------------------------------------

insertDoc :: Core.Document a -> Corpus a -> Corpus a
insertDoc doc = corpus . Map.insert (Core.title doc) (Core.content doc) . unCorpus

------------------------------------------------------------
--                  Looking up Documents                  --
------------------------------------------------------------

lookupContent :: Core.DocumentName -> Corpus a -> Maybe [a]
lookupContent name = Map.lookup name . unCorpus

lookupDocument :: Core.DocumentName -> Corpus a -> Maybe (Core.Document a)
lookupDocument name = fmap (Core.document name) . Map.lookup name . unCorpus

resolveTransclusion :: Corpus Core.Resolved -> Core.Transclusion -> [Core.Resolved]
resolveTransclusion corpus (Core.Transclusion docName options) = case lookupContent docName corpus of
  Nothing -> [Core.ResourceNotFound docName]
  Just resolveds -> case excerpt options resolveds of
    Left heading -> [Core.HeadingNotFound docName heading]
    Right excerpted -> excerpted

------------------------------------------------------------
--                  Excerpting Documents                  --
------------------------------------------------------------

{-
We return a `Left Core.Text.Text` from `excerpt` so that in `resolveTransclusion`,
we can wrap the not-found header into a `Core.HeadingNotFound` and thus show the
failed attempt to excerpt a heading subsection of the given content.
-}
excerpt ::
  Core.TransclusionOptions -> -- How to excerpt the blocks
  [Core.Resolved] -> -- Already resolved blocks
  Either Text.Text [Core.Resolved] -- Excerpt of the document
excerpt Core.WholeDocument = Right
excerpt (Core.FirstLines len) = Right . take len
excerpt (Core.Lines start len) = Right . take len . drop start
excerpt (Core.HeadingSection heading) =
  rejectIfEmpty heading
    . takeWhile (\resolved -> isGivenHeading heading resolved || isNotHeading resolved)
    . dropWhile (not . isGivenHeading heading)
  where
    rejectIfEmpty :: a -> [b] -> Either a [b]
    rejectIfEmpty a [] = Left a
    rejectIfEmpty _ bs = Right bs

    isGivenHeading :: Text.Text -> Core.Resolved -> Bool
    isGivenHeading h = (==) (Core.Present (Core.Heading h))

    isNotHeading :: Core.Resolved -> Bool
    isNotHeading (Core.Present (Core.Heading _)) = False
    isNotHeading _ = True

------------------------------------------------------------
--                  Transclusion Ordering                 --
------------------------------------------------------------

----------           Graph Construction           ----------

docReferences :: [Core.Authored] -> [Core.DocumentName]
docReferences = fmap Core.target . Core.catToResolves

corpusReferences :: Corpus Core.Authored -> Corpus Core.DocumentName
corpusReferences = corpus . fmap docReferences . unCorpus

-- This is just to prepare for constructing a graph
docGraphNode :: Core.Document Core.DocumentName -> (Core.DocumentName, Core.DocumentName, [Core.DocumentName])
docGraphNode doc = (Core.title doc, Core.title doc, Core.content doc)

docLevelReferencesGraph ::
  Corpus Core.Authored -> -- A raw corpus to process
  ( Graph.Graph,
    Graph.Vertex -> (Core.DocumentName, Core.DocumentName, [Core.DocumentName]),
    Core.DocumentName -> Maybe Graph.Vertex
  ) -- The graph
docLevelReferencesGraph =
  Graph.graphFromEdges
    . fmap docGraphNode
    . toDocuments
    . corpusReferences

docLevelDag ::
  Corpus Core.Authored ->
  Either
    (GraphContainsCycles Core.DocumentName)
    -- The list of DocumentNames, sorted topologically.
    --
    -- Julian 20250708: We only want to do a single pass through a 
    -- Corpus Authored when transcluding the documents. That means whenever
    -- we want to transclude a referenced document into the referencing 
    -- document, we want to look up the already-processed version of the
    -- referenced document, which we do by looking up the document in the
    -- Corpus Resolved.

    -- But how can we be sure that the referenced document is already in the
    -- Corpus Resolved? By pre-sorting the DocumentNames in topological order
    -- such that lower Documents (i.e. those that contain no/fewer references)
    -- come first in the list. That means that whenever we come to a Document
    -- which contains Transclusions which need to be resolved, we by definition
    -- have already processed and resolved the documents it is referencing.

    -- Hence we only need a single pass over the Corpus Authored to confidently 
    -- process all the Documents! i.e. by resolving their content

    -- Since we also know this list is only returned if there are no cycles,
    -- we know that at minimum the very first document must contain no
    -- Transclusions - since if the first document referred to some other document,
    -- that other document would come first in the list (by definition of topological
    -- ordering).

    -- Hence even in the worst-case scenario where there is only one document
    -- which contains only raw content, we still only need the one pass - which 
    -- we can do via a fold.
    [Core.DocumentName]
docLevelDag authoredCorpus = output
  where
    (graph, labelLookup, _) = docLevelReferencesGraph authoredCorpus

    vertexLabel :: Graph.Vertex -> Core.DocumentName
    vertexLabel = fst3 . labelLookup

    fst3 :: (a, b, c) -> a
    fst3 (a, _, _) = a

    output ::
      Either
        (GraphContainsCycles Core.DocumentName)
        -- topologically sorted list of DocumentNames. A Document "A" which
        -- is referenced by Document "B" will come before "B" in the list.
        [Core.DocumentName]
    output = case sortDag graph vertexLabel of
      Right sorted -> Right . fmap vertexLabel $ sorted
      Left l -> Left l

----------        Check if Graph is Cyclic        ----------

cycles :: Graph.Graph -> [Graph.Tree Graph.Vertex]
cycles =
  filter (not . null . Tree.subForest) -- Multiple nodes in SCC = nonempty tree
    . Graph.scc

sortDag ::
  Graph.Graph -> -- Graph to check cycles for
  (Graph.Vertex -> a) -> -- Lookup vertex names for easier reporting
  Either
    (GraphContainsCycles a) -- Graph has cycles
    [Graph.Vertex] -- topologically sorted list of vertices
sortDag g nameLookup = case cycles g of
  [] -> Right . Graph.topSort $ g
  cycles' -> Left . GraphContainsCycles . fmap (fmap nameLookup) $ cycles'

------------------------------------------------------------
--             Resolve All Documents In Corpus            --
----------------------------------------------------------

newtype GraphContainsCycles a = GraphContainsCycles [Graph.Tree a] deriving (Eq, Ord, Show)

resolveFromCorpuses :: Core.DocumentName -> Corpus Core.Authored -> Corpus Core.Resolved -> Core.Document Core.Resolved
resolveFromCorpuses docName authoredCorpus resolvedCorpus = case lookupContent docName authoredCorpus of
  Nothing -> Core.document docName [Core.ResourceNotFound docName]
  Just authoredDoc -> Core.document docName content
    where
      content = Core.resolveAuthored (resolveTransclusion resolvedCorpus) authoredDoc

addToCorpus :: Corpus Core.Authored -> [Core.DocumentName] -> Corpus Core.Resolved -> Corpus Core.Resolved
addToCorpus authoredCorpus docNames resolvedCorpus =
  foldr
    (updateResolvedCorpus authoredCorpus)
    resolvedCorpus
    docNames
  where
    updateResolvedCorpus :: Corpus Core.Authored -> Core.DocumentName -> Corpus Core.Resolved -> Corpus Core.Resolved
    updateResolvedCorpus authoredCorpus docName resolvedCorpus = insertDoc (resolveFromCorpuses docName authoredCorpus resolvedCorpus) resolvedCorpus

resolveCorpus :: Corpus Core.Authored -> Either (GraphContainsCycles Core.DocumentName) (Corpus Core.Resolved)
resolveCorpus authoredCorpus = do
  authoredDag <- docLevelDag authoredCorpus
  return $ addToCorpus authoredCorpus authoredDag emptyCorpus