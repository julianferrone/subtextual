module Subtextual.Transclusion (excerpt, resolveCorpus) where

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

newtype Corpus a = Corpus {unCorpus :: Map.Map Core.DocumentName [a]}

corpus :: Map.Map Core.DocumentName [a] -> Corpus a
corpus = Corpus

documents :: Corpus a -> [Core.Document a]
documents = fmap (uncurry Core.document) . Map.assocs . unCorpus

------------------------------------------------------------
--                  Looking up Documents                  --
------------------------------------------------------------

lookupDoc :: Core.DocumentName -> Corpus a -> Maybe [a]
lookupDoc name = Map.lookup name . unCorpus

resolveTransclusion :: Corpus Core.Resolved -> Core.Transclusion -> [Core.Resolved]
resolveTransclusion corpus (Core.Transclusion docName options) = case lookupDoc docName corpus of
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
  Core.TransclusionOptions            -- How to excerpt the blocks
  -> [Core.Resolved]                  -- Already resolved blocks
  -> Either Text.Text [Core.Resolved] -- Excerpt of the document
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

-- resolveTransclusion :: Corpus -> Core.Transclusion -> Maybe [Core.Block]
-- resolveTransclusion corpus transclusion =
--   (excerpt . Core.opts) transclusion
--     <$> lookupTransclusion transclusion corpus

-- resolveToBlock :: Corpus -> Core.Authored -> [Core.Resolved]
-- resolveToBlock _ (Core.Raw block) = singleton . Core.Present $ block
-- resolveToBlock corpus (Core.ToResolve transclusion) =
--   case resolveTransclusion corpus transclusion of
--     Just blocks -> Core.Present <$> blocks
--     Nothing -> singleton . Core.ResourceNotFound . Core.target $ transclusion

-- resolveTransclusions :: Corpus -> [Core.Authored] -> [Core.Resolved]
-- resolveTransclusions corpus = mconcat . fmap (resolveToBlock corpus)

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
   . documents
   . corpusReferences

docLevelDag :: 
  Corpus Core.Authored 
  -> Either 
    (GraphContainsCycles Core.DocumentName) 
    [Core.DocumentName] -- The list of DocumentNames, sorted topologically
docLevelDag authoredCorpus = output where
  (graph, labelLookup, vertexLookup) = docLevelReferencesGraph authoredCorpus

  vertexLabel :: Graph.Vertex -> Core.DocumentName
  vertexLabel = fst3 . labelLookup

  fst3 :: (a, b, c) -> a
  fst3 (a, _, _) = a

  output :: Either 
    (GraphContainsCycles Core.DocumentName) 
    [Core.DocumentName]
  output = case sortDag graph vertexLabel of
    Right sorted -> Right . fmap vertexLabel $ sorted
    Left left -> Left left

----------        Check if Graph is Cyclic        ----------

cycles :: Graph.Graph -> [Graph.Tree Graph.Vertex]
cycles =
  filter (not . null) -- Multiple nodes in SCC = nonempty tree
    . Graph.scc

sortDag :: 
  Graph.Graph               -- Graph to check cycles for
  -> (Graph.Vertex -> a)    -- Lookup vertex names for easier reporting
  -> Either 
    (GraphContainsCycles a) -- Graph has cycles
    [Graph.Vertex]          -- topologically sorted list of vertices
sortDag g nameLookup = case cycles g of
  [] -> Right . Graph.topSort $ g
  cycles' -> Left . GraphContainsCycles . fmap (fmap nameLookup) $ cycles'

------------------------------------------------------------
--             Resolve All Documents In Corpus            --
------------------------------------------------------------

newtype GraphContainsCycles a = GraphContainsCycles [Graph.Tree a]

resolveCorpus :: Corpus Core.Authored -> Either (GraphContainsCycles Core.DocumentName) (Corpus Core.Resolved)        -- 
resolveCorpus authoredCorpus = _