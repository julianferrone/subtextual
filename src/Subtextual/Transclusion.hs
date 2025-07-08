module Subtextual.Transclusion (excerpt) where

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

documents :: Corpus a -> [Core.Document a]
documents = fmap (uncurry Core.document) . Map.assocs . unCorpus

-- ----------          Looking up Documents          ----------

lookupDoc :: Core.DocumentName -> Corpus a -> Maybe [a]
lookupDoc name = Map.lookup name . unCorpus

resolveTransclusion :: Corpus Core.Resolved -> Core.Transclusion -> [Core.Resolved]
resolveTransclusion corpus (Core.Transclusion docName options) = case lookupDoc docName corpus of
  Nothing -> [Core.ResourceNotFound docName]
  Just resolveds -> case excerpt options resolveds of
    Left heading -> [Core.HeadingNotFound docName heading]
    Right excerpted -> excerpted

----------        Excerpting from Documents       ----------

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

referencedDocs :: Core.Document Core.Authored -> Core.Document Core.DocumentName
referencedDocs = Core.liftD (fmap Core.target . Core.catToResolves)

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