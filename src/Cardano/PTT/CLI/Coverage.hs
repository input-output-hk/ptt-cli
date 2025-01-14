{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.PTT.CLI.Coverage (
  CoverageEntry (..),
  CoverageGroup (..),
  flattenCoverage,
  covEntryToCovLoc,
  covEntryToAnnotation,
  FlattenCoverage (..),
  CovLoc (..),
  CoverageAnnotation (CoverBool, CoverLocation),
  Status (..),
  CoverStatus (..),
  IgnoreStatus (..),
)
where

import Data.Aeson
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Map (Map)

import Data.Map qualified as Map
import Data.Set qualified as Set

import PlutusTx.Coverage (
  CovLoc (..),
  CoverageAnnotation (CoverBool, CoverLocation),
 )

data CoverageGroup = CoverageGroup
  { covered :: ![CoverageEntry]
  , uncovered :: ![CoverageEntry]
  , ignored :: ![CoverageEntry]
  }
  deriving (Show)

instance ToJSON CoverageGroup where
  toJSON CoverageGroup{..} =
    object
      [ "covered" .= covered
      , "uncovered" .= uncovered
      , "ignored" .= ignored
      ]

data CoverageEntry = CoverageEntry
  { coveStartLineNo :: !Int
  , coveEndLineNo :: !Int
  , coveStartColumn :: !Int
  , coveEndColumn :: !Int
  , coveStatus :: !(Maybe Bool)
  , coveFile :: !String
  }
  deriving (Show)

instance ToJSON CoverageEntry where
  toJSON CoverageEntry{..} =
    object
      [ "startLine" .= coveStartLineNo
      , "endLine" .= coveEndLineNo
      , "startColumn" .= coveStartColumn
      , "endColumn" .= coveEndColumn
      , "status" .= coveStatus
      , "file" .= coveFile
      ]

-- NOTE: Statuses and their class instances are copied from the cardano-node-emulator
-- since the project doesn't expose them.
data CoverStatus = NotCovered | HasBeenHere | HasBeenFalse | HasBeenTrue | HasBeenBoth
  deriving (Eq, Show)

instance ToJSON CoverStatus where
  toJSON NotCovered = "NotCovered"
  toJSON HasBeenHere = "HasBeenHere"
  toJSON HasBeenFalse = "HasBeenFalse"
  toJSON HasBeenTrue = "HasBeenTrue"
  toJSON HasBeenBoth = "HasBeenBoth"

data IgnoreStatus = NotIgnored | IgnoredIfFalse | IgnoredIfTrue | AlwaysIgnored
  deriving (Eq, Show)

instance ToJSON IgnoreStatus where
  toJSON NotIgnored = "NotIgnored"
  toJSON IgnoredIfFalse = "IgnoredIfFalse"
  toJSON IgnoredIfTrue = "IgnoredIfTrue"
  toJSON AlwaysIgnored = "AlwaysIgnored"

data Status = OnChain !CoverStatus !IgnoreStatus
  deriving (Eq, Show)

instance Semigroup CoverStatus where
  HasBeenBoth <> _ = HasBeenBoth
  _ <> HasBeenBoth = HasBeenBoth
  HasBeenFalse <> HasBeenTrue = HasBeenBoth
  HasBeenTrue <> HasBeenFalse = HasBeenBoth
  HasBeenFalse <> _ = HasBeenFalse
  _ <> HasBeenFalse = HasBeenFalse
  HasBeenTrue <> _ = HasBeenTrue
  _ <> HasBeenTrue = HasBeenTrue
  HasBeenHere <> _ = HasBeenHere
  _ <> HasBeenHere = HasBeenHere
  NotCovered <> NotCovered = NotCovered

instance Monoid CoverStatus where
  mempty = NotCovered

instance Semigroup IgnoreStatus where
  AlwaysIgnored <> _ = AlwaysIgnored
  _ <> AlwaysIgnored = AlwaysIgnored
  IgnoredIfFalse <> IgnoredIfTrue = AlwaysIgnored
  IgnoredIfTrue <> IgnoredIfFalse = AlwaysIgnored
  IgnoredIfTrue <> _ = IgnoredIfTrue
  _ <> IgnoredIfTrue = IgnoredIfTrue
  IgnoredIfFalse <> _ = IgnoredIfFalse
  _ <> IgnoredIfFalse = IgnoredIfFalse
  NotIgnored <> NotIgnored = NotIgnored

instance Monoid IgnoreStatus where
  mempty = NotIgnored

-- The Semigroup instance is used to combine swipes over identical ranges.
instance Semigroup Status where
  OnChain c i <> OnChain c' i' = OnChain (c <> c') (i <> i')

instance ToJSON Status where
  toJSON (OnChain c i) =
    object
      [ "cover" .= c
      , "ignore" .= i
      ]

instance Monoid Status where
  mempty = OnChain mempty mempty

newtype FlattenCoverage = FlattenCoverage
  { unFlattenCoverage :: Map CovLoc Status
  }
  deriving (Show, Eq)

newtype InternalCovLoc = InternalCovLoc CovLoc

-- same as entry
instance ToJSON InternalCovLoc where
  toJSON (InternalCovLoc (CovLoc{..})) =
    object
      [ "startLine" .= _covLocStartLine
      , "endLine" .= _covLocEndLine
      , "startColumn" .= _covLocStartCol
      , "endColumn" .= _covLocEndCol
      , "file" .= _covLocFile
      ]
newtype InternalLocStatusKeyPair = InternalLocStatusKeyPair
  { unInternalLocStatusKeyPair :: (InternalCovLoc, Status)
  }

instance ToJSON InternalLocStatusKeyPair where
  toJSON InternalLocStatusKeyPair{..} =
    object
      [ "position" .= fst unInternalLocStatusKeyPair
      , "status" .= snd unInternalLocStatusKeyPair
      ]

instance ToJSON FlattenCoverage where
  toJSON FlattenCoverage{..} = toJSON xs
   where
    xs =
      map
        (InternalLocStatusKeyPair . Data.Bifunctor.first InternalCovLoc)
        (Map.toList unFlattenCoverage)

flattenCoverage :: CoverageGroup -> FlattenCoverage
flattenCoverage group = FlattenCoverage allAnnWithOneStatus
 where
  -- keep in mind that Annotation for the same portion of code might
  -- appear multiple times because of different statuses
  -- We are going to merge it later
  uncoveredAnn = toAnnSet uncovered
  coveredAnn = toAnnSet covered
  ignoredAnn = Set.fromList $ map covEntryToAnnotation $ ignored group
  allAnn = Set.union coveredAnn uncoveredAnn `Set.union` ignoredAnn
  -- transform the list of CoverageEntry into a set of CoverageAnnotation
  toAnnSet list = Set.fromList $ map covEntryToAnnotation $ list group

  allAnnWithStatuses :: Map CovLoc [Status]
  allAnnWithStatuses =
    listToMap $ map (\ann -> (annLocation ann, status' ann)) $ Set.toList allAnn

  mergeStatuses :: [Status] -> Status
  mergeStatuses = foldl' (<>) mempty

  allAnnWithOneStatus :: Map CovLoc Status
  allAnnWithOneStatus = Map.map mergeStatuses allAnnWithStatuses

  status' ann = OnChain (ifM c covS) (ifM i ignS)
   where
    i = Set.member ann ignoredAnn
    c = Set.notMember ann uncoveredAnn
    ifM False _ = mempty
    ifM True m = m
    covS = case ann of
      CoverBool _ True -> HasBeenTrue
      CoverBool _ False -> HasBeenFalse
      CoverLocation{} -> HasBeenHere
    ignS = case ann of
      CoverBool _ True -> IgnoredIfTrue
      CoverBool _ False -> IgnoredIfFalse
      CoverLocation{} -> AlwaysIgnored

listToMap :: forall k a. (Ord k) => [(k, a)] -> Map k [a]
listToMap = foldl' g Map.empty
 where
  g :: Map k [a] -> (k, a) -> Map k [a]
  g d (k, a) =
    let h Nothing = Just [a]
        h (Just xs) = Just (a : xs)
     in Map.alter h k d

covEntryToCovLoc :: CoverageEntry -> CovLoc
covEntryToCovLoc CoverageEntry{..} =
  CovLoc coveFile coveStartLineNo coveEndLineNo coveStartColumn coveEndColumn

covEntryToAnnotation :: CoverageEntry -> CoverageAnnotation
covEntryToAnnotation entry@CoverageEntry{..} =
  let covLoc = covEntryToCovLoc entry
   in case coveStatus of
        Just bool -> CoverBool covLoc bool
        Nothing -> CoverLocation $ covEntryToCovLoc entry

annLocation :: CoverageAnnotation -> CovLoc
annLocation (CoverLocation loc) = loc
annLocation (CoverBool loc _) = loc
