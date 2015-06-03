{-

Overlap module
By Gregory W. Schwartz

Collection of functions for the detection of overlaps between fasta files

-}

{-# LANGUAGE OverloadedStrings #-}

module Overlap ( getOverlapMapFromFastaSequences
               , assignOverlap
               ) where

-- Built in
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Cabal
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Fasta.ByteString.Lazy
import Data.ByteString.Conversion

-- Local
import Types

-- | Get the fasta sequences in a list as bytestrings and get the unique
-- sequences of them
getUniqueByteStringList :: [FastaSequence] -> [B.ByteString]
getUniqueByteStringList = Set.toList . Set.fromList . map fastaSeq

-- | Get the number of times a bytestring is overlapping in a list of lists
getByteStringOverlapMap :: [[B.ByteString]] -> OverlapMap
getByteStringOverlapMap = Map.fromListWith (+) . concatMap initiate
  where
    initiate = flip zip [1,1..]

-- | Get the number of times a bytestring sequence is overlapping in a list
-- of lists of fasta sequences
getOverlapMapFromFastaSequences :: [[FastaSequence]] -> OverlapMap
getOverlapMapFromFastaSequences = getByteStringOverlapMap
                                . map getUniqueByteStringList

-- | Assign the overlaps to a fasta sequence in its header
assignOverlap :: OverlapMap -> FastaSequence -> FastaSequence
assignOverlap overlapMap fSeq = fSeq { fastaHeader = fastaHeader fSeq
                                          `B.append` "|"
                                          `B.append` (overlapSize $ fastaSeq fSeq)
                                     }
  where
    overlapSize x  = toByteString . check $ Map.lookup x overlapMap
    check (Just x) = x
    check Nothing  = error ( ("Could not find sequence: " :: String)
                          ++ (C.unpack . fastaSeq $ fSeq)
                           )
