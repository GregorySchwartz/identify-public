{-

Types module
By Gregory W. Schwartz

Collection of types for use in the program

-}

module Types where

-- Built in
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as B

-- Basic
type Size = Int

-- Advanced
type OverlapMap = Map.Map B.ByteString Size
