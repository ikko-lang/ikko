module Region where

data Region =
  Region
  { pFileName :: !String
  , pStart    :: !Position
  , pEnd      :: !Position
  } deriving (Eq, Ord, Show)

data Position =
  Position
  { pLine   :: !Int
  , pColumn :: !Int
  } deriving (Eq, Ord, Show)


-- TODO: Show what the actual region was
showRegion :: [String] -> Region -> String
showRegion fileLines region =
  let numberedLines = zipWith addNumber [1..] fileLines
      firstLine = pLine $ pStart region
      lastLine = pLine $ pEnd region
      nlines = min maxLines (lastLine - firstLine + 1)
      regionLines = take nlines $ drop (firstLine - 1) numberedLines
  in unlines regionLines

maxLines = 5

addNumber :: Int -> String -> String
addNumber lineNo line =
  let n = show lineNo
  in n ++ ": " ++ line
