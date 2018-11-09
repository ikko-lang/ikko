module Region where

data Region =
  Region
  { fileName :: !String
  , start :: !Position
  , end :: !Position
  } deriving (Eq, Ord, Show)

data Position =
  Position
  { line :: !Int
  , column :: !Int
  } deriving (Eq, Ord, Show)


-- TODO: Show what the actual region was
showRegion :: [String] -> Region -> String
showRegion fileLines region =
  let numberedLines = zipWith addNumber [1..] fileLines
      firstLine = line $ start region
      lastLine = line $ end region
      nlines = min maxLines (lastLine - firstLine + 1)
      regionLines = take nlines $ drop (firstLine - 1) numberedLines
  in unlines regionLines

maxLines = 5

addNumber :: Int -> String -> String
addNumber lineNo line =
  let n = show lineNo
  in n ++ ": " ++ line
