module MP where

import System.Environment
import Data.Char

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"

-----------------------------------------------------

lookUp :: String -> [(String, a)] -> [a]
lookUp s dictionary
  = [q | (p, q) <- dictionary, p == s]

isNotSeparator :: Char -> [Char] -> Bool
isNotSeparator s separators
  | s `elem` separators = False
  | otherwise = True

splitText :: [Char] -> String -> (String, [String])
splitText _ "" = ("", [""])
splitText someSeparators t@(c:cs)
  | isNotSeparator c someSeparators = (m, ([c] ++ theHead) : theTail)
  | otherwise = (c : m, "" : t')
  where
    (m, t') = splitText someSeparators cs
    theHead = head t'
    theTail = tail t'

combine :: String -> [String] -> [String]
combine "" something = something
combine (a:as) (b:bs) = b : head [[a]:b']
  where
    b' = combine as bs

getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs
  = undefined

expand :: FileContents -> FileContents -> FileContents
expand
  = undefined

-- You may wish to uncomment and implement this helper function
-- when implementing expand
-- replaceWord :: String -> KeywordDefs -> String

-----------------------------------------------------

-- The provided main program which uses your functions to merge a
-- template and source file.
main :: IO ()
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")