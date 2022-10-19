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


splitText :: [Char] -> String -> (String, [String])
splitText _ "" = ("", [""])
splitText someSeparators t@(c:cs)
  | not (c `elem` someSeparators) = (m, ([c] ++ theHead) : theTail)
  | otherwise                     = (c : m, "" : t')
  where
    (m, t') = splitText someSeparators cs
    theHead = head t'
    theTail = tail t'

combine :: String -> [String] -> [String]
combine "" something  = something
combine (a:as) (b:bs) = b : head [[a]:b'] -- strips separator off original list
  where
    b' = combine as bs

getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
getKeywordDefs file@(f:fs)
  = (kw, unwords kd) : kwds       -- unwords work like combine " " text
  where                           -- but it returns String instead of [String]
    (_, kw:kd) = splitText " \n" f
    kwds       = getKeywordDefs fs

expand :: FileContents -> FileContents -> FileContents
expand "" _ = ""
expand stuff definitions
  = foldr1 (++) ([replaceWord p (getKeywordDefs definitions') | p <- q])
  where
    (_, definitions') = splitText "\n" definitions
    (m, n)            = splitText separators stuff
    q                 = combine m n

-- You may wish to uncomment and implement this helper function
-- when implementing expand
replaceWord :: String -> KeywordDefs -> String
replaceWord str [] = str
replaceWord str (c : cs)
  | str == a  = b
  | otherwise = replaceWord str cs
  where
    (a, b) = c
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
