module MP where

import System.Environment
import Data.Char

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\#"
-----------------------------------------------------
-- Looks up a corresponding value of inputted string in a dictionary
lookUp :: String -> [(String, a)] -> [a]
lookUp s dictionary
  = [q | (p, q) <- dictionary, p == s]

-- Split text based on chosen separators and string
splitText :: [Char] -> String -> (String, [String])
splitText _ "" = ("", [""])
splitText someSeparators t@(c:cs)
  | c `elem` someSeparators = (c : m, "" : t')
  | otherwise               = (m, ([c] ++ theHead) : theTail)
  where
    (m, t') = splitText someSeparators cs
    theHead = head t'
    theTail = tail t'

-- The inverse function of splitText
combine :: String -> [String] -> [String]
combine "" something  = something
combine (a:as) (b:bs) = b : head [[a]:b'] -- strips separator off original list
  where
    b' = combine as bs

-- gets a pair of Keyword and its Definition based on an inputted string
getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
getKeywordDefs (f:fs)
  = (kw, unwords kd) : kwds       -- unwords work like combine " " text
  where                           -- but it returns String instead of [String]
    (_, kw:kd) = splitText " \n" f
    kwds       = getKeywordDefs fs

-- expands definition based on file contents
expand :: FileContents -> FileContents -> FileContents
expand "" _ = ""
expand stuff definitions
 =  foldr1 (++) ([replaceWord p defs | p <- q])
  where
    (_, definitions') = splitText "\n" definitions
    (m, n)            = splitText separators stuff
    q                 = combine m n
    defs              = getKeywordDefs definitions'


-- this adds \n-----\n to the expanded file
addSpace :: String -> [String] -> String
addSpace stuff [c]    = c
addSpace stuff (c:cs) = concat [c, stuff, addSpace stuff cs]

-- extended function of expand
multiExpand :: FileContents -> FileContents -> FileContents
multiExpand cont defs = addSpace "\n-----\n" [expand cont i | i <- splitted]
  where splitted = snd(splitText "#" defs)

-- helper function, replaces placeholders with definitions
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
      writeFile output (multiExpand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")
