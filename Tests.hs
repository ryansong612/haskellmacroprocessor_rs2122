module Tests where

import IC.TestSuite

import MP hiding (main)

lookUpTestCases
  = [ ("A", [("A", 8), ("B",9), ("C",5), ("A",7)]) ==> [8,7]
    , ("a", []) ==> []
    , ("a", [("a", 9)]) ==> [9]
    , ("a", [("b", 9)]) ==> []
    , ("c", [("z", 9), ("A", 10), ("B", 12), ("c", 5), ("P", 2)]) ==> [5]
    , ("L", [("L", 250), ("L", 251), ("L", 1314250)]) ==> [250, 251, 1314250]
    , ("W", [("L", 251), ("L", 245), ("L", 373858)]) ==> []
    , ("", []) ==> []
    ]

splitTextTestCases
  = [ (" .,", "A comma, then some words.")
        ==> (" ,   .",["A","comma","","then","some","words",""])
    , ("", "")
        ==> ("", [""])
    , (".", "A.B")
        ==> (".", ["A","B"])
    , (" ", " A")
        ==> (" ", ["", "A"])
    , (" ,;/!", "Hi! I am Ryan, very excited to be here at; Imperial!")
        ==> ("!   ,      ; !",["Hi","","I","am","Ryan","","very","excited",
 "to","be","here","at","","Imperial",""])
    , ("/.';9[]", "")
        ==> ("", [""])
    , ("[]", "Guess What! This is [] a list!")
        ==> ("[]",["Guess What! This is ",""," a list!"])
    ]

combineTestCases :: [((String, [String]), [String])]
combineTestCases
  = [ (" ,   .", ["A","comma","","then","some","words",""])
        ==> ["A"," ","comma",",",""," ","then"," ","some"," ","words",".",""]

    , ("", [""])
        ==> [""]
    , (".", ["A","B"])
        ==> ["A",".","B"]
    , (" ", ["", "A"])
        ==> [""," ","A"]
    , ("!   ,      ; !", ["Hi","","I","am","Ryan","","very","excited","to",
 "be","here","at","","Imperial",""])
        ==> ["Hi","!",""," ","I"," ","am"," ","Ryan",",",""," ","very"," ",
 "excited"," ","to"," ","be"," ","here"," ","at",";",""," ","Imperial","!",""]
    , ("[]", ["Guess What! This is ",""," a list!"])
        ==> ["Guess What! This is ","[","","]"," a list!"]
    ]

getKeywordDefsTestCases
  = [ ["$rule Reproduce this precisely -- or else!!"]
        ==> [("$rule","Reproduce this precisely -- or else!!")]
    , ["$x Define x", "$y 55"]
        ==> [("$x","Define x"),("$y","55")]
    , ["$a A", "$b B", "$c C"]
        ==> [("$a","A"),("$b","B"),("$c","C")]
    , []
        ==> []
    , ["$x-y-z $$$"]
        ==> [("$x-y-z","$$$")]
    , ["$$ something to think about"]
        ==> [("$$","something to think about")]
    , ["$ meanie!"]
        ==> [("$","meanie!")]
    , ["$var  Tristan Allwood"]
        ==> [("$var", " Tristan Allwood")]
    , ["$wassup What is up"]
        ==> [("$wassup","What is up")]
    , ["$ wassup What is up"]
        ==> [("$","wassup What is up")]
    ]

expandTestCases
  = [ ("The capital of $1 is $2", "$1 Peru\n$2 Lima.")
        ==> "The capital of Peru is Lima."
    , ("The time is $a", "$a now.")
        ==> "The time is now."
    , ("Keywords (e.g. $x, $y, $z...) may appear anwhere, e.g. <$here>.",
       "$x $a\n$y $b\n$z $c\n$here $this-is-one")
        ==> "Keywords (e.g. $a, $b, $c...) may appear anwhere, e.g. <$this-is-one>."
    , ("", "$keyword keywords")
        ==> ""
    , ("My name is $name", "$name Kevin Durant")
        ==> "My name is Kevin Durant"
    ]



multiExpandTestCases
  = [ ("Welcome to $town, where $name was born in $birth-date."
  , "$name William Shakespeare\n$birth-date 1564\n$town Stratford upon Avon\n#\n$birth-date 1840\n$town Stinsford\n$name Thomas Hardy\n#\n$name Charles Dickens\n$town Landport\n$birth-date 1812")
    ==> "Welcome to Stratford upon Avon, where William Shakespeare was born in 1564.\n-----\nWelcome to Stinsford, where Thomas Hardy was born in 1840.\n-----\nWelcome to Landport, where Charles Dickens was born in 1812."
  ]
allTestCases
  = [ TestCase "lookUp"  (uncurry lookUp)
                         lookUpTestCases
    , TestCase "splitText"   (uncurry splitText)
                         splitTextTestCases
    , TestCase "combine" (uncurry combine)
                         combineTestCases

    , TestCase "getKeywordDefs" getKeywordDefs
                                getKeywordDefsTestCases

    , TestCase "expand"  (uncurry expand)
                         expandTestCases
    , TestCase "multiExpand" (uncurry multiExpand)
                         multiExpandTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
