module Main where

import Test.QuickCheck
import Mangling
import Data.Either (lefts, rights)
import Data.List (inits)

allNumerics :: [Numeric]
allNumerics = [Intval x | x <- [0..35]] ++
            [ Variable x | x <- [0..7]] ++
            [ MaxLength
            , MaxLengthMinus1
            , MaxLengthPlus1
            , CurrentWordLength
            , InitialLastCharPos
            , PosFoundChar
            , Infinite ]

allCharacterClassTypes = [ MatchVowel
                         , MatchConsonant
                         , MatchWhitespace
                         , MatchPunctuation
                         , MatchSymbol
                         , MatchLower
                         , MatchUpper
                         , MatchDigit
                         , MatchLetter
                         , MatchAlphaNum
                         , MatchAny ]

allMChar = [MatchChar x | x <- [' '..'Z']]

allCharacterClass = [CharacterClass ctype b | ctype <- allCharacterClassTypes, b <- [True, False]] ++ [CharacterClass ctype False | ctype <- allMChar]
allStrings = tail ((take 4 . inits . repeat) "abcABC \\\"'" >>= sequence)

wNum    f = [f n | n <- allNumerics]
wChar   f = [f n | n <- [' '..'Z']]
wCClass f = [f n | n <- allCharacterClass]

allRules = [ RejectUnlessCaseSensitive -- -c
           , RejectUnless8Bits         -- -8
           , RejectUnlessSplit         -- -s
           , RejectUnlessWordPairs     -- -p
           , LowerCase
           , UpperCase
           , Capitalize
           , ToggleAllCase
           , Reverse
           , Duplicate
           , Reflect
           , RotateLeft
           , RotateRight
           , Pluralize
           , PastTense
           , Genitive
           , ShiftCase
           , LowerVowels
           , DeleteFirst
           , ShiftRightKeyboard
           , ShiftLeftKeyboard
           , RejectUnlessChanged
           , Memorize
           , DeleteLast ]
           ++ concatMap wNum
           [ ToggleCase
           , Delete
           , RejectUnlessLengthLess
           , RejectUnlessLengthMore
           , Truncate
           ]
           ++ concatMap wChar
           [ Append
           , Prepend
           ]
           ++ [Extract a b    | a <- allNumerics, b <- allNumerics]
           ++ [Insert a b     | a <- allNumerics, b <- [' '..'Z'] ]
           ++ [Overstrike a b | a <- allNumerics, b <- [' '..'Z'] ]
           ++ [ExtractInsert a b c | a <- allNumerics, b <- allNumerics, c <- allNumerics]
           ++ [Update a b c        | a <- allNumerics, b <- allNumerics, c <- allNumerics]
           ++ concatMap wCClass
           [ RejectUnlessFirstChar
           , RejectUnlessLastChar
           , PurgeAll
           , RejectIfContains
           , RejectUnlessContains
           ]
           ++ [InsertString n s | n <- allNumerics, s <- allStrings]
           ++ [RejectUnlessCharInPos n c | n <- allNumerics, c <- allCharacterClass]
           ++ [RejectUnlessNInstances n c | n <- allNumerics, c <- allCharacterClass]
           ++ [ReplaceAll cc c | cc <- allCharacterClass, c <- [' '..'Z']]

instance Arbitrary Rule where
    arbitrary = elements allRules

testJTR :: [Rule] -> Bool
testJTR r = let
    rc  = cleanup r
    str = showRule JTR rc
    rev = case str of
            Right "" -> parseSingleRule JTR ":"
            Right s  -> parseSingleRule JTR s
            Left  e  -> [Left e]
    l' = lefts rev
    r' = rights rev
    in if (null l') && (length r' == 1)
        then head r' == rc
        else False

main = verboseCheck testJTR
