module Main where

import Test.QuickCheck
import Mangling
import Data.Either (lefts, rights)

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

allCharacterClassTypes = [ MatchQuestionMark
                         , MatchVowel
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

wNum  f = [f n | n <- allNumerics]
wChar f = [f n | n <- [' '..'Z']]

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

{-
           , AppendString Numeric String
           , InsertString Numeric String

           , ExtractInsert Numeric Numeric Numeric
           , Update Numeric Numeric Numeric

           , ReplaceAll CharacterClass Char
           , RejectUnlessFirstChar CharacterClass
           , RejectUnlessLastChar CharacterClass
           , PurgeAll CharacterClass
           , RejectIfContains CharacterClass
           , RejectUnlessContains CharacterClass

           , RejectUnlessCharInPos Numeric CharacterClass
           , RejectUnlessNInstances Numeric CharacterClass
-}

instance Arbitrary Rule where
    arbitrary = elements allRules

testJTR :: [Rule] -> Bool
testJTR r = let
    str = showRule JTR r
    rev = case str of
            Right "" -> parseSingleRule JTR ":"
            Right s  -> parseSingleRule JTR s
            Left  e  -> [Left e]
    l' = lefts rev
    r' = rights rev
    in if (null l') && (length r' == 1)
        then head r' == r
        else False

main = verboseCheck testJTR
