module Mangling where

import Text.Parsec
import Text.Parsec.Char
import Control.Applicative hiding (many)
import Data.Char (ord,chr,isSpace,digitToInt)
import System.IO

import Debug.Trace

type Parser = Parsec String Bool

data Numeric
    = Intval Int
    | MaxLength
    | MaxLengthMinus1
    | MaxLengthPlus1
    | Variable Int
    | CurrentWordLength
    | InitialLastCharPos
    | PosFoundChar
    | Infinite
    | Unknown -- used for error handling
    deriving (Show, Ord, Eq)

numeric :: Parser Numeric
numeric = do
    c <- anyChar
    let output | c `elem` ['0'..'9']    = Intval (ord c - ord '0')
               | c `elem` ['A'..'Z']    = Intval (ord c - ord 'A' + 10)
               | c `elem` ['a'..'k']    = Variable (ord c - ord 'a')
               | c == '*'               = MaxLength
               | c == '-'               = MaxLengthMinus1
               | c == '+'               = MaxLengthPlus1
               | c == 'l'               = CurrentWordLength
               | c == 'm'               = InitialLastCharPos
               | c == 'p'               = PosFoundChar
               | c == 'z'               = Infinite
               | otherwise              = Unknown
    case output of
        Unknown -> unexpected "Can't decode position"
        _       -> return output


data CharacterClass
    = MatchQuestionMark
    | MatchVowel
    | MatchConsonant
    | MatchWhitespace
    | MatchPunctuation
    | MatchSymbol
    | MatchLower
    | MatchUpper
    | MatchDigit
    | MatchLetter
    | MatchAlphaNum
    | MatchAny
    | MatchChar Char
    deriving (Show, Ord, Eq)

charclass :: Parser CharacterClass
charclass = do
    c <- anyChar
    hashcat <- getState
    if hashcat
        then return $ MatchChar c
        else case c of
            '?' -> anyChar >>= \l -> case l of
                '?' -> return MatchQuestionMark
                'v' -> return MatchVowel
                'c' -> return MatchConsonant
                'w' -> return MatchWhitespace
                'p' -> return MatchPunctuation
                's' -> return MatchSymbol
                'l' -> return MatchLower
                'u' -> return MatchUpper
                'd' -> return MatchDigit
                'a' -> return MatchLetter
                'x' -> return MatchAlphaNum
                'z' -> return MatchAny
                _   -> unexpected $ "Unknown character class " ++ [l]
            _   -> return $ MatchChar c

data Rule
    = Noop
    | RejectUnlessCaseSensitive -- -c
    | RejectUnless8Bits         -- -8
    | RejectUnlessSplit         -- -s
    | RejectUnlessWordPairs     -- -p
    | LowerCase
    | UpperCase
    | Capitalize
    | ToggleAllCase
    | ToggleCase Numeric
    | Reverse
    | Duplicate
    | Reflect
    | RotateLeft
    | RotateRight
    | Append Char
    | Prepend Char
    | RejectLengthLess Numeric
    | RejectLengthMore Numeric
    | Truncate Numeric
    | Pluralize
    | PastTense
    | Genitive
    | DeleteFirst
    | DeleteLast
    | Delete Numeric
    | Extract Numeric Numeric
    | Insert Numeric Char
    | InsertString Numeric String
    | Overstrike Numeric Char
    | ShiftCase
    | LowerVowels
    | ShiftRightKeyboard
    | ShiftLeftKeyboard
    | Memorize
    | RejectUnlessChanged
    | ExtractInsert Numeric Numeric Numeric
    | Update Numeric Numeric Numeric
    | ReplaceAll CharacterClass Char
    | PurgeAll CharacterClass
    | RejectIfContains CharacterClass
    | RejectUnlessContains CharacterClass
    | RejectUnlessCharInPos Numeric CharacterClass
    | RejectUnlessFirstChar CharacterClass
    | RejectUnlessLastChar CharacterClass
    | RejectUnlessNInstances Numeric CharacterClass
    | H HashcatRule
    deriving (Show, Ord, Eq)

data HashcatRule
    = DuplicateFirstN Numeric
    | DuplicateLastN Numeric
    | DuplicateAll
    | DuplicateWord Numeric
    | BitwiseLeft Numeric
    | BitwiseRight Numeric
    | AsciiIncrement Numeric
    | AsciiDecrement Numeric
    | SwapFront
    | SwapBack
    | Swap Numeric Numeric
    deriving (Show, Ord, Eq)

singlechar :: Parser Char
singlechar = anyChar

escapeddelete :: Parser [Rule]
escapeddelete = do
    hashcat <- getState
    if hashcat
        then unexpected "Unknown rule"
        else do
                n <- anyChar
                case n of
                    '[' -> return [DeleteFirst]
                    ']' -> return [DeleteLast]
                    _   -> unexpected "Was expecting an escaped rule, such as \\[ or \\]"

hashcatrules :: Char -> Parser [Rule]
hashcatrules c = do
    case c of
        'z' -> (return . H . DuplicateFirstN) <$> numeric
        'Z' -> (return . H . DuplicateLastN) <$> numeric
        '[' -> return [DeleteFirst]
        ']' -> return [DeleteLast]
        '+' -> (return . H . AsciiIncrement) <$> numeric
        'k' -> return [H SwapFront]
        'K' -> return [H SwapBack]
        '*' -> (return . H) <$> (Swap <$> numeric <*> numeric)
        'q' -> return [H DuplicateAll]
        _ -> unexpected $ "Unknown rule (even hashcat) : " ++ [c]

rule :: Parser [Rule]
rule = do
    c <- anyChar
    hashcat <- getState
    r <- case c of
        '-' -> if hashcat
                then (return . H . AsciiDecrement) <$> numeric
                else anyChar >>= \c -> case c of
                                    '8' -> return [RejectUnless8Bits]
                                    ':' -> return [Noop]
                                    'c' -> return [RejectUnlessCaseSensitive]
                                    's' -> return [RejectUnlessSplit]
                                    'p' -> return [RejectUnlessWordPairs]
                                    _   -> unexpected "Unknown reject rule"
        ':' -> return [Noop]
        'l' -> return [LowerCase]
        'u' -> return [UpperCase]
        'c' -> return [Capitalize]
        'C' -> return [UpperCase, ToggleCase (Intval 0)]
        't' -> return [ToggleAllCase]
        'T' -> (return . ToggleCase) <$> numeric
        'r' -> return [Reverse]
        'd' -> return [Duplicate]
        'f' -> return [Reflect]
        '{' -> return [RotateLeft]
        '}' -> return [RotateRight]
        '$' -> (return . Append)  <$> singlechar
        '^' -> (return . Prepend) <$> singlechar
        'A' -> unexpected "AN\"xxx\""
        '<' -> (return . RejectLengthLess) <$> numeric
        '>' -> (return . RejectLengthMore) <$> numeric
        '\'' -> (return . Truncate) <$> numeric
        'p' -> if hashcat
                    then (return . H . DuplicateWord) <$> numeric
                    else return [Pluralize]
        'P' -> return [PastTense]
        'I' -> return [Genitive]
        '\\' -> escapeddelete
        'D' -> (return . Delete) <$> numeric
        'x' -> return <$> (Extract <$> numeric <*> numeric)
        'i' -> return <$> (Insert <$> numeric <*> singlechar)
        'o' -> return <$> (Overstrike <$> numeric <*> singlechar)
        'S' -> return [ShiftCase]
        'V' -> return [LowerVowels]
        'R' -> if hashcat
                    then (return . H . BitwiseRight) <$> numeric
                    else return [ShiftRightKeyboard]
        'L' -> if hashcat
                    then (return . H . BitwiseLeft) <$> numeric
                    else return [ShiftLeftKeyboard]
        'M' -> return [Memorize]
        'Q' -> return [RejectUnlessChanged]
        'X' -> return <$> (ExtractInsert <$> numeric <*> numeric <*> numeric)
        'v' -> return <$> (Update <$> numeric <*> numeric <*> numeric)
        's' -> return <$> (ReplaceAll <$> charclass <*> anyChar)
        '@' -> (return . PurgeAll) <$> charclass
        '!' -> (return . RejectIfContains) <$> charclass
        '/' -> (return . RejectUnlessContains) <$> charclass
        '=' -> return <$> (RejectUnlessCharInPos <$> numeric <*> charclass)
        '(' -> (return . RejectUnlessFirstChar) <$> charclass
        ')' -> (return . RejectUnlessLastChar) <$> charclass
        'N' -> return <$> (RejectUnlessNInstances <$> numeric <*> charclass)
        _   -> if hashcat
                    then hashcatrules c
                    else unexpected "Unknown rule"
    spaces
    return r

parserule :: Bool -> String -> Either String [Rule]
parserule hashcat line = case runP (many1 rule) hashcat "rule" line of
                    Right x -> Right $ concat x
                    Left  r -> Left  $ show r

parseRuleFile :: Bool -> String -> IO [Either String [Rule]]
parseRuleFile hashcat fname = do
    let isCommentOrEmpty ""         = True
        isCommentOrEmpty ('#':_)    = True
        isCommentOrEmpty x          = all isSpace x
    rawrules <- fmap (filter (not . isCommentOrEmpty) . lines) (readFile fname)
    let parsed = map (parserule hashcat) rawrules
        paired = zip3 [1..] rawrules parsed
        niceError (l, raw, Left err) = Left (err ++ "\n" ++ raw ++ "\nline " ++ show l)
        niceError (_, _  , Right x ) = Right x
    return $ map niceError paired

hashcat2jtr :: Rule -> Maybe [Rule]
hashcat2jtr (H _) = Nothing
hashcat2jtr r = Just [r]
