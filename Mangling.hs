module Mangling where

import Text.Parsec
import Text.Parsec.Char
import Control.Applicative hiding (many, (<|>))
import Data.Char (ord,chr,isSpace,digitToInt,isDigit,toLower,isUpper)
import System.IO

import Debug.Trace

{- PREPROCESSOR IMPLENTATION -}
type PreParser = Parsec String ()
data PBlock = Raw String | ProcessorBlock String
    deriving (Show, Ord, Eq)

blockparser :: PreParser [PBlock]
blockparser = do
    s <- many (noneOf "\\[")
    n <- optionMaybe anyChar
    case n of
        Nothing     -> return [Raw s]
        Just '\\'   -> docheckEscape s
        Just '['    -> fmap ((Raw s):) blockparser'

blockparser' :: PreParser [PBlock]
blockparser' = do
    s <- many (noneOf "\\]")
    n <- optionMaybe anyChar
    case n of
        Nothing     -> return [ProcessorBlock s]
        Just '\\'   -> do
            n' <- anyChar
            fmap (concatBlocks (ProcessorBlock (s ++ [n']))) blockparser'
        Just ']'    -> fmap ((ProcessorBlock s):) blockparser


concatBlocks :: PBlock -> [PBlock] -> [PBlock]
concatBlocks (Raw cur)              ((Raw nxt):xs)              = (Raw (cur ++ nxt)) : xs
concatBlocks (ProcessorBlock cur)   ((ProcessorBlock nxt):xs)   = (ProcessorBlock (cur ++ nxt)) : xs
concatBlocks cur                    next                        = cur : next

-- do handle the escape sequence, and then return the right PBlock
docheckEscape :: String -> PreParser [PBlock]
docheckEscape curstring = do
    n <- optionMaybe anyChar
    case n of
        Just x -> fmap (concatBlocks (Raw (curstring ++ [x]))) blockparser
        Nothing -> return [Raw (curstring ++ ['\\'])]

preprocess :: String -> [String]
preprocess str =
    let pstr = case runP (blockparser) () "" str of
                   Right x -> x
                   Left  r -> [Raw str]
        tstr = map tok2str pstr
        tok2str :: PBlock -> [[String]]
        tok2str (Raw x) = [[x]]
        tok2str (ProcessorBlock x) = foldl (\cur c -> cur ++ [[[c]]]) [] x
    in map (concat . concat) $ sequence tstr

{- ACTUAL STUFF -}

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


data CharacterClassType
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

-- true = match, false = exclude
data CharacterClass = CharacterClass CharacterClassType Bool
    deriving (Show, Ord, Eq)

charclass :: Parser CharacterClass
charclass = do
    c <- anyChar
    hashcat <- getState
    if hashcat
        then return $ CharacterClass (MatchChar c) False
        else case c of
            '?' -> anyChar >>= \l ->
                    let exclude = isUpper l
                    in  case (toLower l) of
                            '?' -> return $ CharacterClass MatchQuestionMark exclude
                            'v' -> return $ CharacterClass MatchVowel exclude
                            'c' -> return $ CharacterClass MatchConsonant exclude
                            'w' -> return $ CharacterClass MatchWhitespace exclude
                            'p' -> return $ CharacterClass MatchPunctuation exclude
                            's' -> return $ CharacterClass MatchSymbol exclude
                            'l' -> return $ CharacterClass MatchLower exclude
                            'u' -> return $ CharacterClass MatchUpper exclude
                            'd' -> return $ CharacterClass MatchDigit exclude
                            'a' -> return $ CharacterClass MatchLetter exclude
                            'x' -> return $ CharacterClass MatchAlphaNum exclude
                            'z' -> return $ CharacterClass MatchAny exclude
                            _   -> unexpected $ "Unknown character class " ++ [l]
            _   -> return $ CharacterClass (MatchChar c) False

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
    | AppendString Numeric String
    | Prepend Char
    | RejectUnlessLengthLess Numeric
    | RejectUnlessLengthMore Numeric
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

doubleQuotedString :: Parser String
doubleQuotedString = char '"' >> doubleQuotedStringContent

doubleQuotedStringContent = do
    c1 <- optionMaybe anyChar
    case c1 of
        Just '\\' -> do
            c2 <- anyChar
            n <- doubleQuotedStringContent
            return (c2:n)
        Just '"' -> return ""
        Just x -> do
            n <- doubleQuotedStringContent
            return (x:n)
        Nothing -> return ""

hashcatrules :: Char -> Parser [Rule]
hashcatrules c = do
    case c of
        'z' -> (return . H . DuplicateFirstN) <$> numeric
        'Z' -> (return . H . DuplicateLastN) <$> numeric
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
        'A' -> if hashcat
                   then unexpected "Unknown rule A"
                   else return <$> (AppendString <$> numeric <*> doubleQuotedString)
        '<' -> (return . RejectUnlessLengthLess) <$> numeric
        '>' -> (return . RejectUnlessLengthMore) <$> numeric
        '[' -> return [DeleteFirst]
        ']' -> return [DeleteLast]
        '\'' -> (return . Truncate) <$> numeric
        'p' -> if hashcat
                    then (return . H . DuplicateWord) <$> numeric
                    else return [Pluralize]
        'P' -> return [PastTense]
        'I' -> return [Genitive]
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
                    else unexpected $ "Unknown rule '" ++ c : "'"
    spaces
    return r

parserule :: Bool -> String -> Either String [Rule]
parserule hashcat line = case runP (spaces >> many1 rule) hashcat "rule" line of
                Right x -> Right $ cleanup $ concat x
                Left  r -> Left  $ show r

removeNBPWD :: String -> String
removeNBPWD str =
    let rstr = dropWhile isDigit $ reverse str
    in  case rstr of
            ('=':'D':'W':'P':'B':'N':' ':xs) -> reverse xs
            otherwise                        -> str

cleanup :: [Rule] -> [Rule]
cleanup = filter (not . isNoop)

isNoop :: Rule -> Bool
isNoop Noop = True
isNoop _    = False


parseRuleFile :: Bool -> String -> IO [Either String [Rule]]
parseRuleFile hashcat fname = do
    let isCommentOrEmpty ""         = True
        isCommentOrEmpty ('#':_)    = True
        isCommentOrEmpty x          = all isSpace x
    rawrules <- fmap (filter (not . isCommentOrEmpty) . map removeNBPWD . lines) (readFile fname)
    let processedrules = if hashcat
                             then rawrules
                             else concatMap preprocess rawrules
        parsed = map (parserule hashcat) processedrules
        paired = zip3 [1..] rawrules parsed
        niceError (l, raw, Left err) = Left (err ++ "\n" ++ raw ++ "\nline " ++ show l)
        niceError (_, _  , Right x ) = Right x
    return $ map niceError paired

hashcat2jtr :: Rule -> Maybe [Rule]
hashcat2jtr (H _) = Nothing
hashcat2jtr r = Just [r]
