module Mangling where

import Text.Parsec
import Text.Parsec.Char
import Control.Applicative hiding (many, (<|>))
import Data.Char (ord,chr,isSpace,digitToInt,isDigit,toLower,isUpper)
import System.IO
import Control.Monad.Error

import Debug.Trace

data Flavor = JTR | HashCat
    deriving (Show, Ord, Eq)

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

type Parser = Parsec String Flavor

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
    flavor <- getState
    case flavor of
        JTR -> case c of
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
        _ -> return $ CharacterClass (MatchChar c) False

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
    flavor <- getState
    r <- case c of
        '-' -> case flavor of
                  HashCat -> (return . H . AsciiDecrement) <$> numeric
                  JTR -> anyChar >>= \c -> case c of
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
        'A' -> case flavor of
                   JTR -> return <$> (AppendString <$> numeric <*> doubleQuotedString)
                   _   -> unexpected "Unknown rule A"
        '<' -> (return . RejectUnlessLengthLess) <$> numeric
        '>' -> (return . RejectUnlessLengthMore) <$> numeric
        '[' -> return [DeleteFirst]
        ']' -> return [DeleteLast]
        '\'' -> (return . Truncate) <$> numeric
        'p' -> case flavor of
                   HashCat  -> (return . H . DuplicateWord) <$> numeric
                   _        -> return [Pluralize]
        'P' -> return [PastTense]
        'I' -> return [Genitive]
        'D' -> (return . Delete) <$> numeric
        'x' -> return <$> (Extract <$> numeric <*> numeric)
        'i' -> return <$> (Insert <$> numeric <*> singlechar)
        'o' -> return <$> (Overstrike <$> numeric <*> singlechar)
        'S' -> return [ShiftCase]
        'V' -> return [LowerVowels]
        'R' -> case flavor of
                    HashCat -> (return . H . BitwiseRight) <$> numeric
                    _       -> return [ShiftRightKeyboard]
        'L' -> case flavor of
                    HashCat -> (return . H . BitwiseLeft) <$> numeric
                    _       -> return [ShiftLeftKeyboard]
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
        _   -> case flavor of
                   HashCat -> hashcatrules c
                   _ -> unexpected $ "Unknown rule '" ++ c : "'"
    spaces
    return r

parserule :: Flavor -> String -> Either String [Rule]
parserule flavor line = case runP (spaces >> many1 rule) flavor "rule" line of
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


parseRuleFile :: Flavor -> String -> IO [Either String [Rule]]
parseRuleFile flavor fname = do
    let isCommentOrEmpty ""         = True
        isCommentOrEmpty ('#':_)    = True
        isCommentOrEmpty x          = all isSpace x
    rawrules <- fmap (filter (not . isCommentOrEmpty) . map removeNBPWD . lines) (readFile fname)
    let processedrules = case flavor of
                             JTR -> concatMap preprocess rawrules
                             _   -> rawrules
        parsed = map (parserule flavor) processedrules
        paired = zip3 [1..] rawrules parsed
        niceError (l, raw, Left err) = Left (err ++ "\n" ++ raw ++ "\nline " ++ show l)
        niceError (_, _  , Right x ) = Right x
    return $ map niceError paired

showPos :: Flavor -> Numeric -> Either String Char
showPos _ (Intval x) | (x>=0) && (x<10) = Right $ chr (x + ord '0')
                     | (x>=10) && (x<36) = Right $ chr (x + ord 'A' - 10)
                     | otherwise = Left $ "Invalid Intval " ++ show x
showPos _ x = Left $ "Not yet implemented " ++ show x

cnext :: Flavor -> [Rule] -> String -> Either String String
cnext f n c = case (showRule f n) of
                  Left x -> Left x
                  Right ns -> Right $ c ++ " " ++ ns

showRule :: Flavor -> [Rule] -> Either String String
showRule _ [] = Right ""
showRule f rules = do
    (curstring, nextrules) <- showRule' f rules
    nextstring <- showRule f nextrules
    return $ curstring ++ nextstring


fixpos f c pos xs = fmap (\x -> ([c, x], xs)) $ showPos f pos

showRule' _ [] = return ("", [])
showRule' f   (H (AsciiDecrement (Intval a)) : H (AsciiDecrement (Intval b)) : xs) = showRule' f (H (AsciiDecrement (Intval (a+b))) : xs)
showRule' f   (H (AsciiIncrement (Intval a)) : H (AsciiIncrement (Intval b)) : xs) = showRule' f (H (AsciiIncrement (Intval (a+b))) : xs)
showRule' f   (H (AsciiDecrement (Intval a)) : H (AsciiIncrement (Intval b)) : xs) | a == b    = showRule' f xs
                                                                                   | otherwise = showRule' f (H (AsciiDecrement (Intval (a-b))) : xs)
showRule' f   (H (AsciiIncrement (Intval a)) : H (AsciiDecrement (Intval b)) : xs) | a == b    = showRule' f xs
                                                                                   | otherwise = showRule' f (H (AsciiIncrement (Intval (a-b))) : xs)
showRule' f   (H (AsciiIncrement (Intval a)) : xs ) | a == 0          = showRule' f xs
                                                    | a < 0           = showRule' f (H (AsciiDecrement (Intval (negate a))) : xs)
                                                    | f == HashCat    = fixpos f '-' (Intval a) xs
                                                    | otherwise       = throwError "Ascii +/- operations not supported"
showRule' f   (H (AsciiDecrement (Intval a)) : xs ) | a == 0          = showRule' f xs
                                                    | a < 0           = showRule' f (H (AsciiIncrement (Intval (negate a))) : xs)
                                                    | f == HashCat    = fixpos f '+' (Intval a) xs
                                                    | otherwise       = throwError "Ascii +/- operations not supported"
showRule' f   (H (BitwiseLeft  (Intval a)) : H (BitwiseLeft (Intval b))  : xs) = showRule' f (H (BitwiseLeft (Intval (a+b))) : xs)
showRule' f   (H (BitwiseRight (Intval a)) : H (BitwiseRight (Intval b)) : xs) = showRule' f (H (BitwiseRight (Intval (a+b))) : xs)
showRule' f   (H (BitwiseLeft  (Intval a)) : H (BitwiseRight (Intval b)) : xs) | a == b    = showRule' f xs
                                                                               | otherwise = showRule' f (H (BitwiseLeft (Intval (a-b))) : xs)
showRule' f   (H (BitwiseRight (Intval a)) : H (BitwiseLeft (Intval b))  : xs) | a == b    = showRule' f xs
                                                                               | otherwise = showRule' f (H (BitwiseRight (Intval (a-b))) : xs)
showRule' f   (H (BitwiseRight (Intval a)) : xs ) | a == 0          = showRule' f xs
                                                  | a < 0           = showRule' f (H (BitwiseLeft (Intval (negate a))) : xs)
                                                  | f == HashCat    = fixpos f 'R' (Intval a) xs
                                                  | otherwise       = throwError "Bitwise operations not supported"
showRule' f   (H (BitwiseLeft (Intval a)) : xs )  | a == 0          = showRule' f xs
                                                  | a < 0           = showRule' f (H (BitwiseRight (Intval (negate a))) : xs)
                                                  | f == HashCat    = fixpos f 'L' (Intval a) xs
                                                  | otherwise       = throwError "Bitwise operations not supported"
showRule' JTR (Append ' '       : xs)   = return ("Az\" \"", xs)
showRule' JTR (Prepend ' '      : xs)   = return ("A0\" \"", xs)
showRule' _   (Append c         : xs)   = return (['$',c], xs)
showRule' _   (Prepend c        : xs)   = return (['^',c], xs)
showRule' _   (LowerCase        : xs)   = return ("l", xs)
showRule' _   (RotateLeft       : xs)   = return ("{", xs)
showRule' _   (RotateRight      : xs)   = return ("}", xs)
showRule' _   (DeleteFirst      : xs)   = return ("[", xs)
showRule' _   (DeleteLast       : xs)   = return ("]", xs)
showRule' _   (Capitalize       : xs)   = return ("c", xs)
showRule' _   (ToggleAllCase    : xs)   = return ("t", xs)
showRule' _   (Reverse          : xs)   = return ("r", xs)
showRule' _   (Duplicate        : xs)   = return ("d", xs)
showRule' _   (Reflect          : xs)   = return ("f", xs)
showRule' _   (UpperCase        : xs)   = return ("u", xs)
showRule' JTR (Insert     p ' ' : xs)   = showPos JTR p >>= \x -> return (['A', x] ++ "\" \"", xs)
showRule' f   (Insert     p c   : xs)   = showPos f p >>= \x -> return (['i', x, c], xs)
showRule' JTR (Overstrike p ' ' : xs)   = showPos JTR p >>= \x -> return (['D', x, ' ', 'A', x] ++ "\" \"", xs)
showRule' f   (Overstrike p c   : xs)   = showPos f p >>= \x -> return (['o', x, c], xs)
showRule' JTR (Extract a b      : xs)   = do
    pa <- showPos JTR a
    pb <- showPos JTR b
    return (['x', pa, pb], xs)
showRule' f   (Truncate pos     : xs)               = fixpos f '\'' pos xs
showRule' f   (ToggleCase pos   : xs)               = fixpos f 'T' pos xs
showRule' f   (Delete pos       : xs)               = fixpos f 'D' pos xs
showRule' JTR (H (DuplicateLastN (Intval x))  : xs) = return ( unwords ("val1" : replicate x "Xa1a"), xs )
showRule' f   (H (DuplicateLastN pos)         : xs) = fixpos f 'Z' pos xs
showRule' JTR (H (DuplicateFirstN (Intval x)) : xs) = return ( unwords (replicate x "X010"), xs )
showRule' f   (H (DuplicateFirstN pos)        : xs) = fixpos f 'z' pos xs
showRule' JTR (H (DuplicateWord (Intval x))   : xs) = return ( unwords ("val0" : replicate x "X0aa"), xs )
showRule' f   (H (DuplicateWord pos)          : xs) = fixpos f 'p' pos xs
showRule' JTR (H (SwapFront)                  : xs) = return ( "X012 D0", xs)
showRule' _ x = throwError $ "Can't decode: " ++ (show x)
