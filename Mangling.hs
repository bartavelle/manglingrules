{-# LANGUAGE DeriveDataTypeable #-}
module Mangling (
        preprocess,
        cleanup,
        parseRuleFile,
        parseSingleRule,
        showRule,
        Flavor (..),
        Rule (..),
        Numeric (..),
        CharacterClassType (..),
        CharacterClass (..)
    ) where

import Text.Parsec
import Control.Applicative hiding (many, (<|>))
import Data.Char (ord,chr,isSpace,isDigit,toLower,isUpper,toUpper)
import Control.Monad.Error
import Control.Lens
import Data.Data.Lens
import Data.Typeable
import Data.Data

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
        Just '['    -> fmap ( Raw s :) blockparser'
        Just x      -> error $ "Can't happen at blockparser : " ++ [x]

blockparser' :: PreParser [PBlock]
blockparser' = do
    s <- many (noneOf "\\]")
    n <- optionMaybe anyChar
    case n of
        Nothing     -> return [ProcessorBlock s]
        Just '\\'   -> do
            n' <- anyChar
            fmap (concatBlocks (ProcessorBlock (s ++ [n']))) blockparser'
        Just ']'    -> fmap ( ProcessorBlock s :) blockparser
        Just x      -> error $ "Can't happen at blockparser' : " ++ [x]


concatBlocks :: PBlock -> [PBlock] -> [PBlock]
concatBlocks (Raw cur)              (Raw nxt : xs)              = Raw (cur ++ nxt) : xs
concatBlocks (ProcessorBlock cur)   (ProcessorBlock nxt : xs)   = ProcessorBlock (cur ++ nxt) : xs
concatBlocks cur                    next                        = cur : next

-- do handle the escape sequence, and then return the right PBlock
docheckEscape :: String -> PreParser [PBlock]
docheckEscape curstring = do
    n <- optionMaybe anyChar
    case n of
        Just x -> fmap (concatBlocks (Raw (curstring ++ [x]))) blockparser
        Nothing -> return [Raw (curstring ++ "\\")]

preprocess :: String -> [String]
preprocess str =
    let pstr = case runP blockparser () "" str of
                   Right x -> x
                   Left  _ -> [Raw str]
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
    deriving (Show, Ord, Eq, Typeable, Data)

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

showPos :: Flavor -> Numeric -> Either String Char
showPos _ (Intval x) | (x>=0) && (x<10) = Right $ chr (x + ord '0')
                     | (x>=10) && (x<36) = Right $ chr (x + ord 'A' - 10)
                     | otherwise = Left $ "Invalid Intval " ++ show x
showPos JTR (Variable x) | x <= 10  = Right $ chr (x + ord 'a')
                         | otherwise = Left $ "Bad value for variable " ++ show x
showPos JTR MaxLength           = Right '*'
showPos JTR MaxLengthMinus1     = Right '-'
showPos JTR MaxLengthPlus1      = Right '+'
showPos JTR CurrentWordLength   = Right 'l'
showPos JTR InitialLastCharPos  = Right 'm'
showPos JTR PosFoundChar        = Right 'p'
showPos JTR Infinite            = Right 'z'
showPos _ x = Left $ "Not yet implemented " ++ show x



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
    deriving (Show, Ord, Eq, Typeable, Data)

-- true = match, false = exclude
data CharacterClass = CharacterClass CharacterClassType Bool
    deriving (Show, Ord, Eq, Typeable, Data)

charclass :: Parser CharacterClass
charclass = do
    c <- anyChar
    flavor <- getState
    case flavor of
        JTR -> case c of
            '?' -> anyChar >>= \l ->
                    let exclude = isUpper l
                    in  case toLower l of
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


mu :: Char -> Bool -> String
mu c False = ['?', c]
mu c True  = ['?', toUpper c]

showCC :: CharacterClass -> String
showCC (CharacterClass (MatchChar '?') _) = "??"
showCC (CharacterClass (MatchChar c) _) = [c]
showCC (CharacterClass MatchQuestionMark _) = "??"
showCC (CharacterClass MatchVowel x)        = mu 'v' x
showCC (CharacterClass MatchConsonant x)    = mu 'c' x
showCC (CharacterClass MatchWhitespace x)   = mu 'w' x
showCC (CharacterClass MatchPunctuation x)  = mu 'p' x
showCC (CharacterClass MatchSymbol x)       = mu 's' x
showCC (CharacterClass MatchLower x)        = mu 'l' x
showCC (CharacterClass MatchUpper x)        = mu 'u' x
showCC (CharacterClass MatchDigit x)        = mu 'd' x
showCC (CharacterClass MatchLetter x)       = mu 'a' x
showCC (CharacterClass MatchAlphaNum x)     = mu 'x' x
showCC (CharacterClass MatchAny x)          = mu 'z' x

data Rule
    = Noop
    | RejectUnlessCaseSensitive
    | RejectUnless8Bits
    | RejectUnlessSplit
    | RejectUnlessWordPairs
    | LowerCase
    | UpperCase
    | Capitalize
    | ToggleAllCase
    | Reverse
    | Duplicate
    | Reflect
    | RotateLeft
    | RotateRight
    | Pluralize
    | PastTense
    | Genitive
    | DeleteFirst
    | DeleteLast
    | ShiftCase
    | LowerVowels
    | ShiftRightKeyboard
    | ShiftLeftKeyboard
    | Memorize
    | RejectUnlessChanged
    | Append Char
    | Prepend Char
    | RejectUnlessLengthLess Numeric
    | RejectUnlessLengthMore Numeric
    | ToggleCase Numeric
    | Truncate Numeric
    | Delete Numeric
    | Extract Numeric Numeric
    | Insert Numeric Char
    | Overstrike Numeric Char
    | InsertString Numeric String
    | Update Numeric Numeric Numeric
    | ExtractInsert Numeric Numeric Numeric
    | PurgeAll CharacterClass
    | RejectIfContains CharacterClass
    | RejectUnlessContains CharacterClass
    | RejectUnlessFirstChar CharacterClass
    | RejectUnlessLastChar CharacterClass
    | RejectUnlessNInstances Numeric CharacterClass
    | RejectUnlessCharInPos Numeric CharacterClass
    | ReplaceAll CharacterClass Char
    | H HashcatRule
    deriving (Show, Ord, Eq, Typeable, Data)

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
    deriving (Show, Ord, Eq, Typeable, Data)

singlechar :: Parser Char
singlechar = anyChar

quotedString :: Parser String
quotedString = do
    separator <- anyChar
    content   <- many1 $ satisfy (/= separator)
    _ <- char separator
    return content

hashcatrules :: Char -> Parser [Rule]
hashcatrules c = case c of
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
                  JTR -> anyChar >>= \c' -> case c' of
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
                   JTR -> return <$> (InsertString <$> numeric <*> quotedString)
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
            _                                -> str

cleanup :: [Rule] -> [Rule]
cleanup rules =
    let cleans = concatMap cleanup' rules & biplate %~ characterClassClean
        l = last cleans
        characterClassClean (MatchChar '?') = MatchQuestionMark
        characterClassClean x = x
    in case (cleans, l) of
           ([], _)               -> [Noop]
           (_, Append ' ')       -> cleans ++ [Noop]
           (_, Prepend ' ')      -> cleans ++ [Noop]
           (_, Insert _ ' ')     -> cleans ++ [Noop]
           (_, Overstrike _ ' ') -> cleans ++ [Noop]
           _                     -> cleans

cleanup' :: Rule -> [Rule]
cleanup' Noop = []
cleanup' (InsertString Infinite   [c]) = [Append c]
cleanup' (InsertString (Intval 0) [c]) = [Prepend c]
cleanup' x    = [x]

parseSingleRule :: Flavor -> String -> [Either String [Rule]]
parseSingleRule flavor crule = case flavor of
                                JTR -> map (parserule flavor) $ preprocess crule
                                _   -> [parserule flavor crule]

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
        paired = zip3 ([1..] :: [Int]) processedrules parsed
        niceError (l, raw, Left err) = Left (err ++ "\n" ++ raw ++ "\nline " ++ show l)
        niceError (_, _  , Right x ) = Right x
    return $ map niceError paired

escapeJTR :: String -> String
escapeJTR = concatMap escapeJTR'

escapeJTR' :: Char -> String
escapeJTR' '['  = "\\["
escapeJTR' ']'  = "\\]"
escapeJTR' '\\' = "\\\\"
escapeJTR' x    = [x]

showDelimitedString :: String -> Either String String
showDelimitedString s =
    let acceptable = '"' : ['a'..'z'] ++ ['A'..'Z']
        valids = filter (`notElem` s) acceptable
    in case valids of
           []    -> Left "Impossible to select a valid character for a delimited string"
           (x:_) -> Right (x : s ++ [x])

showRule :: Flavor -> [Rule] -> Either String String
showRule _ []    = Right ""
showRule f rules = do
    (curstring, nextrules) <- showRule' f rules
    let pstring = case f of
                        JTR -> escapeJTR curstring
                        _   -> curstring
    nextstring <- showRule f nextrules
    return $ if null nextstring
        then pstring
        else pstring ++ " " ++ nextstring

fixpos :: Flavor -> Char -> Numeric -> [Rule] -> Either String (String, [Rule])
fixpos f c pos xs = fmap (\x -> ([c, x], xs)) $ showPos f pos

showRule' :: Flavor -> [Rule] -> Either String (String, [Rule])
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
showRule' _   (ReplaceAll cc c  : xs)   = return ('s' : showCC cc ++ [c], xs)
showRule' _   (PurgeAll cc      : xs)   = return ('@' : showCC cc, xs)
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
showRule' _   (LowerVowels      : xs)   = return ("V", xs)
showRule' _   (ShiftCase        : xs)   = return ("S", xs)
showRule' JTR (ShiftRightKeyboard:xs)   = return ("R", xs)
showRule' JTR (ShiftLeftKeyboard: xs)   = return ("L", xs)
showRule' JTR (PastTense        : xs)   = return ("P", xs)
showRule' JTR (Genitive         : xs)   = return ("I", xs)
showRule' f   (Insert     p c   : xs)   = showPos f p >>= \x -> return (['i', x, c], xs)
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
showRule' JTR (H SwapFront                    : xs) = return ( "X012 D0", xs)
showRule' JTR (H (Swap p1 (Intval p2))        : xs) = do
    r1  <- showPos JTR p1
    r2  <- showPos JTR (Intval p2)
    r2' <- showPos JTR (Intval (p2+1))
    return ( ['X', r1, '1', r2, ' ', 'D', r1, ' ', 'X', r2, '1', r1, ' ', 'D', r2'], xs)
showRule' f   (H (Swap p1 p2)                 : xs) = do
    r1 <- showPos f p1
    r2 <- showPos f p2
    return ( ['*', r1, r2], xs )
showRule' JTR (H DuplicateAll : _)                  = throwError "DuplicateAll (q in HashCat) is not supported for JTR yet."
showRule' _   (H DuplicateAll : xs)                 = return ("q", xs)
showRule' JTR (RejectUnless8Bits : xs)              = return ("-8", xs)
showRule' JTR (Noop : xs)                           = return (":", xs)
showRule' JTR (Memorize : xs)                       = return ("M", xs)
showRule' JTR (Pluralize : xs)                      = return ("p", xs)
showRule' JTR (RejectUnlessChanged : xs)            = return ("Q", xs)
showRule' JTR (RejectUnlessCaseSensitive : xs)      = return ("-c", xs)
showRule' JTR (RejectUnlessSplit : xs)              = return ("-s", xs)
showRule' JTR (RejectUnlessWordPairs : xs)          = return ("-p", xs)
showRule' JTR (RejectIfContains cc : xs)            = return ('!' : showCC cc, xs)
showRule' JTR (RejectUnlessContains cc : xs)        = return ('/' : showCC cc, xs)
showRule' JTR (RejectUnlessFirstChar cc : xs)       = return ('(' : showCC cc, xs)
showRule' JTR (RejectUnlessLastChar cc : xs)        = return (')' : showCC cc, xs)
showRule' JTR (RejectUnlessCharInPos p cc : xs)     = fmap (\x -> ('=' : x : showCC cc, xs)) (showPos JTR p)
showRule' JTR (RejectUnlessNInstances p cc : xs)    = fmap (\x -> ('N' : x : showCC cc, xs)) (showPos JTR p)
showRule' JTR (RejectUnlessLengthMore pos : xs)     = fixpos JTR '>' pos xs
showRule' JTR (RejectUnlessLengthLess pos : xs)     = fixpos JTR '<' pos xs
showRule' JTR (InsertString pos tr : xs)            = do
    p <- showPos JTR pos
    s <- showDelimitedString tr
    return ('A' : p : s, xs)
showRule' JTR (Update a b c : xs)                   = do
    pa <- showPos JTR a
    pb <- showPos JTR b
    pc <- showPos JTR c
    return ('v':pa:pb:[pc], xs)
showRule' JTR (ExtractInsert a b c : xs)            = do
    pa <- showPos JTR a
    pb <- showPos JTR b
    pc <- showPos JTR c
    return ('X':pa:pb:[pc], xs)
showRule' _ x = throwError $ "Can't decode: " ++ show x

