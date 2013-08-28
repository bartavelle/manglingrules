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
import Data.Maybe (mapMaybe)
import Data.List (intercalate)

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
    let cleans = (concatMap cleanup' $ mapMaybe optimizeSingle $ optimizePairs rules) & biplate %~ characterClassClean
        l = last cleans
        characterClassClean (MatchChar '?') = MatchQuestionMark
        characterClassClean x = x
        optimizeSingle :: Rule -> Maybe Rule
        optimizeSingle (H (AsciiDecrement (Intval 0))) = Nothing
        optimizeSingle (H (AsciiIncrement (Intval 0))) = Nothing
        optimizeSingle x@(H (AsciiDecrement (Intval n))) = if n < 0
                                                               then Just (H (AsciiIncrement (Intval (negate n))))
                                                               else Just x
        optimizeSingle x@(H (AsciiIncrement (Intval n))) = if n < 0
                                                               then Just (H (AsciiDecrement (Intval (negate n))))
                                                               else Just x
        optimizeSingle (H (BitwiseLeft (Intval 0))) = Nothing
        optimizeSingle (H (BitwiseRight (Intval 0))) = Nothing
        optimizeSingle x@(H (BitwiseLeft (Intval n))) = if n < 0
                                                               then Just (H (BitwiseRight (Intval (negate n))))
                                                               else Just x
        optimizeSingle x@(H (BitwiseRight (Intval n))) = if n < 0
                                                               then Just (H (BitwiseLeft (Intval (negate n))))
                                                               else Just x
        optimizeSingle (InsertString x [c]) = Just (Insert x c)
        optimizeSingle x = Just x
        optimizePairs :: [Rule] -> [Rule]
        optimizePairs (a:b:xs) = case optimizePair a b of
                                     Just n -> optimizePairs (n : xs)
                                     Nothing -> a : optimizePairs (b : xs)
        optimizePairs x = x
        optimizePair :: Rule -> Rule -> Maybe Rule
        optimizePair (H (AsciiDecrement (Intval a))) (H (AsciiDecrement (Intval b))) = Just $ H (AsciiDecrement (Intval (a+b)))
        optimizePair (H (AsciiIncrement (Intval a))) (H (AsciiIncrement (Intval b))) = Just $ H (AsciiIncrement (Intval (a+b)))
        optimizePair (H (AsciiDecrement (Intval a))) (H (AsciiIncrement (Intval b))) = Just $ H (AsciiDecrement (Intval (a-b)))
        optimizePair (H (AsciiIncrement (Intval a))) (H (AsciiDecrement (Intval b))) = Just $ H (AsciiIncrement (Intval (a-b)))
        optimizePair (H (BitwiseLeft (Intval a))) (H (BitwiseLeft (Intval b))) = Just $ H (BitwiseLeft (Intval (a+b)))
        optimizePair (H (BitwiseRight (Intval a))) (H (BitwiseRight (Intval b))) = Just $ H (BitwiseRight (Intval (a+b)))
        optimizePair (H (BitwiseLeft (Intval a))) (H (BitwiseRight (Intval b))) = Just $ H (BitwiseLeft (Intval (a-b)))
        optimizePair (H (BitwiseRight (Intval a))) (H (BitwiseLeft (Intval b))) = Just $ H (BitwiseRight (Intval (a-b)))
        optimizePair _ _ = Nothing
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
    where
        escapeJTR' x | x `elem` "[]\\" = ['\\', x]
                     | otherwise = [x]

showDelimitedString :: String -> Either String String
showDelimitedString s =
    let acceptable = '"' : ['a'..'z'] ++ ['A'..'Z']
        valids = filter (`notElem` s) acceptable
    in case valids of
           []    -> Left "Impossible to select a valid character for a delimited string"
           (x:_) -> Right (x : s ++ [x])

showRule :: Flavor -> [Rule] -> Either String String
showRule _ []    = Right ""
showRule f rules = fmap (intercalate " " . mightescape) $ mapM (showRule' f) rules
    where
        mightescape = case f of
                          JTR -> map escapeJTR
                          _ -> id

fixpos :: Flavor -> Char -> Numeric -> Either String String
fixpos f c pos = fmap (\x -> [c, x]) $ showPos f pos

showRule' :: Flavor -> Rule -> Either String String
showRule' HashCat (H (AsciiIncrement (Intval a)) ) = fixpos HashCat '-' (Intval a)
showRule' HashCat (H (AsciiDecrement (Intval a)) ) = fixpos HashCat '+' (Intval a)
showRule' HashCat (H (BitwiseRight (Intval a))   ) = fixpos HashCat 'R' (Intval a)
showRule' HashCat (H (BitwiseLeft (Intval a))    ) = fixpos HashCat 'L' (Intval a)
showRule' _   (ReplaceAll cc c                   ) = return ('s' : showCC cc ++ [c])
showRule' _   (PurgeAll cc                       ) = return ('@' : showCC cc)
showRule' _   (Append c                          ) = return ['$',c]
showRule' _   (Prepend c                         ) = return ['^',c]
showRule' _   (LowerCase                         ) = return "l"
showRule' _   (RotateLeft                        ) = return "{"
showRule' _   (RotateRight                       ) = return "}"
showRule' _   (DeleteFirst                       ) = return "["
showRule' _   (DeleteLast                        ) = return "]"
showRule' _   (Capitalize                        ) = return "c"
showRule' _   (ToggleAllCase                     ) = return "t"
showRule' _   (Reverse                           ) = return "r"
showRule' _   (Duplicate                         ) = return "d"
showRule' _   (Reflect                           ) = return "f"
showRule' _   (UpperCase                         ) = return "u"
showRule' _   (LowerVowels                       ) = return "V"
showRule' _   (ShiftCase                         ) = return "S"
showRule' JTR (ShiftRightKeyboard                ) = return "R"
showRule' JTR (ShiftLeftKeyboard                 ) = return "L"
showRule' JTR (PastTense                         ) = return "P"
showRule' JTR (Genitive                          ) = return "I"
showRule' f   (Insert     p c                    ) = fmap (\x -> ['i', x, c]) $ showPos f p
showRule' f   (Overstrike p c                    ) = fmap (\x -> ['o', x, c]) $ showPos f p
showRule' JTR (Extract a b                       ) = do
    pa <- showPos JTR a
    pb <- showPos JTR b
    return ['x', pa, pb]
showRule' f   (Truncate pos                   ) = fixpos f '\'' pos
showRule' f   (ToggleCase pos                 ) = fixpos f 'T' pos
showRule' f   (Delete pos                     ) = fixpos f 'D' pos
showRule' JTR (H (DuplicateLastN (Intval x))  ) = return $ unwords ("val1" : replicate x "Xa1a")
showRule' f   (H (DuplicateLastN pos)         ) = fixpos f 'Z' pos
showRule' JTR (H (DuplicateFirstN (Intval x)) ) = return $ unwords (replicate x "X010")
showRule' f   (H (DuplicateFirstN pos)        ) = fixpos f 'z' pos
showRule' JTR (H (DuplicateWord (Intval x))   ) = return $ unwords ("val0" : replicate x "X0aa")
showRule' f   (H (DuplicateWord pos)          ) = fixpos f 'p' pos
showRule' JTR (H SwapFront                    ) = return "X012 D0"
showRule' JTR (H (Swap p1 (Intval p2))        ) = do
    r1  <- showPos JTR p1
    r2  <- showPos JTR (Intval p2)
    r2' <- showPos JTR (Intval (p2+1))
    return ['X', r1, '1', r2, ' ', 'D', r1, ' ', 'X', r2, '1', r1, ' ', 'D', r2']
showRule' f   (H (Swap p1 p2)                ) = do
    r1 <- showPos f p1
    r2 <- showPos f p2
    return ['*', r1, r2]
showRule' HashCat (H DuplicateAll)          = return "q"
showRule' JTR RejectUnless8Bits             = return "-8"
showRule' JTR Noop                          = return ":"
showRule' JTR Memorize                      = return "M"
showRule' JTR Pluralize                     = return "p"
showRule' JTR RejectUnlessChanged           = return "Q"
showRule' JTR RejectUnlessCaseSensitive     = return "-c"
showRule' JTR RejectUnlessSplit             = return "-s"
showRule' JTR RejectUnlessWordPairs         = return "-p"
showRule' JTR (RejectIfContains cc)         = return ('!' : showCC cc)
showRule' JTR (RejectUnlessContains cc)     = return ('/' : showCC cc)
showRule' JTR (RejectUnlessFirstChar cc)    = return ('(' : showCC cc)
showRule' JTR (RejectUnlessLastChar cc)     = return (')' : showCC cc)
showRule' JTR (RejectUnlessCharInPos p cc)  = fmap (\x -> '=' : x : showCC cc) (showPos JTR p)
showRule' JTR (RejectUnlessNInstances p cc) = fmap (\x -> 'N' : x : showCC cc) (showPos JTR p)
showRule' JTR (RejectUnlessLengthMore pos)  = fixpos JTR '>' pos
showRule' JTR (RejectUnlessLengthLess pos)  = fixpos JTR '<' pos
showRule' JTR (InsertString pos tr)         = do
    p <- showPos JTR pos
    s <- showDelimitedString tr
    return ('A' : p : s)
showRule' JTR (Update a b c)                  = do
    pa <- showPos JTR a
    pb <- showPos JTR b
    pc <- showPos JTR c
    return ('v':pa:pb:[pc])
showRule' JTR (ExtractInsert a b c)           = do
    pa <- showPos JTR a
    pb <- showPos JTR b
    pc <- showPos JTR c
    return ('X':pa:pb:[pc])
showRule' JTR (H x) = throwError $ "Can't translate this specific Hashcat rule: " ++ show x
showRule' _ x = throwError $ "Can't decode: " ++ show x

