module Assignment (bnfParser, generateHaskellCode, validate, ADT(..), ruleName, getTime) where

import Instances (Parser (..))
import Data.Time (formatTime, defaultTimeLocale, getCurrentTime)
import Parser (satisfy, spaces)
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Control.Applicative ((<|>), many, some)
import Data.List (nub, sort, group, intercalate)

-- | Data Types Section
data ADT = Grammar [Rule] deriving (Show)

data Rule = Rule {
    ruleName :: String,
    ruleProductions :: [[Symbol]],
    ruleParameters :: [Char]
} deriving (Show)

newtype Production = Production [Symbol]
    deriving (Show)

data Symbol 
    = NonTerminal String [Symbol]
    | Terminal String 
    | Macro String
    | Group [[Symbol]]
    | Modified Modifier Symbol 
    deriving (Show)

data Modifier
    = Tok
    | ZeroOrMore
    | OneOrMore
    | Optional
    deriving (Show, Eq)

-- | Utility Functions

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
    first <- p 
    rest <- many $ do 
        _ <- sep 
        p
    pure (first : rest)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure [] 

string :: String -> Parser String
string [] = pure []
string (c:cs) = do
    x <- satisfy (== c)
    xs <- string cs
    pure (x:xs)

-- | Parsing Section

bnfParser :: Parser ADT
bnfParser = do
    _ <- spaces
    rules <- many ruleParser
    pure (Grammar rules)

-- | Parse a single grammar rule
-- each rule contains header, productions, and parameter validation 
ruleParser :: Parser Rule
ruleParser = do
    (names, parameters) <- ruleHeader
    _ <- spaces
    _ <- satisfy (== ':')
    _ <- satisfy (== ':')
    _ <- satisfy (== '=')
    _ <- spaces 
    productions <- sepBy1 productionParser (spaces *> satisfy (== '|') *> spaces)
    let allProds = concatMap (\(Production symbols) -> symbols) productions
    _ <- validateParamRef allProds parameters
    let rhs = fmap (\(Production symbols) -> symbols) productions
    _ <- many (satisfy (\c -> isSpace c && c /= '\n'))
    _ <- many (satisfy (== '\n'))
    pure (Rule names rhs parameters)

-- | Validate that all parameter references are declared
validateParamRef :: [Symbol] -> [Char] -> Parser ()
validateParamRef symbols parameters = 
    let declaredParameters = parameters
        usedParameters = extractParamRef symbols
        undeclared = filter (`notElem` declaredParameters) usedParameters
    in if null undeclared
        then pure ()
        else error ("Undeclared parameter references: " ++ show undeclared)

-- | Extract paramter references from symbols
extractParamRef :: [Symbol] -> [Char]
extractParamRef = nub . concatMap extractFromSymbol
    where
        extractFromSymbol (Macro [c])
            | isAlpha c && c >= 'a' && c <= 'z' = [c]
        extractFromSymbol (NonTerminal _ args) = concatMap extractFromSymbol args
        extractFromSymbol (Modified _ s) = extractFromSymbol s
        extractFromSymbol _ = []

-- | Parse rule header
ruleHeader :: Parser (String, [Char])
ruleHeader = 
        satisfy (== '<') >>= \_ ->
        some (satisfy (\c -> isAlphaNum c || c == '_')) >>= \name ->
        (parameterList <|> pure []) >>= \parameters ->
        satisfy (== '>') >>= \_ ->
        pure (name, parameters)

-- | Parser parameter list
parameterList :: Parser [Char]
parameterList = do
    _ <- satisfy (== '(')
    _ <- spaces
    parameters <- sepBy paramChar (spaces *> satisfy (== ',') <* spaces)
    _ <- spaces
    _ <- satisfy (== ')')
    pure parameters
  where
    paramChar = satisfy (\c -> isAlpha c && c >= 'a' && c <= 'z')

-- | Parse a sequence of symbols (production)
productionParser :: Parser Production
productionParser = do
    symbols <- some $ do
        symbol <- modifiedSymbolParser
        _ <- many (satisfy (\c -> isSpace c && c /= '\n'))
        pure symbol
    _ <- many (satisfy (\c -> isSpace c && c /= '\n'))
    pure (Production symbols)

-- | Parse a symbol with optional modifiers
modifiedSymbolParser :: Parser Symbol
modifiedSymbolParser = do
    maybeTok <- (Just Tok <$ (string "tok" *> spaces)) <|> pure Nothing
    baseSymbol <- symbolParser
    let tokAppliedSymbol = case maybeTok of
                            Just Tok -> Modified Tok baseSymbol
                            Nothing -> baseSymbol
                            _ -> baseSymbol 
    maybePostfixMod <- (Just ZeroOrMore <$ satisfy (== '*'))
                        <|> (Just OneOrMore <$ satisfy (== '+'))
                        <|> (Just Optional <$ satisfy (== '?'))
                        <|> pure Nothing 
    case maybePostfixMod of
        Just modifier -> pure (Modified modifier tokAppliedSymbol) 
        Nothing -> pure tokAppliedSymbol

-- | parse a symbol       
symbolParser :: Parser Symbol
symbolParser = 
    parameteredNonTerminal
    <|> nonTerminal
    <|> macro
    <|> quotedTerminal
    <|> groupParser

-- | Parse Parameterized nonTerminal
parameteredNonTerminal :: Parser Symbol
parameteredNonTerminal = 
    satisfy (== '<') >>= \_ ->
    some (satisfy (\c -> isAlphaNum c || c == '_')) >>= \name ->
    (paramArgsParser <|> pure []) >>= \args ->
    satisfy (== '>') >>= \_ ->
    pure (NonTerminal name args)

-- | Parse argument list for parameterized nonterminals
paramArgsParser :: Parser [Symbol]
paramArgsParser = do
    _ <- satisfy (== '(')
    _ <- spaces
    args <- sepBy paramArgumentParser (spaces *> satisfy (== ',') <* spaces)
    _ <- spaces
    _ <- satisfy (== ')')
    pure args

-- | Parse a single argument
paramArgumentParser :: Parser Symbol
paramArgumentParser = 
    paramRefParser <|> symbolParser

-- | Parse parameter reference
paramRefParser :: Parser Symbol
paramRefParser = do
    _ <- satisfy (== '[')
    name <- some $ satisfy (\c -> isAlpha c && c >= 'a' && c <= 'z')
    _ <- satisfy (== ']')
    pure (Macro name)

-- | Parse unparameterized nonterminal
nonTerminal :: Parser Symbol
nonTerminal = do 
    _ <- satisfy (== '<')
    name <- some $ satisfy (\c -> isAlphaNum c || c == '_')
    _ <- satisfy (== '>')
    pure (NonTerminal name [])

-- | Parse macro reference
macro :: Parser Symbol 
macro = do 
    _ <- satisfy (== '[')
    name <- some $ satisfy isAlpha 
    _ <- satisfy (== ']')
    pure (Macro name)

-- | Parse quoted terminal
quotedTerminal :: Parser Symbol
quotedTerminal = do 
    _ <- satisfy (== '"')
    content <- many $ satisfy (/= '"')
    _ <- satisfy (== '"')
    pure (Terminal content)

-- | Parse grouped alternatives: (a | b | c)
groupParser :: Parser Symbol
groupParser = do 
    _ <- satisfy (== '(')
    _ <- spaces 
    alts <- sepBy1 productionInsideGroup (spaces *> satisfy (== '|') <* spaces)
    _ <- spaces
    _ <- satisfy (== ')')
    pure (Group alts)

-- | Parse production inside group 
productionInsideGroup :: Parser [Symbol]
productionInsideGroup = do
    symbols <- many (modifiedSymbolParser <* spaces)
    pure symbols 

-- | Code Generation Section

-- | Generate complete Haskell code from grammar ADT
generateHaskellCode :: ADT -> String
generateHaskellCode (Grammar rules) =
    let cleanedRules = cleanRules rules
    in case cleanedRules of
        [] -> ""
        rs -> 
            let typeLines = concatMap genTypeDeclaration rs
                parserLines = concatMap genParserDeclaration rs
                allLines = typeLines ++ parserLines
            in unlines (reverse (dropWhile null (reverse allLines)))

-- | Generate type declarations for a rule
genTypeDeclaration :: Rule -> [String]
genTypeDeclaration rule = 
    let name = capitalize (ruleName rule)
        prods = ruleProductions rule 
        parameters = ruleParameters rule 
    in if null parameters
        then typeDeclaration name prods 
        else parameteredTypeDeclaration name parameters prods

-- | Generate simple type declaration 
typeDeclaration :: String -> [[Symbol]] -> [String]
typeDeclaration name prods = 
    case prods of 
        [prod] | isSingleField prod ->
            [generateNewType name prod, "    deriving Show", ""]
        _ -> generateDataType name prods ++ [""]

-- | Generate type declaration with type parameters
parameteredTypeDeclaration :: String -> [Char] -> [[Symbol]] -> [String]
parameteredTypeDeclaration name parameters prods =
    let typeVars = unwords ((\c -> [c]) <$> parameters)
        dataHeader = "data " ++ name ++ " " ++ typeVars ++ " = "
        constructors = zipWith (generateConstructor name) [1..] prods 
    in case constructors of
        [] -> []
        (first:rest) ->
            let headerLen = length dataHeader
                pipeIndent = replicate (headerLen - 2) ' ' -- make sure the indentation is correct
                firstLine = dataHeader ++ first
                restLines = [ pipeIndent ++ "| " ++ c | c <- rest ]
            in (firstLine : restLines) ++ ["    deriving Show", ""]

-- | Check if production is single field
isSingleField :: [Symbol] -> Bool
isSingleField [symbol] =
    case symbol of
        NonTerminal _ _ -> True
        Terminal _ -> True
        Macro _ -> True
        Modified ZeroOrMore _ -> True
        Modified OneOrMore _ -> True
        _ -> False
isSingleField _ = False

-- | Generate newtype for single-field production
generateNewType :: String -> [Symbol] -> String
generateNewType name [symbol] =
    let typ = symbolToType symbol
        firstChar = case typ of
            (c:_) -> c
            []    -> ' '  -- fallback, shouldn't happen
        needsParens = 
            (' ' `elem` typ) && firstChar /= '[' && firstChar /= '('
        typWrapped = if needsParens then "(" ++ typ ++ ")" else typ
    in "newtype " ++ name ++ " = " ++ name ++ " " ++ typWrapped
generateNewType _ _ = ""

-- | Generate datatype for multi-production rules
generateDataType :: String -> [[Symbol]] -> [String]
generateDataType name prods = 
    case prods of
        [] -> []
        _ -> 
            let dataHeader = "data " ++ name ++ " = "
                constructors = 
                    zipWith (generateConstructor name) [1..] prods
                headerLen = length dataHeader
            in case constructors of 
                [] -> []
                (first:rest) ->
                    let firstLine = dataHeader ++ first
                        pipeIndent = replicate (headerLen - 2) ' '
                        restLines = [ pipeIndent ++ "| " ++ c | c <- rest ]
                    in (firstLine : restLines) ++ ["    deriving Show"]

-- | Generate constructor for a production
generateConstructor :: String -> Int -> [Symbol] -> String
generateConstructor conName idx symbols =
    conName ++ show idx ++ " " ++ unwords (fmap symbolToType symbols)

-- | Convert symbol to Haskell type syntax
symbolToType :: Symbol -> String
symbolToType (Terminal _) = "String"
symbolToType (NonTerminal n []) = capitalize n
symbolToType (NonTerminal n args) = 
    capitalize n ++ " " ++ unwords (symbolToType <$> args)
symbolToType (Macro "int") = "Int"
symbolToType (Macro "alpha") = "String"
symbolToType (Macro "newline") = "Char"
symbolToType (Macro m) = m
symbolToType (Group alts) = 
    "(" ++ intercalate " | " (altsToTypes alts) ++ ")"
    where
        altsToTypes [] = []
        altsToTypes (a:as) = [unwords (fmap symbolToType a)] ++ altsToTypes as 
symbolToType (Modified Optional s) = 
    "(" ++ "Maybe " ++ symbolToType s ++ ")"
symbolToType (Modified ZeroOrMore s) = "[" ++ symbolToType s ++ "]"
symbolToType (Modified OneOrMore s) = "[" ++ symbolToType s ++ "]"
symbolToType (Modified Tok s) = symbolToType s

-- | Generate parser declarations for a rule
genParserDeclaration :: Rule -> [String]
genParserDeclaration rule = 
    let name = ruleName rule
        capitalized = capitalize name 
        parameters = ruleParameters rule 
        prods = ruleProductions rule 
    in if null parameters
        then parserDeclaration name capitalized prods 
        else 
            parameteredParserDeclaration name capitalized parameters prods

-- | Generate simple parser declaration
parserDeclaration :: String -> String -> [[Symbol]] -> [String]
parserDeclaration functionName typeName prods =
    case prods of 
        [prod] | isSingleField prod ->
            let symbol = case prod of (s:_) -> s; [] -> Terminal ""
            in [functionName ++ " :: Parser " ++ typeName, 
                functionName ++ " = " ++ typeName ++ " <$> " ++ symbolToParser symbol, ""]
        _ -> generateParserBody functionName typeName prods ++ [""]

-- | Generate parser declaration with type parameters
parameteredParserDeclaration :: String -> String -> [Char] -> [[Symbol]] -> [String]
parameteredParserDeclaration functionName typeName parameters prods = 
    let paramVars = [ [c] | c <- parameters ]
        paramTypes = [ "Parser " ++ [c] | c <- parameters ]
        returnType = "Parser (" ++ typeName ++ " " ++ unwords paramVars ++ ")"
        headerType = buildHeaderType paramTypes returnType
        indexedProds = zip [1..] prods
        alts = [ generateParserAlt typeName idx prod | (idx, prod) <- indexedProds ]
        body = case alts of
                [] -> "undefined"
                [alt] -> alt
                (first:rest) -> 
                    first ++ 
                    concat [ "\n" ++ replicate (length functionName + 3) ' ' ++ 
                    "<|> " ++ a | a <- rest ]
    in [functionName ++ " :: " ++ headerType, 
        functionName ++ " " ++ unwords paramVars ++ " = " ++ body, ""]
  where
    buildHeaderType :: [String] -> String -> String
    buildHeaderType [] ret = ret
    buildHeaderType [x] ret = x ++ " -> " ++ ret
    buildHeaderType (x:xs) ret = x ++ " -> " ++ buildHeaderType xs ret

-- | Generate parser body with multiple alternatives
generateParserBody :: String -> String -> [[Symbol]] -> [String]
generateParserBody functionName typeName prods =
    let header = functionName ++ " :: Parser " ++ typeName
        indexedProds = zip [1..] prods
        alts = [ 
            (idx, generateParserAlt typeName idx prod) 
            | (idx, prod) <- indexedProds 
                ]
    in case alts of 
        [] -> []
        [(_, alt)] -> 
            [header, functionName ++ " = " ++ alt]
        ((_, first):rest) -> 
            let firstLine = functionName ++ " = " ++ first
                indentSpaces = length functionName + 1
                restLines = [ 
                    replicate indentSpaces ' ' ++
                    "<|> " ++ 
                    alt | (_, alt) <- rest 
                    ]
            in [header, firstLine] ++ restLines

-- | Generate parser for single production alternative
generateParserAlt :: String -> Int -> [Symbol] -> String
generateParserAlt typeName idx symbols = 
    let constructorName = typeName ++ show idx
        parsers = [ symbolToParser s | s <- symbols ]
    in case parsers of 
        [] -> ""
        [p] -> constructorName ++ " <$> " ++ p 
        (p:ps) -> constructorName ++ " <$> " ++ p ++ " " ++
            unwords ([ "<*> " ++ x | x <- ps ])

-- | Convert symbol to parser expression
symbolToParser :: Symbol -> String
symbolToParser (Terminal s) = "(string \"" ++ s ++ "\")"
symbolToParser (NonTerminal n []) = n
symbolToParser (NonTerminal n args) = 
    "(" ++ n ++ " " ++ unwords (fmap symbolToParser args) ++ ")"
symbolToParser (Macro "int") = "int"
symbolToParser (Macro "alpha") = "(some alpha)"
symbolToParser (Macro "newline") = "(is '\\n')"
symbolToParser (Macro m) = m
symbolToParser (Group alts) =
    "(" ++ intercalate " <|> " (altsToParsers alts) ++ ")"
  where
    altsToParsers [] = []
    altsToParsers (a:as) = [concatParsers a] ++ altsToParsers as

    concatParsers [] = "pure []"
    concatParsers (s:ss) =
        case ss of
            [] -> symbolToParser s
            _  -> symbolToParser s ++ " <*> " ++ concatParsers ss
symbolToParser (Modified Optional s) = "(optional " ++ symbolToParser s ++ ")"
symbolToParser (Modified ZeroOrMore s) = "(many " ++ symbolToParser s ++ ")"
symbolToParser (Modified OneOrMore s) = "(some " ++ symbolToParser s ++ ")"
symbolToParser (Modified Tok s) = 
    case s of
        Terminal t -> "(stringTok \"" ++ t ++ "\")"
        _ -> "(tok " ++ symbolToParser s ++ ")"

-- | Utility Functions

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

toUpper :: Char -> Char
toUpper c
    | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
    | otherwise = c 

-- | Validation Section

-- | Validate grammar and return list of warnings
validate :: ADT -> [String]
validate (Grammar rules) = 
    let duplicates = findDuplicateRules rules
        undefineds = findUndefinedNonterminals rules
        leftRec = findLeftRecursions rules
        warnings = [ "Duplicate rule: " ++ d | d <- duplicates ] ++
                   [ "Undefined nonterminal: " ++ u | u <- undefineds ] ++
                   [ "Left recursion in: " ++ l | l <- leftRec ]
    in warnings
    
-- | Find duplicate rule definitions    
findDuplicateRules :: [Rule] -> [String]
findDuplicateRules rules = 
    [ 
        x | xs <- group (sort (ruleNames rules))
        , length xs > 1
        , let x = case xs of (h:_) -> h; [] -> ""
    ]

-- | Extract rule names from rules
ruleNames :: [Rule] -> [String]
ruleNames [] = []
ruleNames (r:rs) = ruleName r : ruleNames rs

-- | Find nonterminals referenced but not defined
findUndefinedNonterminals :: [Rule] -> [String]
findUndefinedNonterminals rules = 
    nub [ n | sym <- allSymbols rules
        , isUndefined sym definedNames
        , let n = getNTName sym
        ]
  where 
    definedNames = ruleNames rules

-- | Check if symbol is undefined nonterminal
isUndefined :: Symbol -> [String] -> Bool
isUndefined (NonTerminal n _) defined = n `notElem` defined
isUndefined _ _ = False 

-- | Extract nonterminal name from symbol
getNTName :: Symbol -> String
getNTName (NonTerminal n _) = n
getNTName _ = ""

-- | Collect all symbols from all rules
allSymbols :: [Rule] -> [Symbol]
allSymbols rules = concatMap ruleSymbols rules 

-- | Extract symbols from productions in a rule
ruleSymbols :: Rule -> [Symbol]
ruleSymbols (Rule _ prods _) = concat prods 

-- | Find rules with left recursion 
findLeftRecursions :: [Rule] -> [String]
findLeftRecursions rules = 
    [ name 
    | (name, _deps) <- buildDependencyGraph rules
    , hasCycle (buildDependencyGraph rules) name
    ]

-- | Build dependency graph for recursion detection
--   Maps each rule to its leftmost nonterminals (immediate deps).
buildDependencyGraph :: [Rule] -> [(String, [String])]
buildDependencyGraph rules = 
    [ (ruleName r, leftmostNonterminals (ruleProductions r)) | r <- rules ]

-- | Extract leftmost nonterminals from productions
leftmostNonterminals :: [[Symbol]] -> [String]
leftmostNonterminals [] = []
leftmostNonterminals (p:ps) 
  | null p = leftmostNonterminals ps
  | otherwise = 
    case p of 
        (NonTerminal n _:_) -> n : leftmostNonterminals ps
        _ -> leftmostNonterminals ps

-- | Detect cycles in dependency graph using DFS
--   Returns True if 'start' can reach itself through dependencies.
--   Uses visited set to detect cycles.
hasCycle :: [(String, [String])] -> String -> Bool
hasCycle graph start = dfs [] start
  where 
    dfs visited node
      | node `elem` visited = True
      | otherwise = 
        case lookup node graph of 
            Nothing -> False
            Just deps -> or [dfs (node:visited) d | d <- deps]

-- | Rule Cleaning

-- | Clean rules by removing duplicates, undefined refs, and left recursion
cleanRules :: [Rule] -> [Rule]
cleanRules rules =
    let step1 = removeDuplicates rules
        step2 = removeUndefinedRefs step1
        step3 = removeLeftRecursive step2
    in if length step1 == length step2 && length step2 == length step3
       then step1
       else cleanRules step3

-- | Remove duplicate rule definitions 
removeDuplicates :: [Rule] -> [Rule]
removeDuplicates [] = []
removeDuplicates (r:rs) = 
    r : removeDuplicates [ x | x <- rs, ruleName x /= ruleName r ]

-- | Remove rules with undefined nonterminal references
removeUndefinedRefs :: [Rule] -> [Rule]
removeUndefinedRefs rules =
    let definedNames = [ ruleName r | r <- rules ]
        isValid r = 
            all (\sym -> isValidSymbol sym definedNames) (allSymbolsInRule r)
    in [ r | r <- rules, isValid r ]

-- | Check if symbol is valid
isValidSymbol :: Symbol -> [String] -> Bool
isValidSymbol (NonTerminal n _) defined = n `elem` defined
isValidSymbol _ _ = True

-- | Extract all symbols from a rule's productions
allSymbolsInRule :: Rule -> [Symbol]
allSymbolsInRule (Rule _ prods _) = concat prods

-- | Remove rules with left recursion
removeLeftRecursive :: [Rule] -> [Rule]
removeLeftRecursive rules =
    let leftRecursive = findLeftRecursions rules
    in [ r | r <- rules, ruleName r `notElem` leftRecursive ]

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime