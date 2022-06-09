{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module DimAnalysis (SIBaseUnit (..), parseExpr, parseEquation, simplifyExpr, mainParser, solveEquation, ppExpr) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.Array.IArray
import Data.Ix (Ix)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Ratio (Rational, denominator, numerator, (%))
import Data.Void (Void)
import Text.Megaparsec hiding (sepBy1)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.PrettyPrint as PP

-- [angular velocity] [resistance] ??? [magnetic field]^2 [length] / ( [resistance]^2 + [angular velocity]^2 [permeability]^2 [radius]^2 ) = [torque]
-- s^-8 kg^3 m^3 A^-4 ??? / ( kg^2 m^4 s^-6 A^-4 ) = kg m^2 s^-2

-- {{{ Types
data SIBaseUnit = Second | Metre | Kilogram | Ampere | Kelvin | Mole | Candela | Unknown deriving (Show, Read, Eq, Ord, Bounded, Ix)

data Unit = Unit SIBaseUnit Rational deriving (Show, Read, Eq)

type DerivedUnit = NonEmpty Unit

data Expr
  = EUnit DerivedUnit
  | EDimensionless
  | EMult Expr Expr
  | EDiv Expr Expr
  | EPower Expr Rational
  deriving (Show, Read, Eq)

-- Equation is just a Expr = 1
newtype Equation = Equation Expr deriving (Show, Read, Eq)

type Parser = Parsec Void String

-- }}}

-- {{{ Utils
skipSpace :: Parser ()
skipSpace = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

symbol :: String -> Parser String
symbol = L.symbol skipSpace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parseSignedInteger :: Parser Integer
parseSignedInteger = L.signed skipSpace $ lexeme L.decimal

binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

-- }}}

-- {{{ Parsing
parseSIBaseUnitSymbol :: Parser SIBaseUnit
parseSIBaseUnitSymbol =
  choice
    [ Mole <$ symbol "mol",
      Candela <$ symbol "cd",
      Kilogram <$ symbol "kg",
      Second <$ symbol "s",
      Metre <$ symbol "m",
      Ampere <$ symbol "A",
      Kelvin <$ symbol "K",
      Unknown <$ symbol "???"
    ]

listOfDerivedUnits :: [(String, Expr)]
listOfDerivedUnits =
  sortBy
    (\(s1, _) (s2, _) -> compare (Down s1) (Down s2)) -- sorting inverse alphabetically to ensure that the longer ones are tried first so the shorter ones don't "override" it
    [ ("Hz", EUnit $ Unit Second (-1) :| []),
      ("rad", EDimensionless),
      ("sr", EDimensionless),
      ("N", EUnit $ Unit Kilogram 1 :| [Unit Metre 1, Unit Second (-2)]),
      ("Pa", EUnit $ Unit Kilogram 1 :| [Unit Metre (-1), Unit Second (-2)]),
      ("J", EUnit $ Unit Kilogram 1 :| [Unit Metre 2, Unit Second (-2)]),
      ("W", EUnit $ Unit Kilogram 1 :| [Unit Metre 2, Unit Second (-3)]),
      ("C", EUnit $ Unit Second 1 :| [Unit Ampere 1]),
      ("V", EUnit $ Unit Kilogram 1 :| [Unit Metre 2, Unit Second (-3), Unit Ampere (-1)]),
      ("F", EUnit $ Unit Kilogram (-1) :| [Unit Metre (-2), Unit Second 4, Unit Ampere 2]),
      ("ohm", EUnit $ Unit Kilogram 1 :| [Unit Metre 2, Unit Second (-3), Unit Ampere (-2)]),
      ("S", EUnit $ Unit Kilogram (-1) :| [Unit Metre (-2), Unit Second 3, Unit Ampere 2]),
      ("Wb", EUnit $ Unit Kilogram 1 :| [Unit Metre 2, Unit Second (-2), Unit Ampere (-1)]),
      ("T", EUnit $ Unit Kilogram 1 :| [Unit Second (-2), Unit Ampere (-1)]),
      ("H", EUnit $ Unit Kilogram 1 :| [Unit Metre 2, Unit Second (-2), Unit Ampere (-2)]),
      ("lm", EUnit $ Unit Candela 1 :| []),
      ("lx", EUnit $ Unit Candela 1 :| [Unit Metre (-2)]),
      ("Bq", EUnit $ Unit Second (-1) :| []),
      ("Gy", EUnit $ Unit Metre 2 :| [Unit Second (-2)]),
      ("Sv", EUnit $ Unit Metre 2 :| [Unit Second (-2)]),
      ("kat", EUnit $ Unit Second (-1) :| [Unit Mole 1])
    ]

parseDerivedUnitSymbol :: Parser Expr
parseDerivedUnitSymbol = choice $ map parsePair listOfDerivedUnits
  where
    parsePair :: (String, Expr) -> Parser Expr
    parsePair (s, exp) = exp <$ symbol s

parseExponent :: Parser Rational
parseExponent = try integerParser <|> parens ratioParser
  where
    integerParser = fromIntegral <$> parseSignedInteger
    ratioParser = do
      num <- parseSignedInteger
      symbol "/"
      den <- parseSignedInteger
      return (num % den)

parseDerivedUnit :: Parser Expr
parseDerivedUnit = lexeme $ do
  derivedUnit <- parseDerivedUnitSymbol
  power <- option 1 (symbol "^" >> parseExponent)
  return $ EPower derivedUnit power

parseSIBaseUnit :: Parser Expr
parseSIBaseUnit = lexeme $ do
  baseUnitSymbol <- parseSIBaseUnitSymbol
  power <- option 1 (symbol "^" >> parseExponent)
  return $ EUnit $ Unit baseUnitSymbol power :| []

parseUnit :: Parser Expr
parseUnit = try parseSIBaseUnit <|> parseDerivedUnit

parseDimensionless :: Parser Expr
parseDimensionless = lexeme $ EDimensionless <$ symbol "1"

parsePower :: Parser Expr
parsePower = try $ do
  innerExpr <- parens parseExpr
  symbol "^"
  EPower innerExpr <$> parseExponent

parseTerm :: Parser Expr
parseTerm =
  choice
    [ parsePower,
      parens parseExpr,
      parseUnit,
      parseDimensionless
    ]

parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ binary "/" EDiv,
      binary "" EMult
    ]
  ]

parseEquation :: Parser Equation
parseEquation = do
  lhs <- parseExpr
  symbol "="
  Equation . simplifyExpr . EDiv lhs <$> parseExpr

mainParser :: String -> Either String Equation
mainParser input =
  let outputE = parse parseEquation "" input
   in case outputE of
        Left err -> Left $ errorBundlePretty err
        Right output -> Right output

-- }}}

-- {{{ Simplifying
simplifyExpr :: Expr -> Expr
simplifyExpr (EUnit a) = maybe EDimensionless EUnit $ nonEmpty unitList
  where
    initial :: Array SIBaseUnit Rational
    initial = listArray (minBound, maxBound) (replicate 8 0)

    f :: Unit -> Array SIBaseUnit Rational -> Array SIBaseUnit Rational
    f (Unit baseUnit power) acc = acc // [(baseUnit, acc ! baseUnit + power)]

    arrToDerivedUnit :: Array SIBaseUnit Rational -> [Unit]
    arrToDerivedUnit arr = foldr (\(i, e) acc -> if e /= 0 then Unit i e : acc else acc) [] (assocs arr)

    unitList :: [Unit]
    unitList = arrToDerivedUnit $ foldr f initial a
simplifyExpr (EPower EDimensionless _) = EDimensionless
simplifyExpr (EPower (EUnit xs) power) = EUnit $ applyPower power xs
  where
    applyPower :: Rational -> DerivedUnit -> DerivedUnit
    applyPower power = NE.map (\(Unit base oldPower) -> Unit base (power * oldPower))
simplifyExpr (EPower expr power) = simplifyExpr $ EPower (simplifyExpr expr) power
simplifyExpr (EMult expr EDimensionless) = simplifyExpr expr
simplifyExpr (EMult EDimensionless expr) = simplifyExpr expr
simplifyExpr (EMult (EUnit xs) (EUnit ys)) = simplifyExpr $ EUnit (xs <> ys)
simplifyExpr (EMult (EUnit xs) expr) = simplifyExpr $ EMult (EUnit xs) $ simplifyExpr expr
simplifyExpr (EMult expr (EUnit xs)) = simplifyExpr $ EMult (EUnit xs) $ simplifyExpr expr
simplifyExpr (EDiv expr1 expr2) = simplifyExpr $ EMult expr1 (EPower expr2 (-1))
simplifyExpr (EMult expr1 expr2) = simplifyExpr $ EMult (simplifyExpr expr1) (simplifyExpr expr2)
simplifyExpr EDimensionless = EDimensionless

-- }}}

-- {{{ Solving
solveEquation :: Equation -> Either String Expr
solveEquation (Equation (EUnit xs)) =
  case extractUnknown xs of
    (Just (Unit Unknown power), x) -> Right $ simplifyExpr $ EPower x (-1 / power)
    (Just _, _) -> Left "somehow ended up with not an unknown during solving"
    (Nothing, _) -> Left "no unknown appeared in eqn OR unknowns cancelled out"
  where
    extractUnknown :: DerivedUnit -> (Maybe Unit, Expr)
    extractUnknown xs =
      let (unknown, rest) = NE.partition (\(Unit base _) -> base == Unknown) xs
       in (listToMaybe unknown, maybe EDimensionless EUnit (nonEmpty rest))
solveEquation _ = Left "equation isn't fully simplified"

-- }}}

-- {{{ Pretty printing
ppSIBaseUnit :: SIBaseUnit -> PP.Doc
ppSIBaseUnit Second = PP.text "s"
ppSIBaseUnit Metre = PP.text "m"
ppSIBaseUnit Kilogram = PP.text "kg"
ppSIBaseUnit Ampere = PP.text "A"
ppSIBaseUnit Kelvin = PP.text "K"
ppSIBaseUnit Mole = PP.text "mol"
ppSIBaseUnit Candela = PP.text "cd"
ppSIBaseUnit Unknown = PP.text "???"

ppExponent :: Rational -> PP.Doc
ppExponent x
  | denominator x == 1 = PP.integer $ numerator x
  | otherwise = PP.parens $ PP.integer (numerator x) <> PP.char '/' <> PP.integer (denominator x)

ppBaseAndExponent :: PP.Doc -> Rational -> PP.Doc
ppBaseAndExponent doc power
  | power == 1 = doc
  | otherwise = doc <> PP.char '^' <> ppExponent power

ppUnit :: Unit -> PP.Doc
ppUnit (Unit siBaseUnit power) = ppBaseAndExponent (ppSIBaseUnit siBaseUnit) power

ppExpr :: Expr -> PP.Doc
ppExpr (EUnit xs) = PP.hsep (map ppUnit (NE.toList xs))
ppExpr EDimensionless = PP.empty
ppExpr (EMult expr1 expr2) = ppExpr expr1 <> PP.space <> ppExpr expr2
ppExpr (EDiv expr1 expr2) = ppExpr expr1 <> PP.text " / " <> PP.parens (ppExpr expr2)
ppExpr (EPower expr1 power) = ppBaseAndExponent (PP.parens (ppExpr expr1)) power

-- }}}
