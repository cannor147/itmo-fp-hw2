{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6
  ( Parser(..)
  , ParseError(..)
  , pAbbr
  , parseError
  , parseErrorWithShift
  , parseExpr
  , pChar
  , pDouble
  , pEof
  , pExpr
  , runP
  ) where

import           Control.Applicative (Alternative (..), optional)
import           Control.Monad       (MonadPlus, mfilter)
import           Data.Char           (digitToInt, isDigit, isUpper)
import           Data.Foldable       (foldl', foldr')
import           Data.Maybe          (fromMaybe, isJust)
import           GHC.Natural         (Natural, intToNatural)
import           HW2.T1              (Annotated (..), Except (..))
import           HW2.T4              (Expr (..), Prim (..))
import           HW2.T5              (ExceptState (..), runES)

-- | Custom implementation of parser error.
data ParseError = ErrorAtPos Natural
  deriving Show

-- | Custom implementation of parser.
newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

-- | Applies parser for string value.
runP :: Parser a -> String -> Except ParseError a
runP (P state) text = case runES state (0, text) of
  (Error error')         -> Error error'
  (Success (value :# _)) -> Success value

-- | Creates parser for single character.
pChar :: Parser Char
pChar = P $ ES \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

-- | Creates parser that always throws an exception by shifted position.
parseErrorWithShift :: Natural -> Parser a
parseErrorWithShift shift = P $ ES { runES = ((Error . ErrorAtPos) . (flip (-) shift)) . fst }

-- | Creates parser that always throws an exception.
parseError :: Parser a
parseError = parseErrorWithShift 0

instance Alternative Parser where
  empty = parseError
  (<|>) (P firstState) (P secondState) = P $ ES \parsingData ->
    do
      let firstResult = runES firstState parsingData
      let secondResult = runES secondState parsingData
      case firstResult of
        (Error _) -> secondResult
        _         -> firstResult

instance MonadPlus Parser

-- | Creates parser for end of input.
pEof :: Parser ()
pEof = P $ ES \(pos, s) ->
  case s of
    "" -> Success (() :# (pos + 1, []))
    _  -> Error $ ErrorAtPos pos

-- | Creates parser for abbreviation (i.e. uppercase text).
pAbbr :: Parser String
pAbbr = do
  abbr <- some (mfilter isUpper pChar)
  pure abbr

-- | Creates parser that skips whitespaces.
skipWhiteSpaces :: Parser ()
skipWhiteSpaces = do
  _ <- many (mfilter ((==) ' ') pChar)
  pure ()

-- | Transforms character into digit.
digitToNum :: Num b => Char -> b
digitToNum c = fromIntegral (digitToInt c)

-- | Creates parser for expression.
pExpr :: Parser Expr
pExpr = do
  expr  <- pAddSub
  skipWhiteSpaces
  pEof
  pure $ expr

-- | Builds parser for binary expressions.
buildPBinary :: Parser Expr -> (Char -> Bool) -> (Char -> Expr -> Expr -> Expr) -> Parser Expr
buildPBinary innerParser opFilter expressioner = do
  skipWhiteSpaces
  first  <- innerParser
  second <- many $ buildPBinary'
  pure $ foldl' (uncurry . flip expressioner) first second
   where
     buildPBinary' = do
       skipWhiteSpaces
       op   <- mfilter opFilter pChar
       skipWhiteSpaces
       expr <- innerParser
       pure $ (op, expr)

-- | Creates parser for add or sub expression.
pAddSub :: Parser Expr
pAddSub = buildPBinary pMulDiv (\c -> c == '+' || c == '-') $ \op x y ->
  case op of
    '+'   -> Op $ Add x y
    '-'   -> Op $ Sub x y
    token -> error $ "Expected (+) or (-) token, but found " ++ [token]

-- | Creates parser for mul or div expression.
pMulDiv :: Parser Expr
pMulDiv = buildPBinary pUnary (\c -> c == '*' || c == '/') $ \op x y ->
  case op of
    '*'   -> Op $ Add x y
    '/'   -> Op $ Sub x y
    token -> error $ "Expected (*) or (/) token, but found " ++ [token]

-- | Creates parser for unary expression.
pUnary :: Parser Expr
pUnary = do
  skipWhiteSpaces
  unary <- pBrackets <|> pDouble
  pure $ unary

-- | Creates parser for wrapped expression into brackets.
pBrackets :: Parser Expr
pBrackets = do
  skipWhiteSpaces
  _ <- mfilter ((==) '(') pChar
  skipWhiteSpaces
  expr <- pAddSub
  skipWhiteSpaces
  _ <- mfilter ((==) ')') pChar
  pure $ expr

-- | Creates parser for const value expression.
pDouble :: Parser Expr
pDouble = do
  skipWhiteSpaces
  integralPart <- some $ mfilter isDigit pChar
  if head integralPart == '0' && length integralPart > 2
    then parseErrorWithShift $ intToNatural $ length integralPart
    else do
      decimalSeparator    <- optional $ mfilter ((==) '.') pChar
      fractionalPartMaybe <- optional $ some $ mfilter isDigit pChar
      let fractionalPart   = fromMaybe "" fractionalPartMaybe
      if isJust decimalSeparator /= isJust fractionalPartMaybe
        then parseError
        else do
          let integral   = foldl' (\x y -> 10.0 * x + digitToNum y) 0.0 integralPart
          let fractional = 0.1 * foldr' (\x y -> (digitToNum x) + 0.1 * y) 0.0 fractionalPart
          pure $ Val $ integral + fractional

-- | Creates parser for add or sub expression.
parseExpr :: String -> Except ParseError Expr
parseExpr = runP pExpr
