{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6
  ( Parser(..)
  , ParseError(..)
  , pAbbr
  , parseError
  , parseExpr
  , pChar
  , pEof
  , runP
  ) where

import           Control.Applicative (Alternative (..))
import           Control.Monad       (MonadPlus, mfilter)
import           Data.Char           (isUpper)
import           GHC.Natural         (Natural)
import           HW2.T1              (Annotated (..), Except (..))
import           HW2.T4              (Expr)
import           HW2.T5              (ExceptState (..), runES)

data ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P state) text = case runES state (0, text) of
  (Error error')         -> Error error'
  (Success (value :# _)) -> Success value

pChar :: Parser Char
pChar = P $ ES \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES { runES = const $ Error $ ErrorAtPos 0 }

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

pEof :: Parser ()
pEof = P $ ES \(pos, s) ->
  case s of
    "" -> Success (() :# (pos + 1, []))
    _  -> Error $ ErrorAtPos pos

pAbbr :: Parser String
pAbbr = do
  abbr <- some (mfilter isUpper pChar)
  pEof
  pure abbr

parseExpr :: String -> Except ParseError Expr
parseExpr = undefined

