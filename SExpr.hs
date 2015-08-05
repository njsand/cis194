{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

oneOrMore :: Parser a -> Parser [a]
oneOrMore x = (:) <$> x <*> zeroOrMore x

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore x = oneOrMore x <|> pure []

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (char ' ')

ident :: Parser String
ident = (++) <$> oneOrMore (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show


parseAtom :: Parser SExpr
parseAtom = A <$> ((N <$> (spaces *> posInt)) <|> (I <$> (spaces *> ident)))

parseSExpr :: Parser SExpr
parseSExpr = parseAtom <|> (Comb <$> (spaces *> char '(' *> (zeroOrMore parseSExpr) <* spaces <* char ')'))

