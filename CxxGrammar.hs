{-# LANGUAGE UnicodeSyntax #-}

-- changes i made to grammar: rename static_assert-declaration to static-assert-declaration
-- i've guessed that alignment-expression should be assignment-expression

module CxxGrammar (grammar, Section(..), Rule(..), Alternative(..), Thing(..), RuleName, SectionName, Grammar) where

import Text.Parsec (many, char, string, (<|>), satisfy, try, many1, runParser)
import Text.Parsec.String (Parser)
import Control.Monad (liftM, liftM2)
import Data.List (stripPrefix)
import System.IO.Unsafe (unsafePerformIO)
import Data.Char (isAlphaNum, isSpace)
import Prelude hiding ((.))

(.) :: Functor f ⇒ (a → b) → (f a → f b)
(.) = fmap

type Grammar = [Section]
data Section = Section { sectionName :: SectionName, sectionRules :: [Rule] }
data Rule = Rule { ruleName :: RuleName, ruleAlternatives :: [Alternative] }
type RuleName = String
type SectionName = String
data Alternative = Informal { informalDescription :: String } | Sequence [(Thing, Bool {- optional -})]
data Thing = RuleUse RuleName | Literal String

stripSuffix :: Eq a ⇒ [a] → [a] → Maybe [a]
stripSuffix suf s = reverse . stripPrefix (reverse suf) (reverse s)

(<<) :: Monad m ⇒ m a → m b → m a
(<<) = liftM2 const

name :: Parser String
name = many1 $ satisfy $ \c → isAlphaNum c || c == '-'

restOfLine :: Parser String
restOfLine = many (satisfy (/= '\n'))

ruleUse :: Parser (Thing, Bool)
ruleUse = liftM2 (,) (RuleUse . name) ((string "_opt" >> return True) <|> (return False))

literal :: Parser (Thing, Bool)
literal = do
  _ ← char '"'
  s ← many1 $ satisfy (not . isSpace)
  return $ case stripSuffix "_opt" s of
    Just s' → (Literal $ read $ '"' : s', True)
    Nothing →(Literal $ read $ '"' : s, False)

thing :: Parser (Thing, Bool)
thing = (literal <|> ruleUse) << many (char ' ')

alternativeLine :: Parser [Alternative]
alternativeLine = try $ string "    " >> (list <|> informal <|> normal) << char '\n'
  where
    normal = liftM ((:[]) . Sequence) (many thing)
    informal = try $ (:[]) . Informal . (string "informal: " >> restOfLine)
    list = do
      l ← char '[' >> restOfLine
      return $ map (Sequence . (:[]) . flip (,) False . Literal) (read ('[' : l))

rule :: Parser Rule
rule = liftM2 Rule (string "  " >> name << char '\n') (concat . many alternativeLine)

section :: Parser Section
section = liftM2 Section (restOfLine << char '\n') (many rule)

readGrammar :: IO Grammar
readGrammar = do
  s ← readFile "grammar.txt"
  case runParser (many section) () "stdin" s of
    Left e → error $ show e
    Right r → return r

grammar :: Grammar
grammar = unsafePerformIO readGrammar

