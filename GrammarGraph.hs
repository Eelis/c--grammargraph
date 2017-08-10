{-# LANGUAGE UnicodeSyntax, RecordWildCards #-}

module Main (main) where

import Data.GraphViz.Types.Canonical
import Data.GraphViz.Commands
import Data.GraphViz.Attributes (textLabel)
import Data.GraphViz.Attributes.Colors
import Data.GraphViz.Attributes.Colors.X11
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types (printDotGraph)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.IO (putStrLn)
import Data.Monoid (Monoid(..))
import CxxGrammar (grammar, Section(..), Rule(..), Alternative(..), Thing(..), RuleName, SectionName, Grammar)
import Data.List (nub)
import Prelude hiding (putStrLn)

type Graph = [(SectionName, [(RuleName, [RuleName])])]

grammarToGraph :: Grammar → Graph
grammarToGraph = map sectionToGraph

sectionToGraph :: Section → (SectionName, [(RuleName, [RuleName])])
sectionToGraph Section{..} = (sectionName, map ruleToGraph sectionRules)

ruleToGraph :: Rule → (RuleName, [RuleName])
ruleToGraph Rule{..} = (ruleName, nub $ concatMap f ruleAlternatives)
  where
    f :: Alternative → [RuleName]
    f (Informal _) = []
    f (Sequence l) = concatMap (thingSubs . fst) l
    thingSubs :: Thing → [RuleName]
    thingSubs (Literal _) = []
    thingSubs (RuleUse r) = [r]

parents :: RuleName → [RuleName]
parents r = [p  | (_, rs) ← graph, (p, subs) ← rs, r `elem` subs]

bigBox :: [String] → String
bigBox l = unlines $ if length l <= limit then l else take (limit-1) l ++ ["+ " ++ show (length l - limit + 1) ++ " more"]
  where limit = 4

nodeLabel :: String -> String
nodeLabel = go 0
  where
    go n [] = []
    go n ('-':x)
      | n > 5 = "-\\n" ++ nodeLabel x
    go n (x:y) = x : go (n+1) y

ruleToDot :: Color → (RuleName, [RuleName]) → DotStatements String
ruleToDot c (rule, children) = DotStmts
    { attrStmts = []
    , subGraphs = []
    , nodeStmts =
        [DotNode from
           [ FillColor (toColorList [c])
           , Color [toWC $ X11Color White]
           , LabelFontColor (X11Color White)
           , textLabel $ pack $ nodeLabel from]]
        ++ [ghostNode parentNode brokenParents | not $ hideBrokenParents]
        ++ [ghostNode childNode brokenChildren | not $ null brokenChildren]
    , edgeStmts = edges ++ [parentEdge | not $ hideBrokenParents] ++ [childEdge | not $ null brokenChildren]}
  where
    ghostNode x y = DotNode x
      [ Label $ StrLabel $ pack $ bigBox y
      , Shape BoxShape
      , FillColor $ toColorList [X11Color Gray]
      , Color $ toColorList [X11Color White]]
    from = rule
    hideBrokenParents = null brokenParents || rule == "identifier" -- hack
    brokenParents = [p | p ← parents rule, route p rule == Break]
    brokenChildren = [ch | ch ← children, route rule ch == Break]
    parentNode = rule ++ "--parent"
    parentEdge = DotEdge parentNode rule []

    childNode = rule ++ "--child"
    childEdge = DotEdge rule childNode []
    edges = map (\to → DotEdge from to []) $ filter (\t → route rule t == Pass) children

sectionToDot :: (SectionName, [(RuleName, [RuleName])]) → DotStatements String
sectionToDot (name, rules) = mconcat $ map (ruleToDot (sectionColor name)) rules

instance Monoid (DotStatements a) where
  mempty = DotStmts [] [] [] []
  mappend (DotStmts a b c d) (DotStmts a' b' c' d')
    = DotStmts (a ++ a') (b ++ b') (c ++ c') (d ++ d')

graphToDot :: Graph → DotGraph String
graphToDot g = DotGraph True True Nothing $
  mappend
    (DotStmts [NodeAttrs [Style [SItem Filled []]]] [] [] [])
    (mconcat $ map sectionToDot g)

data Route = Pass | Kill | Break
  deriving Eq

route :: RuleName → RuleName → Route
route x y | x == y = Pass
route "pseudo-destructor-name" "decltype-specifier" = Break
route "pseudo-destructor-name" "nested-name-specifier" = Break
route "nested-name-specifier" "type-name" = Break
route "postfix-expression" "typename-specifier" = Break
route "postfix-expression" "simple-type-specifier" = Break
route "postfix-expression" "expression-list" = Break
route "new-declarator" "ptr-operator" = Break
route "conversion-declarator" "ptr-operator" = Break
route "defining-type-specifier" _ = Break
route "fold-expression" "cast-expression" = Break
route "condition" "initializer-clause" = Break
route "condition" "declarator" = Pass
route "constrained-parameter" "qualified-concept-name" = Break
route "unqualified-id" "class-name" = Break
route "declarator-id" "class-name" = Break
route "constraint-logical-and-expression" "primary-expression" = Break
route "type-requirement" "type-name" = Break
route "pseudo-destructor-name" "type-name" = Break
route "decltype-specifier" "expression" = Pass
route "for-range-initializer" "expression" = Break
route "literal" "string-literal" = Pass
route "elaborated-type-specifier" "class-key" = Break
route "template-argument" "type-id" = Break
route "using-declaration" "unqualified-id" = Break
route "mem-initializer-id" "class-or-decltype" = Break
route "parameter-declaration" "initializer-clause" = Break
route "member-declaration" "alias-declaration" = Break
route "literal-operator-id" "user-defined-string-literal" = Break
route "function-try-block" "handler-seq" = Break
route "noexcept-specifier" "constant-expression" = Break
route "lambda-declarator" "noexcept-specifier" = Break
route "template-id" "simple-template-id" = Pass
route "lambda-expression" "template-parameter-list" = Break
route "preprocessing-token" "user-defined-character-literal" = Break
route "preprocessing-token" "user-defined-string-literal" = Break
route "preprocessing-token" "identifier" = Pass
route "return-type-requirement" "constrained-parameter" = Break
route "member-declaration" "static-assert-declaration" = Break
route "concept-definition" "concept-name" = Break
route "deduction-guide" "template-name" = Break
route "simple-type-specifier" "template-name" = Break
route "type-name" "class-name" = Break
route "token" "operator" = Break
route "token" "identifier" = Pass
route "type-id-list" "type-id" = Pass
route _ "attribute-specifier-seq" = Kill
route "expr-or-braced-init-list" "expression" = Pass
route "trailing-return-type" "type-id" = Pass
route "return-type-requirement" "trailing-return-type" = Break
route "lambda-declarator" "trailing-return-type" = Break
route "for-range-declaration" "ref-qualifier" = Break
route "simple-declaration" "ref-qualifier" = Break
route "init-capture" "initializer" = Break
route "using-declarator" "unqualified-id" = Break
route "brace-or-equal-initializer" "braced-init-list" = Pass
route "type-id" "abstract-declarator" = Pass
route "init-declarator" "declarator" = Pass
route "type-id" "type-specifier-seq" = Pass
route "primary-expression" "id-expression" = Pass
route _ "identifier" = Kill
route _ to
  | to `elem` words "braced-init-list constant-expression decl-specifier-seq type-specifier-seq identifier nested-name-specifier cv-qualifier-seq declarator type-id decltype-specifier expression expression-list brace-or-equal-initializer parameter-declaration-clause simple-template-id compound-statement expr-or-braced-init-list string-literal constraint-expression abstract-declarator identifier-list id-expression" = Break
route _ _ = Pass

sectionColor :: SectionName → Color
sectionColor s = case s of
  "keywords" → X11Color Khaki
  "lexical conventions" → X11Color DeepSkyBlue
  "basic concepts" → X11Color Magenta
  "expressions" → X11Color MediumTurquoise
  "statements" → RGB 0xbf 0x7f 0xc6
  "declarations" → RGB 0xca 0xb6 0x68
  "declarators" → X11Color YellowGreen
  "classes" → X11Color HotPink
  "derived classes" → X11Color Red
  "special member functions" → X11Color Plum
  "overloading" → X11Color LightCoral
  "templates" → X11Color Gold
  "exception handling" → X11Color IndianRed
  "preprocessing directives" → X11Color DarkOrange
  _ → error "unknown section"

graph :: Graph
graph = grammarToGraph grammar

main :: IO ()
main = putStrLn $ printDotGraph $ graphToDot graph
