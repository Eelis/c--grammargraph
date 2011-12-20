{-# LANGUAGE UnicodeSyntax, RecordWildCards #-}

module Main (main) where

import Data.GraphViz.Types.Canonical
import Data.GraphViz.Commands
import Data.GraphViz.Attributes.Colors
import Data.GraphViz.Attributes.Complete
import Data.Text.Lazy (pack)
import Data.Monoid (Monoid(..))
import CxxGrammar (grammar, Section(..), Rule(..), Alternative(..), Thing(..), RuleName, SectionName, Grammar)
import Data.List (nub)

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

ruleToDot :: Color → (RuleName, [RuleName]) → DotStatements String
ruleToDot c (rule, children) = DotStmts
    { attrStmts = []
    , subGraphs = []
    , nodeStmts = [DotNode from [FillColor c, Color [X11Color White], LabelFontColor (X11Color White)]]
        ++ [ghostNode parentNode brokenParents | not $ hideBrokenParents]
        ++ [ghostNode childNode brokenChildren | not $ null brokenChildren]
    , edgeStmts = edges ++ [parentEdge | not $ hideBrokenParents] ++ [childEdge | not $ null brokenChildren]}
  where
    ghostNode x y = DotNode x [Label $ StrLabel $ pack $ bigBox y, Shape BoxShape, FillColor $ X11Color Gray, Color [X11Color White]]
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
route "postfix-expression" "id-expression" = Break
route "declarator-id" "id-expression" = Break
route "type-parameter" "id-expression" = Break
route "template-argument" "id-expression" = Break
route "new-declarator" "ptr-operator" = Break
route "conversion-declarator" "ptr-operator" = Break
route "condition" "initializer-clause" = Break
route "unqualified-id" "class-name" = Break
route "declarator-id" "class-name" = Break
route "class-or-decltype" "class-name" = Break
route "decltype-specifier" "expression" = Pass
route "for-range-initializer" "expression" = Break
route "linkage-specification" "string-literal" = Break
route "elaborated-type-specifier" "class-key" = Break
route "asm-definition" "string-literal" = Break
route "template-argument" "type-id" = Break
route "using-declaration" "unqualified-id" = Break
route "nested-name-specifier" "simple-template-id" = Break
route "pseudo-destructor-name" "simple-template-id" = Break
route "simple-type-specifier" "simple-template-id" = Break
route "type-name" "simple-template-id" = Break
route "elaborated-type-specifier" "simple-template-id" = Break
route "typename-specifier" "simple-template-id" = Break
route "class-name" "simple-template-id" = Break
route "preprocessing-token" "pp-number" = Break
route "preprocessing-token" "character-literal" = Break
route "preprocessing-token" "user-defined-character-literal" = Break
route "preprocessing-token" "string-literal" = Break
route "preprocessing-token" "user-defined-string-literal" = Break
route "member-declaration" "static-assert-declaration" = Break
route "static-assert-declaration" "string-literal" = Break
route "type-name" "class-name" = Break
route "token" "operator" = Break
route "type-id-list" "type-id" = Pass
route _ "attribute-specifier-seq" = Kill
route _ to
  | to `elem` words "braced-init-list constant-expression decl-specifier-seq decl-specifier-seq type-specifier-seq identifier nested-name-specifier cv-qualifier-seq declarator type-id decltype-specifier expression expression-list" = Break
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
main = do
  _ ← runGraphvizCommand Dot (graphToDot graph) Png "grammar.png" 
  return ()
