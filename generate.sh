#!/bin/sh

set -ve

runhaskell -Wall GrammarGraph.hs > grammar.dot
dot -Tpng grammar.dot > grammar.png
