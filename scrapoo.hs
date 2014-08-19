#!/usr/bin/runghc
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, 
	TypeOperators, TupleSections, LambdaCase, OverloadedStrings,
	NoMonomorphismRestriction, RelaxedPolyRec, ScopedTypeVariables #-}

import DSL.Scrapoo.ParseTree
import DSL.Scrapoo.Syntax
import System.Environment
import Text.Groom
import Data.String.Here
import qualified Data.List.Split as LS

import Language.Javascript.JMacro
import Data.Monoid

--jsGen :: Expr -> String

main = do
	nCheck<-getArgs
	let _t = test (head $ map read nCheck++[1]) snippet snippet $ putStrLn.groom
	--let _t0 = test 1 snippet snippet $ \_->return ()
	let t = test 999 expr expr print
	let t' = test 999 expr expr $ putStrLn.groom
	let t0 = test 999 expr expr $ \_->return ()
	-- let z x s = putStr ">>" >> print s >> mapM_ print (Parser.parse x s)
	-- test 999 curriedList curriedList (putStrLn.groom) "(`html@1,`text@2)"
	t0 "$ - $"
	t0 "$`-``$$"
	t0 "$`-``[$`-``$$][$`-``$$]"
	t0 "$a`-`` $b`-``$c$d $e`-``$f$g"
	t0 "$`-`` $ $`-``$$"
	t0 "$`-`` $`-``$$ $"
	t0 "$ $ $ ```-"
	t0 "-``` $ $ $"
	t0 "//@"
	t0 "//@a_b"
	t0 "//@a_"
	t0 "//@_b"
	t0 "$abcf@kkk+++/.whatever//.kkk/@abc@def+++○div○@123@1@1"
	t0 "$@+++/.whatever//.kkk/@@+++○div○@@@"
	t0 "$aaa@kkk-_"
	t0 "--`$abcf"
	t0 "--``/.ww/@1$1"
	t0 "$a$b``+$1``+"
	t0 "$1`+@5"
	t0 "+`$1@5"
	t0 "$1--$2@3"
	t0 "$1--$2@3$4@5$5@6```x"
	t0 "$1--$2@3$4@5$5@6```x/a/@1``y"
	t0 "$1--$2@3$4@5$5@6```x/a/@1``y/b/@2``z"
	t0 "$1--$2@3$4@5$5@6```x/a/@1``y/b/@2``z-/fff/@5"
	t0 "$--$@$@$@```x/a/@``y/b/@``z-/fff/@"
	t0 "+``$1$2"
	t0 "$2$3``-"
	t0 "[+``$1$2]$3``-"
	t0 "+``$1[$2$3``-]"
	t0 "+``$1$2$3``-"
	t0 "+``@x$1@a$2@b$3@c$4@d```-@z"
	t0 "$`a`$"
	t0 "$a$``a"
	t' [here|$`[_`a]
	|]
	t' "$ $ ``+"
	t' "$a $ ``+"
	t' "$ $b ``+"
	t' "$a $b ``+"
	t' "$ $``+"
	t' "$$ ``+"

	t' "$ `+"
	t' "[ /a/ ]"
	t' "[/a/ ]"
	t' "[ /a/]"
	t' "[ ]"
	{-z exprList "[]"
	z exprList "[ ]"
	z (many' expr) "/a/"
	z (many' expr) "/a/ "
	z (many' expr) " /a/"
	z (many' expr) " /a/ "
	z (many' alpha) "a"
	z (many' alpha) "a "-}

	{-t "--``$abcd"
	t ""-}

	let tf f = readFile f >>= mapM_ _t.map unlines.LS.splitOn ["==="].lines
	
	t0 "-``[-``$$][-``$$]"
	t0 "-`` [-``$$] -``$$"
	t0 "-`` -``$$ [-``$$]"
	t0 "-``-``$$ [-``$$]"
	t0 "-`` -``$$ -``$$"
	t0 "-``-``$$ -``$$"
	t0 "[$$``-][$$``-]``-"
	t0 "$$``- [$$``-] ``-"
	t0 "[$$``-] $$``- ``-"
	t0 "[$$``-] $$``-``-"
	t0 "$$``- $$``- ``-"
	t0 "$$``- $$``-``-"

	t' "$-$-$`children"
	t' "$1-$2-$3"
	t' "$1-$2$3``-"
	t' "$1-$2`-"

	t' "$`[_-$a@x,_+$b@y,_`find`$c@z,_+$+//@+//@]"

	tf "sampletests"
	-- tf "sampletests1"

	putStrLn "*******\nDONE!!!\n*******"
	return ()
