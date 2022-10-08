.PHONY : run
run : build
	# stack exec contraption -- -g Ebnf.ebnf ebnf-grammar  dependency-graph
	-rm -rf build-dir
	stack exec contraption -- -g Ebnf.ebnf --build token syntax

.PHONY : build
build : 
	stack build

############################################################
.PHONY : token
token :  build
	stack exec token | more

.PHONY : token-generators
token-generators :  build
	stack exec token-generators | more

.PHONY : token-parsers
token-parsers :  build
	stack exec token-parsers | more

.PHONY : token-prettyprinters
token-prettyprinters :  build
	stack exec token-prettyprinters | more

.PHONY : syntax
syntax :  build
	stack exec syntax | more

.PHONY : syntax-generators
syntax-generators :  build
	stack exec syntax-generators | more

.PHONY : syntax-parsers
syntax-parsers :  build
	stack exec syntax-parsers | more

.PHONY : syntax-prettyprinters
syntax-prettyprinters :  build
	stack exec syntax-prettyprinters | more

.PHONY : compile-all
compile-all :  build
	-rm -rf build-dir
	stack exec compile-all

############################################################
.PHONY : test
test : tidy
	stack test

.PHONY : docs
docs :
	stack haddock && open `stack path --local-doc-root`/index.html

.PHONY : lint
lint : 
	hlint app src test 

.PHONY : tidy
tidy :
	find . -name '*~' -delete
	find . -name '#*' -delete

.PHONY : clean
clean : tidy
	stack clean

.PHONY : purge
purge : clean
	-rm -rf .stack-work
