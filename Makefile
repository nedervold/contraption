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

.PHONY : syntax
syntax :  build
	stack exec syntax | more

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
