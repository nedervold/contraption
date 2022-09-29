.PHONY : run
run : build
	stack exec contraption

.PHONY : build
build : 
	stack build
	
.PHONY : test
test : 
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