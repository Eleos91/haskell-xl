
all: main.hs 
	ghc -dynamic -Wall -lcrypt -o xl main.hs 
	sudo setcap cap_dac_read_search+ep xl

stack: 
	stack install
	mv $$HOME/.local/bin/haskell-xl $$HOME/.local/bin/lock
	sudo setcap cap_dac_read_search+ep $$HOME/.local/bin/lock
