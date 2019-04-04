
all: main.hs 
	ghc -dynamic -Wall -lcrypt -o xl main.hs 
	sudo setcap cap_dac_read_search+ep xl
