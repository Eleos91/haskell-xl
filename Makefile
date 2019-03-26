
all:
	ghc -dynamic -lcrypt -o xl main.hs 
	sudo chown root:root xl
	sudo chmod u+s xl
