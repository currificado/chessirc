chessirc : Chess.hs Chess/FEN.hs Chess/PGN.hs Game.hs Input.hs Server.hs
	ghc --make Server.hs -o chessirc

clean :
	rm -f *.hi *.o chessirc Chess/*.hi Chess/*.o 
