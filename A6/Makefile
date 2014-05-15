all: pi bool lam nat

pi: PiMain.hs Pi.hs
	ghc --make PiMain.hs

bool: BoolExp.hs BoolMain.hs
	ghc --make BoolMain.hs

lam:  Lam.hs LamMain.hs
	ghc --make LamMain.hs

nat:  NatExp.hs NatMain.hs
	ghc --make NatMain.hs

channels: Channels.hs
	ghc --make Channels.hs

clean:
	rm -f *.hi *.o *.exe LamMain PiMain BoolMain NatMain Channels
