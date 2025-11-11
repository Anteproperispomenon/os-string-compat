haddock : 
	stack haddock --haddock-arguments "src/System/OsString/include/Common.hs"

hackage :
	stack haddock --haddock-for-hackage --haddock-arguments "src/System/OsString/include/Common.hs"
