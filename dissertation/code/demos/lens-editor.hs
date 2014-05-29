{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.Lens.Edit
import Data.Lens.Edit.Stateful
import Data.Lens.Edit.String
import Data.Maybe
import Data.Module hiding (Delete, Insert)
import Data.Module.String
import Graphics.UI.Gtk

newtype DATE      = DATE      () deriving Default
newtype COUNTRY   = COUNTRY   () deriving Default
newtype COMMA     = COMMA     () deriving Default
newtype SEMICOLON = SEMICOLON () deriving Default
newtype NEWLINE   = NEWLINE   () deriving Default

instance Show DATE      where show DATE      {} = "0000"
instance Show COUNTRY   where show COUNTRY   {} = "Unknown"
instance Show COMMA     where show COMMA     {} = ","
instance Show SEMICOLON where show SEMICOLON {} = ";"
instance Show NEWLINE   where show NEWLINE   {} = "\n"

commaToSemicolon
                 =     skipNonEmpty COMMA     {} ","
                 # op (skipNonEmpty SEMICOLON {} ";")
composerName     = copyEmpty "[^,;\n]*"
composerYear     = skipNonEmpty DATE {} "\\d\\d\\d\\d"
composerCountry  = op (skipNonEmpty COUNTRY {} "[A-Z][^\n]*")
newline          = copyNonEmpty NEWLINE {} "\n"
(leftM, lens, rightM) = star (composerName # commaToSemicolon #
                              composerYear # composerCountry  # newline)

initView string = do
	buffer <- textBufferNew Nothing
	view   <- textViewNewWithBuffer buffer
	good   <- newIORef (def, [])
	del    <- newIORef (Delete 0 0)
	sigm   <- newEmptyMVar
	font   <- fontDescriptionNew
	fontDescriptionSetSize font 48
	widgetModifyFont view (Just font)
	readIORef good >>= textBufferInsertAtCursor buffer . pprint string . fst
	return (buffer, view, good, del, sigm)

changeEvent cref sigm this that goodThis goodThat dput stringThis stringThat e = do
	c             <- readIORef cref
	(oldThis, es) <- readIORef goodThis
	(oldThat, _ ) <- readIORef goodThat
	newThisS      <- get this textBufferText
	sigs          <- readMVar sigm
	mapM_ signalBlock sigs
	case parse stringThis newThisS of
		Nothing      -> writeIORef goodThis (oldThis, e:es)
		Just newThis -> do
			let eThis        = edit stringThis oldThis (e:es)
			    (eThat, c')  = dput lens (edit stringThis oldThis (e:es), c)
			    Just newThat = apply eThat oldThat
			putStrLn $ "Edit received: " ++ show eThis
			putStrLn $ "Lens computes: " ++ show eThat
			writeIORef cref c'
			writeIORef goodThis (newThis, [])
			writeIORef goodThat (newThat, [])
			set that [ textBufferText := pprint stringThat newThat ]
	mapM_ signalUnblock sigs

changeEventL cref (buf1, _, good1, _, _) (buf2, _, good2, _, sigm2)
	= changeEvent cref sigm2 buf1 buf2 good1 good2 dputr leftM rightM
changeEventR cref (buf1, _, good1, _, sigm1) (buf2, _, good2, _, _)
	= changeEvent cref sigm1 buf2 buf1 good2 good1 dputl rightM leftM

insertEdit p s  = get p textIterOffset >>= \n -> return (Insert (n - length s) s)
deleteEdit p p' = liftM2 Delete (get p textIterOffset) (get p' textIterOffset)

main = do
	initGUI
	window <- windowNew
	hbox   <- hBoxNew False 2
	cref   <- newIORef (missing lens)
	v1@(buf1, view1, good1, del1, sigm1) <- initView leftM
	v2@(buf2, view2, good2, del2, sigm2) <- initView rightM

	window `on` objectDestroy $ mainQuit
	-- abbreviations to save horizontal space
	let ins = bufferInsertText; del = deleteRange
	sig1i <- buf1 `after` ins $ \p s  -> insertEdit p s  >>= changeEventL cref v1 v2
	sig1w <- buf1 `on`    del $ \p p' -> deleteEdit p p' >>= writeIORef del1
	sig1r <- buf1 `after` del $ \p p' -> readIORef  del1 >>= changeEventL cref v1 v2
	sig2i <- buf2 `after` ins $ \p s  -> insertEdit p s  >>= changeEventR cref v1 v2
	sig2w <- buf2 `on`    del $ \p p' -> deleteEdit p p' >>= writeIORef del2
	sig2r <- buf2 `after` del $ \p p' -> readIORef  del2 >>= changeEventR cref v1 v2
	putMVar sigm1 [sig1i, sig1w, sig1r]
	putMVar sigm2 [sig2i, sig2w, sig2r]

	set window [ containerChild      := hbox
	           , windowDefaultWidth  := 600
	           , windowDefaultHeight := 200
	           ]
	set hbox   [ containerChild      := view1
	           , containerChild      := view2
	           ]

	widgetShowAll window
	mainGUI
