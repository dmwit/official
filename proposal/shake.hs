import Control.Monad
import Development.Shake
import Development.Shake.FilePath
import System.Posix.Env

-- TODO: learn enough more stuff about shake that you understand why this doesn't work correctly
-- TODO: handle graphics files and stuff
-- TODO: make a "clean" target

system_ prog args = do
	Exit _ <- command [] prog args
	return ()

sources = ["paper", "finished_work", "front_matter", "introduction", "related_work", "spreadsheets"]
bibs = map (++".bib") ["bcp", "delta", "harmony", "spreadsheets", "symmetric"]
sourcesWith s = [f ++ "." ++ s | f <- sources]
pdflatex  out = system' "pdflatex" ["-interaction=nonstopmode", dropExtension out]
pdflatex_ out = system_ "pdflatex" ["-interaction=nonstopmode", dropExtension out]
lowerExtension s = case takeExtension s of
	"pdf" -> replaceExtension s "aux"
	"aux" -> replaceExtension s "tex"
	_ -> s

main = shakeArgs shakeOptions $ do
	want ["paper.pdf", "presentation.pdf"]
	sourcesWith "aux" *>> \_ -> do
		need (sourcesWith "tex")
		-- when we haven't run bibtex yet, pdflatex will fail, so ignore that
		-- failure here and report on it during the second run in the pdf rule
		pdflatex_ "paper"
	["*.blg", "*.bbl"] *>> \[blg, bbl] -> do
		need (sourcesWith "aux" ++ bibs)
		system' "bibtex" [dropExtension blg]
	"paper.pdf" *> \out -> do
		need (replaceExtension out "tex" : replaceExtension out "bbl" : sourcesWith "aux")
		pdflatex out
	"presentation.pdf" *> \out -> do
		need ["presentation.aux"]
		liftIO $ setEnv "TEXINPUTS" "..:" True
		pdflatex out
	"presentation.aux" *> \out -> do
		need ["presentation.tex"]
		liftIO $ setEnv "TEXINPUTS" "..:" True
		pdflatex out
