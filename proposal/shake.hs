import Development.Shake
import Development.Shake.FilePath

-- TODO: learn enough more stuff about shake that you understand why this doesn't work correctly

sources = ["paper", "abstract", "finished_work", "introduction", "related_work", "spreadsheets"]
bibs = map (++".bib") ["bcp", "delta", "harmony", "symmetric"]
sourcesWith s = [f ++ "." ++ s | f <- sources]
pdflatex out = system' "pdflatex" ["-interaction=nonstopmode", dropExtension out]

main = shakeArgs shakeOptions $ do
	want ["paper.pdf"]
	sourcesWith "aux" *>> \_ -> do
		need (sourcesWith "tex")
		pdflatex "paper"
	["*.blg", "*.bbl"] *>> \[blg, bbl] -> do
		need (sourcesWith "aux" ++ bibs)
		system' "bibtex" [dropExtension blg]
	"*.pdf" *> \out -> do
		need (replaceExtension out "tex" : replaceExtension out "bbl" : sourcesWith "aux")
		pdflatex out
