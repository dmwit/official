{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

import Control.Applicative
import Control.Arrow
import Data.List hiding (find)
import Data.Monoid
import Data.Ord
import System.FilePath
import System.FilePath.Find
import System.Process

sortFilePath = sortBy . comparing $ first ((init &&& last) . splitDirectories) . splitExtension

process fp = do
	putStrLn $ "\\section{\\texttt{" <> fp <> "}}"
	case splitExtension fp of
		(base, ".hs") -> do
			putStrLn ("\\label{mod:" <> (intercalate "." . drop 1 . splitDirectories) base <> "}")
			putStrLn ("\\lstinputlisting{code/" <> fp <> "}")
		_ -> do
			putStrLn "\\begin{verbatim}"
			readFile ("code/" <> fp) >>= putStr
			putStrLn "\\end{verbatim}"
	putStrLn ""

main = do
	Just fps <- mapM (stripPrefix "code/") . sortFilePath <$> find (return True) (fileType ==? RegularFile) "code"
	mapM_ process fps
