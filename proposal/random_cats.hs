import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.RWS
import Data.List
import Data.List.Split
import Data.Tree
import System.Random
import System.Directory
import System.FilePath.Posix

-- this is not really the right type for a forest with holes, but whatever
fillTree :: Forest (Maybe a) -> [[a]] -> Forest a
fillTree = evalState . goForest where
	goForest f = concat <$> mapM goTree f
	goTree (Node Nothing _) = do
		cs <- state (head &&& tail)
		return [Node c [] | c <- cs]
	goTree (Node (Just a) f) = (:[]) . Node a <$> goForest f

allCats = filter isCat <$> getDirectoryContents "cats" where
	isCat s = ".jpg" `isSuffixOf` s && not ("_full.jpg" `isSuffixOf` s)
allTags = ["costume", "food", "onlyface", "cute", "dog", "invisible", "kitten", "apathy"]

select cs = (cs!!) <$> randomRIO (0, length cs-1)
noAdjacentRepeats = map head . group

randomCats n = allCats >>= replicateM n . select >>= return . noAdjacentRepeats
randomTags n = do
	m <- randomRIO (0,n)
	tags <- replicateM m (select allTags)
	return (nub tags)
randomReplicas = do
	chunks <- replicateM 24 (randomRIO (3,5))
	cats_  <- randomCats (sum chunks * 2) -- we really want (sum chunks) random cats, so generating (sum chunks * 2) of them and then filtering _probably_ gets enough of them
	let catGroups = splitPlacesBlanks chunks cats_
	    cats      = concat catGroups
	tags   <- mapM (const (randomTags 3)) cats
	return (fillTree skeleton catGroups, zip cats tags)
	where
	skeleton = [Node (Just "2012") skeleMon, Node (Just "2013") skeleMon]
	skeleMon = [Node (Just m) [Node Nothing []] | m <- months]
	months   = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

frst (a,b,c) = a
scnd (a,b,c) = b
thrd (a,b,c) = c
notSillyRunRWS r s rws = runRWS rws r s
written = thrd . notSillyRunRWS () False
leaf = put True
nodeName = do
	v <- get
	tell $ if v then "last" else "first"

formatWeb web = written $ do
	tell "\\begin{scope}[start chain=going below,node distance=0]\\draw(root.north -| last-fs-pic.east) +(1.3,0)"
	mapM_ go web
	tell ";\\end{scope}\\webborder{first}{last}"
	where
	go (c, t) = do
		tell "node[on chain,anchor=north,scale=0.06]("
		nodeName
		tell "-web-pic){\\lolcat{"
		tell (dropExtension c)
		tell "}}node[right=of "
		nodeName
		tell "-web-pic,scale=0.06]("
		nodeName
		tell "-web-tag){\\tiny ["
		tell (formatTags t)
		tell "]}"
		leaf
	formatTags [] = "\\ "
	formatTags ts = intercalate "," ts

formatFS fs = written $ do
	tell "\\draw[busy picture tree] node (root) {}"
	mapM_ (go 0) fs
	tell ";\\fsborder{first}{last}"
	where
	leaf = put True
	nodeName = do
		v <- get
		tell $ if v then "last" else "first"

	scale 0 = 0.5
	scale 1 = 0.1
	go depth (Node n []) = do
		tell "child{node[scale=0.1]("
		nodeName
		tell "-fs-name){\\tiny "
		tell n
		tell "}node[below,scale=0.05]("
		nodeName
		tell $ "-fs-pic){\\lolcat{" ++ dropExtension n ++ "}}"
		tell "}"
		leaf
	go depth (Node n cs) = do
		tell "child{node[inner sep=0,scale="
		tell . show . scale $ depth
		tell "]("
		tell n
		tell "){\\strut "
		tell n
		tell "}"
		mapM_ (go (depth+1)) cs
		tell "}"

main = randomReplicas >>= putStrLn . uncurry (<>) . (formatFS *** formatWeb)
