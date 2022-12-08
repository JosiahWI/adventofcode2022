type Name = String
type Size = Integer

newtype Path = Path [Name] deriving Show

data Filesystem = File Name Size | Dir Name [Filesystem]
  deriving Show

data FileManager = FileManager Filesystem Path
  deriving Show

mkFilesystem :: Name -> Filesystem
mkFilesystem name = Dir name []

mkFileManager :: FileManager
mkFileManager = FileManager (mkFilesystem "/") (Path ["/"])

insert :: Path -> Size -> Filesystem -> Filesystem
insert (Path []) _ fs = fs
insert _ _ fs@(File _ _) = fs
insert (Path (pathnext:filename:[])) size fs@(Dir name contents) =
  if name /= pathnext
    then fs
    else
      if size == 0
      then Dir name (mkFilesystem filename : contents)
      else Dir name (File filename size : contents)
insert (Path (pathnext:pathrest)) size fs@(Dir name contents) =
  if name /= pathnext
    then fs
    else Dir name (map (insert (Path pathrest) size) contents)

isFile :: Filesystem -> Bool
isFile (File _ _) = True
isFile (Dir _ _) = False

size :: Filesystem -> Integer
size (File _ size) = size
size (Dir _ _) = 0

sumFiles :: Filesystem -> Integer
sumFiles (File _ size) = size
sumFiles (Dir _ contents) = sum $ map size $ filter isFile contents

sumTotal :: Filesystem -> Integer
sumTotal (File _ size) = size
sumTotal (Dir _ contents) = sum $ map sumTotal contents

sumfs :: Filesystem -> Integer
sumfs fs =
  case fs of
    (File _ size)       -> 0
    fs@(Dir _ contents) -> if thisDirSize <= 100000
                           then thisDirSize + subdirScore
                           else subdirScore
      where thisDirSize = sumTotal fs
            subdirScore = sum $ map sumfs contents

allSizes :: [Integer] -> Filesystem -> [Integer]
allSizes empty fs =
  case fs of
    (File _ size)       -> empty
    fs@(Dir _ contents) -> empty ++ [thisDirSize] ++ (concat $ map (allSizes []) contents)
    where thisDirSize = sumTotal fs

processLine :: FileManager -> String -> FileManager
processLine fm@(FileManager fs path@(Path pathnames)) line =
  case args of
    ("$":"cd":"/":_)  -> FileManager fs (Path ["/"])
    ("$":"cd":"..":_) -> FileManager fs (Path (init pathnames))
    ("$":"cd":dir:_)  -> FileManager fs (Path (pathnames ++ [dir]))
    ("$":_)           -> fm
    ("dir":name:_)    -> FileManager (insert dirpath 0 fs) path
      where dirpath = (Path (pathnames ++ [name]))
    (sizestr:name:_)  -> FileManager (insert filepath size fs) path
      where size = read sizestr
            filepath = (Path (pathnames ++ [name]))
    where args = words line

main :: IO ()
main = do
  content <- readFile "input.txt"
  let initialFiles = mkFileManager
  let (FileManager fs _) = foldl processLine initialFiles $ lines content
  print $ sumfs fs
  print $ minimum $ filter (>= (sumTotal fs - 40000000)) $ allSizes [] fs
