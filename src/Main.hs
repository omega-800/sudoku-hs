import Control.Monad (join)
import Data.Char (digitToInt, isDigit)
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence (Seq, empty, fromList, singleton, update, (!?), (<|), (><), (|>))
import qualified Data.Sequence as Sequence
import GHC.IO.Handle (hSetBuffering)
import GHC.IO.Handle.FD (stdin)
import GHC.IO.Handle.Types (BufferMode (NoBuffering))
import System.Exit (exitSuccess)
import System.Random (Random (randomR, randomRs), StdGen, newStdGen)
import System.Random.Stateful (Random (randoms))

type Cell = Maybe Int

type Board = Seq (Seq Cell)

type BoardStr = Seq (Seq String)

type Pos = (Int, Int)

shift :: Int -> Seq a -> Seq a
shift i s = Sequence.drop i s >< Sequence.take i s

shift' :: Int -> [a] -> [a]
shift' i s = drop i s ++ take i s

shuffle :: StdGen -> [a] -> [a]
shuffle gen [] = []
shuffle gen list = elem : shuffle newGen newList
  where
    (idx, newGen) = randomR (0, (length list) - 1) gen
    elem = list !! idx
    newList = take idx list ++ drop (idx + 1) list

szg :: Foldable a => a b -> Int
szg = floor . sqrt . fromIntegral . length

splits :: [a] -> [[a]]
splits [] = []
splits r = spl grSize r
  where
    grSize = szg r
    spl _ [] = []
    spl n r' = take n r' : spl n (drop n r')

genBoard :: StdGen -> Int -> Board
genBoard g s = mapCells $ mapRows finalRow
  where
    size = case s of
      1 -> 4
      2 -> 9
      3 -> 16
    grSize = floor $ sqrt $ fromIntegral size
    mapCells = fmap (fmap Just)
    mapRows = fromList . map fromList
    shiftNrForPos i = (grSize * i - (grSize - 1) * floor ((fromIntegral i) / (fromIntegral grSize))) `mod` size
    renewRow = foldr (\(i, el) res -> (shift' (shiftNrForPos i) el) : res) [] (zip [0 .. size - 1] $ replicate size (shuffle g [1..size]))
    sflRow = concat $ map (shuffle g) (splits [0 .. size - 1])
    finalRow = gen $ gen renewRow
    gen x = foldn (\i s -> foldn (\j s' -> (x !! i) !! (sflRow !! j) : s') : s)
    foldn f = foldr f [] [0 .. size - 1]

maskBoard :: StdGen -> Int -> Board -> Board
maskBoard g d b = foldr (\x b' -> setCell Nothing x b') b (take (((szg b) ^ 2) * d) $ randomRs ((0, 0), (size - 1, size - 1)) g)
  where
    size = length b

setCell :: a -> Pos -> Seq (Seq a) -> Seq (Seq a)
setCell c = mapCell (const c)

mapCell :: (a -> a) -> Pos -> Seq (Seq a) -> Seq (Seq a)
mapCell f (x, y) b = update y (update x (f cell) row) b
  where
    row = fromMaybe empty (b !? y)
    cell = fromJust (row !? x) -- yes yes very good practice

splitg :: Seq a -> Seq (Seq a)
splitg Sequence.Empty = empty
splitg r = spl grSize r
  where
    grSize = szg r
    spl _ Sequence.Empty = empty
    spl n r' = Sequence.take n r' <| spl n (Sequence.drop n r')

intercal :: Seq a -> Seq (Seq a) -> Seq a
intercal s ss = join (Sequence.intersperse s ss)

interPipe :: Seq String -> Seq String
interPipe = intercal (singleton "|") . splitg

pad :: String -> String
pad s = " " ++ s ++ " "

boardToStr :: Board -> BoardStr
boardToStr = fmap (pad . maybe "_" show <$>) -- mmh i love destroying readability

interGrid :: BoardStr -> BoardStr
interGrid b = intercal (singleton $ interPipe $ Sequence.replicate (length b) "___") (splitg $ fmap interPipe b)

setPosStr :: Pos -> BoardStr -> BoardStr
setPosStr = mapCell (map repl)
  where
    repl ' ' = '*'
    repl a = a

printBoard :: Pos -> Board -> IO ()
printBoard p b = mapM_ (putStrLn . concat) $ interGrid $ setPosStr p $ boardToStr b

move :: Board -> Pos -> Char -> Pos
move b (x, y) c
  | c == 'h' && x == 0 = (maxSize, y)
  | c == 'h' = (x - 1, y)
  | c == 'j' && y == maxSize = (x, 0)
  | c == 'j' = (x, y + 1)
  | c == 'k' && y == 0 = (x, maxSize)
  | c == 'k' = (x, y - 1)
  | c == 'l' && x == maxSize = (0, y)
  | c == 'l' = (x + 1, y)
  where
    maxSize = (length b) - 1

isFull :: Board -> Bool
isFull = all (notElem Nothing)

validateRow :: Seq Cell -> Bool
validateRow r = all (\n -> 0 < n && n <= size) nums && (sum nums == sum [1 .. size])
  where
    nums = fromMaybe (-1) <$> r
    size = length r

cellAt :: Int -> Int -> Board -> Cell
cellAt x y b = num (lst (b !? y) !? x)
  where
    num = fromMaybe $ Just (-1)
    lst = fromMaybe empty

cellsFromTo :: Pos -> Pos -> Board -> Seq Cell
cellsFromTo (fx,fy) (tx,ty) b = foldr (\x s -> foldr (\y s' -> cellAt x y b <| s') s [fy .. ty]) empty [fx .. tx]

validateGroups :: Board -> Bool
validateGroups b = all3 (\i -> all3 (\j -> validateRow $ square (i,j) b))
  where
    all3 f = all f [0 .. (grSize - 1)]
    grSize = szg b
    square (x,y) = cellsFromTo (x * grSize, y * grSize) ((x + 1) * grSize - 1, (y + 1) * grSize - 1)

switcheroo :: Board -> Board
switcheroo b = fold9 (\x s -> fold9 (\y col -> cellAt y x b <| col) <| s)
  where
    fold9 f = foldr f empty [0 .. ((length b) - 1)]

validate :: Board -> Bool
validate b = all validateRow b && all validateRow cols && validateGroups b
  where
    cols = switcheroo b

handleInput _ _ 'q' = exitSuccess
handleInput b p 'x' = loopedyLoop (setCell Nothing p b) p
handleInput b p i
  | i `elem` ['h', 'j', 'k', 'l'] = loopedyLoop b (move b p i)
  | isDigit i && i /= '0' = loopedyLoop (setCell (Just $ digitToInt i) p b) p
handleInput b p _ = loopedyLoop b p

beginGame :: IO ()
beginGame = do
  gen <- newStdGen
  putStrLn "Choose difficulty (1,2,3) "
  d <- (getValidChar ['1', '2', '3'])
  putStrLn "Choose size (1,2) "
  s <- (getValidChar ['1', '2'])
  loopedyLoop (maskBoard gen (digitToInt d) $ genBoard gen (digitToInt s)) (0, 0)
  where
    getValidChar valid = do
      d <- getChar
      if d `elem` valid
        then return d
        else getValidChar valid

endGame :: Board -> Pos -> IO ()
endGame b p = do
  putStrLn "Finished! check? (y)"
  input <- getChar
  if input == 'y'
    then do
      if validate b
        then do
          putStrLn "Correct! :)"
          putStrLn "Start new game? (Y/n)"
          input <- getChar
          if input == 'n'
            then exitSuccess
            else beginGame
        else
          putStrLn "Incorrect... :("
    else
      handleInput b p input
  exitSuccess

loopedyLoop :: Board -> Pos -> IO ()
loopedyLoop b p = do
  putStrLn "\ESC[2J"
  mapM_ putStrLn ["h,j,k,l to move", "1,2,3,4,5,6,7,8,9 to set number", "x to clear cell", "q to quit", "position: " ++ show p, ""]
  printBoard p b
  if isFull b
    then
      endGame b p
    else do
      input <- getChar
      handleInput b p input

main = do
  hSetBuffering stdin NoBuffering
  beginGame
