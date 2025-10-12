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
shuffle gen list = randomElem : shuffle newGen newList
  where
    randomTuple = randomR (0, (length list) - 1) gen
    randomIndex = fst randomTuple
    newGen = snd randomTuple
    randomElem = list !! randomIndex
    newList = take randomIndex list ++ drop (randomIndex + 1) list

-- TODO: different sizes and difficulties
genBoard :: StdGen -> Board
genBoard g = mapCells $ mapRows finalRow
  where
    row = [1 .. 9]
    mapCells = fmap (fmap Just)
    mapRows = fromList . map fromList

    row1 = shuffle g row
    row2 = shift' 3 row1
    row3 = shift' 3 row2
    row4 = shift' 1 row3
    row5 = shift' 3 row4
    row6 = shift' 3 row5
    row7 = shift' 1 row6
    row8 = shift' 3 row7
    row9 = shift' 3 row8

    renewRow = [row1, row2, row3, row4, row5, row6, row7, row8, row9]

    colSh1 = shuffle g [0, 1, 2]
    colSh2 = shuffle g [3, 4, 5]
    colSh3 = shuffle g [6, 7, 8]
    rowSh1 = shuffle g [0, 1, 2]
    rowSh2 = shuffle g [3, 4, 5]
    rowSh3 = shuffle g [6, 7, 8]

    colResult = colSh1 ++ colSh2 ++ colSh3
    rowResult = rowSh1 ++ rowSh2 ++ rowSh3

    finalCol = foldr (\i s -> foldr (\j s' -> (renewRow !! i) !! (colResult !! j) : s') [] [0 .. 8] : s) [] [0 .. 8]
    finalRow = foldr (\i s -> foldr (\j s' -> (finalCol !! i) !! (rowResult !! j) : s') [] [0 .. 8] : s) [] [0 .. 8]

maskBoard :: StdGen -> Board -> Board
maskBoard g b = foldr (\x b' -> setCell Nothing x b') b (take 60 $ randomRs ((0, 0), (8, 8)) g)

setCell :: a -> Pos -> Seq (Seq a) -> Seq (Seq a)
setCell c = mapCell (const c)

mapCell :: (a -> a) -> Pos -> Seq (Seq a) -> Seq (Seq a)
mapCell f p b = update y (update x (f cell) row) b
  where
    x = fst p
    y = snd p
    row = fromMaybe empty (b !? y)
    cell = fromJust (row !? x) -- yes yes very good practice

split3 :: Seq a -> Seq (Seq a)
split3 Sequence.Empty = empty
split3 r = Sequence.take 3 r <| split3 (Sequence.drop 3 r)

intercal :: Seq a -> Seq (Seq a) -> Seq a
intercal s ss = join (Sequence.intersperse s ss)

interPipe :: Seq String -> Seq String
interPipe = intercal (singleton "|") . split3

pad :: String -> String
pad s = " " ++ s ++ " "

boardToStr :: Board -> BoardStr
boardToStr = fmap (pad . maybe "_" show <$>) -- mmh i love destroying readability

interGrid :: BoardStr -> BoardStr
interGrid = intercal (singleton $ interPipe $ Sequence.replicate 9 "___") . split3 . fmap interPipe

setPosStr :: Pos -> BoardStr -> BoardStr
setPosStr = mapCell (map repl)
  where
    repl ' ' = '*'
    repl a = a

printBoard :: Pos -> Board -> IO ()
printBoard p b = mapM_ (putStrLn . concat) $ interGrid $ setPosStr p $ boardToStr b

move :: Pos -> Char -> Pos
move (0, y) 'h' = (8, y)
move (x, y) 'h' = (x - 1, y)
move (x, 8) 'j' = (x, 0)
move (x, y) 'j' = (x, y + 1)
move (x, 0) 'k' = (x, 8)
move (x, y) 'k' = (x, y - 1)
move (8, y) 'l' = (0, y)
move (x, y) 'l' = (x + 1, y)

isFull :: Board -> Bool
isFull = all (notElem Nothing)

validateRow :: Seq Cell -> Bool
validateRow r = all (\n -> 0 < n && n < 10) nums && (sum nums == 45)
  where
    nums = fromMaybe (-1) <$> r

cellAt :: Int -> Int -> Board -> Cell
cellAt x y b = num (lst (b !? y) !? x)
  where
    num = fromMaybe (Just (-1))
    lst = fromMaybe empty

cellsFromTo :: Pos -> Pos -> Board -> Seq Cell
cellsFromTo f t b = foldr (\x s -> foldr (\y s' -> cellAt x y b <| s') s [(snd f) .. (snd t)]) empty [(fst f) .. (fst t)]

validateGroups :: Board -> Bool
validateGroups b = all3 (\i -> all3 (\j -> validateRow (cellsFromTo (i * 3, j * 3) (i * 3 + 2, j * 3 + 2) b)))
  where
    all3 f = all f [0 .. 2]

switcheroo :: Board -> Board
switcheroo b = fold9 (\x s -> fold9 (\y col -> cellAt y x b <| col) <| s)
  where
    fold9 f = foldr f empty [0 .. 8]

validate :: Board -> Bool
validate b = all validateRow b && all validateRow cols && validateGroups b
  where
    cols = switcheroo b

handleInput _ _ 'q' = exitSuccess
handleInput b p 'x' = loopedyLoop (setCell Nothing p b) p
handleInput b p i
  | i `elem` ['h', 'j', 'k', 'l'] = loopedyLoop b (move p i)
  | isDigit i = loopedyLoop (setCell (Just $ digitToInt i) p b) p
handleInput b p _ = loopedyLoop b p

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
            then 
              exitSuccess
            else do
              gen <- newStdGen
              loopedyLoop (maskBoard gen $ genBoard gen) (0, 0)
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
  gen <- newStdGen
  loopedyLoop (maskBoard gen $ genBoard gen) (0, 0)
