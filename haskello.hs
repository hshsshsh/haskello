import Data.Char -- for digitToInt, ord in crToCoor
import Data.List.Split
import Data.List -- for zipWith in numberer

type Board = [[Char]]
type Coor = (Int,Int) -- abbreviation of "coordinate"

-- main statement and subroutine ("loop") --

main :: IO ()
main = do printBoard board
          loop board

loop :: Board -> IO ()
loop bd = do line       <- getLine
             coordinate <- return . fst $ parse line
             disk       <- return . snd $ parse line
             newBoard   <- return $ replace bd coordinate disk
             n          <- return 0 :: IO Int
             flipped    <- return $ flipping newBoard n coordinate disk
             printBoard flipped
             loop newBoard

flipping :: Board -> Int -> Coor -> Char -> Board
flipping bd n coordinate disk
  | n >= 8 = bd
  | otherwise = let directions = directionList!!n
                    flipped = flipDisk bd coordinate disk directions
                in  flipping flipped (n+1) coordinate disk

-- parse user input and extract coordinate and kind of disk --

parse :: String -> (Coor,Char) -- input e.g. : "a1 @"
parse line =
  let splited = splitOn " " line
      coordinate = crToCoor $ splited!!0
      disk = head $ splited!!1
  in (coordinate, disk)

-- print current game board with header and line number  --

printBoard :: Board -> IO ()
printBoard bd = do putStrLn header
                   putBoard $ numberer bd

putBoard :: Board -> IO () -- sub routine of printBoard
putBoard bd = putStr $ unlines bd

header :: String
header = " " ++ ['a'..'h']

numberer :: Board -> Board
numberer bd = zipWith (:) ['1'..'8'] bd

-- flipDisk --

flipDisk :: Board -> Coor -> Char -> Coor -> Board
flipDisk bd (x,y) disk directions@(xDirection,yDirection)
  | not (fst further `elem` [0..7] && snd further `elem` [0..7]) = bd
  | nextDisk == ' ' || furtherDisk == ' ' = bd
  | nextDisk == disk = bd
  | nextDisk == furtherDisk = flipDisk bd next disk directions
  | otherwise = flipDisk newBoard back disk directions
  where back = ( x - xDirection, y - yDirection )
        next = ( x + xDirection, y + yDirection )
        further = ( x + 2*xDirection, y + 2*yDirection )
        nextDisk = getDisk bd next
        furtherDisk = getDisk bd further
        newBoard = replace bd next disk

getDisk :: Board -> Coor -> Char
getDisk bd (x,y) = (bd!!y)!!x

directionList :: [(Int,Int)]
directionList =
  filter (/=(0,0)) $ (\x y -> (y,x)) <$> [-1..1] <*> [-1..1]
  -- assignment of directions : --
  -- 0 1 2 --
  -- 3 x 4 --
  -- 5 6 7 --

-- create board with new disk and its position --

replace :: Board -> Coor -> Char -> Board
replace bd (x,y) disk =
  let line = bd!!y
      ---------------------------
      (a, _:bs) = splitAt x line
      newLine = a ++ disk:bs
      ---------------------------
      (c, _:ds) = splitAt y bd
      newBoard = c ++ newLine:ds
  in  newBoard

-- transform (column, row) form to coordinate form; e.g. "a7" -> (0,6) --

crToCoor :: String -> Coor -- cr means Column & Row form
crToCoor line = let column = head line
                    row    = digitToInt $ last line
                    x = ord column - ord 'a'
                    y = row - 1
                in (x,y)

board :: Board
board = ["        "
        ,"        "
        ,"        "
        ,"   O@   "
        ,"   @O   "
        ,"        "
        ,"        "
        ,"        "
        ]
