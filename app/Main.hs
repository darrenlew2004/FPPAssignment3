module Main where

import System.Random (randomRIO)
import Data.List (transpose, intercalate, foldl')
import Control.Monad (foldM, (>=>))
import System.IO (hFlush, stdout)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))

-- Define the game state representation
data Game a = Game {
    board :: [[a]],
    emptyPos :: (Int, Int),
    size :: Int,
    moves :: Int,
    moveHistory :: [(Int, Int)],
    startTime :: Maybe UTCTime
} deriving (Show)

-- Implement Functor instance for Game to allow mapping over the board
instance Functor Game where
    fmap f game = game {
        board = (fmap . fmap) f (board game)
    }

-- Define Semigroup instance to combine two game states
instance Semigroup (Game Int) where
    (<>) g1 g2 = Game {
        board = solvedBoard (size g1),
        emptyPos = (size g1 - 1, size g1 - 1),
        size = size g1,
        moves = moves g1 + moves g2,
        moveHistory = moveHistory g1 ++ moveHistory g2,
        startTime = startTime g1
    }
      where
        solvedBoard n = chunks n ([1 .. n * n - 1] ++ [0])
        chunks _ [] = []
        chunks m xs = take m xs : chunks m (drop m xs)

-- Define Monoid instance for Game with default empty state
instance Monoid (Game Int) where
    mempty = Game {
        board = solvedBoard 3,
        emptyPos = (2, 2),
        size = 3,
        moves = 0,
        moveHistory = [],
        startTime = Nothing
    }
      where
        solvedBoard n = chunks n ([1 .. n * n - 1] ++ [0])
        chunks _ [] = []
        chunks m xs = take m xs : chunks m (drop m xs)

    mappend = (<>)

-- Initialize the game board with solvable puzzle
initBoard :: Int -> IO (Game Int)
initBoard n =
    let solved = chunks n ([1..n*n-1] ++ [0])
    in shuffleBoard solved n >>= \shuffled ->
        let (ex, ey) = findEmpty shuffled
        in getCurrentTime >>= \currentTime ->
            return Game {
                board = shuffled,
                emptyPos = (ex, ey),
                size = n,
                moves = 0,
                moveHistory = [],
                startTime = Just currentTime
            }
  where
    chunks _ [] = []
    chunks m xs = take m xs : chunks m (drop m xs)

-- Find the empty tile's position
findEmpty :: [[Int]] -> (Int, Int)
findEmpty brd = head [(i, j) | (i, row) <- zip [0..] brd, (j, v) <- zip [0..] row, v == 0]

-- Check if the game is won
isWon :: Game Int -> Bool
isWon game = board game == solvedBoard (size game)
  where
    solvedBoard n = chunks n ([1..n*n-1] ++ [0])
    chunks _ [] = []
    chunks m xs = take m xs : chunks m (drop m xs)

-- Display success message with score
printSuccess :: Game Int -> IO ()
printSuccess game =
    getCurrentTime >>= \endTime ->
        let elapsedTime = realToFrac $ diffUTCTime endTime (fromJust $ startTime game) :: Double
            score = calculateScore (moves game) elapsedTime (size game)
        in putStrLn "========================================" >>
           putStrLn "             SUCCESSFUL!                " >>
           putStrLn "    Congratulations on solving the      " >>
           putStrLn "           sliding puzzle!              " >>
           putStrLn "========================================" >>
           putStrLn ("Total moves made: " ++ show (moves game)) >>
           putStrLn ("Time taken: " ++ show elapsedTime ++ " seconds") >>
           putStrLn ("Your score: " ++ show score) >>
           mainMenu

-- Calculate score based on moves, time, and size
calculateScore :: Int -> Double -> Int -> Int
calculateScore moves time size = round $ 10000 / (fromIntegral moves * time * fromIntegral size)

-- Get valid moves based on the empty tile position
validMoves :: Game Int -> [(Int, Int)]
validMoves game = filter isValid [(x+dx, y+dy) | (dx, dy) <- directions]
  where
    (x, y) = emptyPos game
    directions = [(0,1), (0,-1), (1,0), (-1,0)]
    isValid (x', y') = x' >= 0 && x' < size game && y' >= 0 && y' < size game

-- Make a move and update the game state
makeMove :: Game Int -> (Int, Int) -> Maybe (Game Int)
makeMove game pos@(newX, newY)
    | pos `elem` validMoves game =
        let (emptyX, emptyY) = emptyPos game
            newBoard = [[if (i,j) == pos then 0
                          else if (i,j) == (emptyX, emptyY) then board game !! newX !! newY
                          else board game !! i !! j
                         | j <- [0..size game-1]]
                        | i <- [0..size game-1]]
        in Just game {
            board = newBoard,
            emptyPos = pos,
            moves = moves game + 1,
            moveHistory = emptyPos game : moveHistory game
        }
    | otherwise = Nothing

-- Undo the last move
undoMove :: Game Int -> Maybe (Game Int)
undoMove game = case moveHistory game of
    [] -> Nothing
    ((prevX, prevY):rest) -> 
        let newBoard = [[if (i,j) == (prevX, prevY) then 0
                          else if (i,j) == (emptyX, emptyY) then board game !! prevX !! prevY
                          else board game !! i !! j
                         | j <- [0..size game-1]]
                        | i <- [0..size game-1]]
            (emptyX, emptyY) = emptyPos game
        in Just game {
            board = newBoard,
            emptyPos = (prevX, prevY),
            moveHistory = rest,
            moves = moves game - 1
        }

-- Shuffle the board by making random valid moves
shuffleBoard :: [[Int]] -> Int -> IO [[Int]]
shuffleBoard brd n = foldM shuffleStep brd [1..n^3]
  where
    shuffleStep b _ =
        let empty = findEmpty b
            neighbors = filter isValid [(x+dx, y+dy) | (dx, dy) <- directions, let (x,y) = empty]
        in randomRIO (0, length neighbors - 1) >>= \idx ->
            let (newX, newY) = neighbors !! idx
            in return $ swapTiles b empty (newX, newY)
    isValid (x', y') = x' >= 0 && x' < n && y' >= 0 && y' < n
    directions = [(0,1), (0,-1), (1,0), (-1,0)]

-- Swap two tiles on the board
swapTiles :: [[Int]] -> (Int, Int) -> (Int, Int) -> [[Int]]
swapTiles brd (x1, y1) (x2, y2) = [[swap i j | j <- [0..length row - 1]] | (i, row) <- zip [0..] brd]
  where
    swap i j
        | (i,j) == (x1,y1) = brd !! x2 !! y2
        | (i,j) == (x2,y2) = brd !! x1 !! y1
        | otherwise = brd !! i !! j

-- Print the current board
printBoard :: Game Int -> IO ()
printBoard game =
    putStrLn ("\nMoves: " ++ show (moves game)) >>
    putStrLn (replicate (size game * 4 + 1) '-') >>
    mapM_ printRow (board game)
  where
    printRow row = putStrLn $ "| " ++ intercalate " | " (map showCell row) ++ " |"
    showCell 0 = " "
    showCell n = show n

-- Main game loop
gameLoop :: Game Int -> IO ()
gameLoop game =
    printBoard game >>
    if isWon game then printSuccess game
    else putStr "Enter move (w/a/s/d), u to undo, or q to quit: " >>
         hFlush stdout >>
         getLine >>= \input ->
            case input of
                "q" -> putStrLn "Thanks for playing!" >> mainMenu
                "u" -> case undoMove game of
                    Just prevGame -> gameLoop prevGame
                    Nothing -> putStrLn "No moves to undo!" >> gameLoop game
                [dir] -> case getNewPos (emptyPos game) dir >>= makeMove game of
                    Just newGame -> gameLoop newGame
                    Nothing -> putStrLn "Invalid move!" >> gameLoop game
                _ -> putStrLn "Invalid input!" >> gameLoop game

-- Get new position based on input direction
getNewPos :: (Int, Int) -> Char -> Maybe (Int, Int)
getNewPos (x, y) dir = case dir of
    'w' -> Just (x-1, y)
    's' -> Just (x+1, y)
    'a' -> Just (x, y-1)
    'd' -> Just (x, y+1)
    _   -> Nothing

-- "How It Works" Screen
howItWorks :: IO ()
howItWorks =
    putStrLn "\n=== How It Works ===" >>
    putStrLn "Objective:" >>
    putStrLn "The goal is to arrange the numbers in ascending order with the empty tile at the bottom-right." >>
    putStrLn "\nControls:" >>
    putStrLn "  - Use 'w' to move the empty tile up" >>
    putStrLn "  - Use 's' to move the empty tile down" >>
    putStrLn "  - Use 'a' to move the empty tile left" >>
    putStrLn "  - Use 'd' to move the empty tile right" >>
    putStrLn "  - Use 'u' to undo the last move" >>
    putStrLn "  - Use 'q' to quit the game" >>
    putStrLn "\nGameplay Tips:" >>
    putStrLn "  - Plan your moves to avoid unnecessary backtracking." >>
    putStrLn "  - Try to solve row by row or column by column for easier management." >>
    putStrLn "\nPress Enter to return to the main menu." >>
    getLine >>
    mainMenu

-- Main Menu with "How It Works" Option
mainMenu :: IO ()
mainMenu =
    putStrLn "\n=== Number Sliding Puzzle ===" >>
    putStrLn "1. Start 3x3 Puzzle" >>
    putStrLn "2. Start 4x4 Puzzle" >>
    putStrLn "3. Start 5x5 Puzzle" >>
    putStrLn "4. How It Works" >>
    putStrLn "5. Quit" >>
    putStr "Select option (1-5): " >>
    hFlush stdout >>
    getLine >>= \choice ->
        case choice of
            "1" -> initBoard 3 >>= gameLoop
            "2" -> initBoard 4 >>= gameLoop
            "3" -> initBoard 5 >>= gameLoop
            "4" -> howItWorks
            "5" -> putStrLn "Goodbye!"
            _    -> putStrLn "Invalid choice!" >> mainMenu

-- Main function to start the program
main :: IO ()
main = mainMenu
