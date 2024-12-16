module Main where

import System.Random (randomRIO)
import Data.List (transpose, intercalate)
import Control.Monad (foldM, (>=>))
import System.IO (hFlush, stdout)
import Data.Maybe (fromJust, isJust)

data Game = Game {
    board :: [[Int]],
    emptyPos :: (Int, Int),
    size :: Int,
    moves :: Int
} deriving (Show)

initBoard :: Int -> Game
initBoard n = Game {
    board = chunks n [1..n*n-1] ++ [[0]],
    emptyPos = (n-1, n-1),
    size = n,
    moves = 0
}
  where chunks _ [] = []
        chunks n xs = take n xs : chunks n (drop n xs)

isWon :: Game -> Bool
isWon game = board game == board (initBoard (size game))

printSuccess :: IO ()
printSuccess = 
    mapM_ putStrLn [
        "========================================",
        "             SUCCESSFUL!                ",
        "    Congratulations on solving the      ",
        "           sliding puzzle!              ",
        "========================================"]

validMoves :: Game -> [(Int, Int)]
validMoves game = filter isValid [(x+dx, y+dy) | (dx, dy) <- directions]
  where
    (x, y) = emptyPos game
    directions = [(0,1), (0,-1), (1,0), (-1,0)]
    isValid (x', y') = x' >= 0 && x' < size game && y' >= 0 && y' < size game

makeMove :: Game -> (Int, Int) -> Maybe Game
makeMove game pos@(newX, newY)
    | pos `elem` validMoves game = Just $ game {
        board = newBoard,
        emptyPos = pos,
        moves = moves game + 1
    }
    | otherwise = Nothing
  where
    (emptyX, emptyY) = emptyPos game
    newBoard = [[if (i,j) == pos then 0
                 else if (i,j) == (emptyX, emptyY) then board game !! newX !! newY
                 else board game !! i !! j
                | j <- [0..size game-1]]
               | i <- [0..size game-1]]

shuffleBoard :: Game -> IO Game
shuffleBoard game = 
    let numMoves = (size game)^3
        makeRandomMove g = (randomRIO (0, length (validMoves g) - 1)) >>= 
            \idx -> return $ fromJust $ makeMove g (validMoves g !! idx)
    in foldM (\g _ -> makeRandomMove g) game [1..numMoves]

printBoard :: Game -> IO ()
printBoard game = 
    putStrLn ("\nMoves made: " ++ show (moves game)) >>
    putStrLn (replicate (size game * 4 + 1) '-') >>
    printRows (board game)
  where
    printRows = foldr ((>>) . printRow) (return ())
    printRow row = 
        putStr "| " >>
        foldr (\cell acc -> printCell cell >> acc) (return ()) row >>
        putStrLn "" >>
        putStrLn (replicate (size game * 4 + 1) '-')
    printCell 0 = putStr "  | "
    printCell n = putStr (show n ++ " | ")

getNewPos :: (Int, Int) -> Char -> Maybe (Int, Int)
getNewPos (x, y) dir = case dir of
    'w' -> Just (x-1, y)
    's' -> Just (x+1, y)
    'a' -> Just (x, y-1)
    'd' -> Just (x, y+1)
    _ -> Nothing

gameLoop :: Game -> IO ()
gameLoop game = 
    printBoard game >>
    if isWon game 
    then printSuccess >> 
         putStrLn ("Total moves made: " ++ show (moves game)) >>
         mainMenu
    else putStr "Enter move (w/a/s/d) or q to quit: " >>
         hFlush stdout >>
         getChar >>= \input ->
         putStrLn "" >>
         case input of
             'q' -> putStrLn "Thanks for playing!" >> mainMenu
             dir -> handleMove game dir
  where
    handleMove g dir = 
        case getNewPos (emptyPos g) dir of
            Just newPos -> case makeMove g newPos of
                Just newGame -> gameLoop newGame
                Nothing -> putStrLn "Invalid move!" >> gameLoop g
            Nothing -> putStrLn "Invalid input! Use w/a/s/d to move or q to quit." >> 
                      gameLoop g

displayInstructions :: IO ()
displayInstructions = 
    mapM_ putStrLn [
        "\nInstructions:",
        "- Use W/A/S/D keys to move tiles",
        "- w: move up",
        "- s: move down",
        "- a: move left",
        "- d: move right",
        "- q: quit current game",
        "- Arrange numbers in order to win",
        "\nPress Enter to continue..."
    ] >>
    getLine >>
    mainMenu

mainMenu :: IO ()
mainMenu = 
    mapM_ putStrLn [
        "\n=== Number Sliding Puzzle ===",
        "1. Start 3x3 Puzzle",
        "2. Start 4x4 Puzzle",
        "3. View Instructions",
        "4. Quit"
    ] >>
    putStr "Select option (1-4): " >>
    hFlush stdout >>
    getLine >>= \choice ->
    case choice of
        "1" -> shuffleBoard (initBoard 3) >>= \game -> gameLoop game
        "2" -> shuffleBoard (initBoard 4) >>= \game -> gameLoop game
        "3" -> displayInstructions
        "4" -> putStrLn "Goodbye!"
        _ -> putStrLn "Invalid choice!" >> mainMenu

main :: IO ()
main = mainMenu