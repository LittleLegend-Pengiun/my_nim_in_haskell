nim :: IO()
nim = do putStrLn "Intializing..."
         mapM_ putStrLn (initBoard newBoard)
         turn newBoard 1

switch :: Int -> Int 
switch 1 = 2
switch 2 = 1

newBoard :: [Int]
newBoard = [1, 2, 3, 4, 5]

showStars :: Int -> String
showStars x = ["",  "*", "* *", "* * *", "* * * *", "* * * * *"] !! x

initBoard :: [Int] -> [String]
initBoard board = 
    reverse [show x ++ ": " ++ showStars (board !! (x-1)) | 
    x <- [1..5]] 

change :: [Int] -> Int -> Int -> [Int]
change board row stars = first ++ [x] ++ last
    where
        first = take (row-1) board
        x = (board !! (row - 1)) - stars
        last = drop row board
    
check :: [Int] -> Int -> Int -> Bool 
check board row stars = not (stars < 0 || stars > board !! row)

turn :: [Int] -> Int -> IO ()
turn board player = do
    putStrLn ("\nPlayer " ++ show player ++ " is playing...")
    putStrLn "Row?"
    row <- getLine
    putStrLn "How many stars?"
    stars <- getLine
    if read row > 6 || read row < 0 then do
            putStrLn "Invalid row, please enter again"
            turn board player
    else do  
        if read stars < 0 || read stars > (board !! (read row - 1)) then do
            putStrLn "Invalid stars, please enter again"
            turn board player 
        else do
            let newboard = change board (read row) (read stars)
            if isOver newboard then 
                putStrLn ("Player " ++ show player ++ " wins!!!")
            else do
                putStrLn "\nIntializing...\n"
                mapM_ putStrLn (initBoard newboard)
                turn newboard (switch player)
            
isOver :: [Int] -> Bool
isOver board = board == [0, 0, 0, 0, 0]