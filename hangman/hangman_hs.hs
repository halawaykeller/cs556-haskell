import Data.List.Split
import System.Random
import Data.Char
import Control.Monad

{-
    State of the world:
    Current word
    Number of incorrect guesses
    Correctly guessed letters
    Incorrectly guessed letters
    If the game is won or not
    Partially revealed word 

    // take each character and play a game of hangman each printing 
-}

case0 :: String
case0 = "Amount of wrong letters: 0 \n\n" ++
        "\n" ++
        "\n" ++
        "\n" ++
        "\n" ++
        "\n" ++
        "\n" ++
        "____________\n\n"

case1 :: String
case1 = "Amount of wrong letters: 1 \n\n" ++
        "\n"    ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "__|_________\n\n"

case2 :: String
case2 = "Amount of wrong letters: 2 \n\n" ++
        "  _______\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "__|_________\n\n" 

case3 :: String
case3 = "Amount of wrong letters: 3 \n\n" ++
        "  _______\n" ++
        "  |/\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "__|_________\n\n" 

case4 :: String
case4 = "Amount of wrong letters: 4 \n\n" ++
        "  _______\n" ++
        "  |/   | \n" ++
        "  |    O \n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "__|_________\n\n" 

case5 :: String
case5 = "Amount of wrong letters: 5 \n\n" ++
        "  _______\n" ++
        "  |/   | \n" ++
        "  |    O \n" ++
        "  |    |\n" ++
        "  |    |\n" ++
        "  |\n" ++
        "__|_________\n\n" 

case6 :: String
case6 = "Amount of wrong letters: 6 \n\n" ++
        "  _______\n" ++
        "  |/   | \n" ++
        "  |    O \n" ++
        "  |   \\|\n" ++
        "  |    | \n" ++
        "  |\n" ++
        "__|_________\n\n"

case7 :: String
case7 = "Amount of wrong letters: 7 \n\n" ++
        "  _______\n" ++
        "  |/   | \n" ++
        "  |    O \n" ++
        "  |   \\|/\n" ++
        "  |    | \n" ++
        "  |\n" ++
        "__|_________\n\n"

case8 :: String
case8 = "Amount of wrong letters: 8 \n\n" ++
        "  _______\n" ++
        "  |/   | \n" ++
        "  |    O \n" ++
        "  |   \\|/\n" ++
        "  |    | \n" ++
        "  |   /\n" ++
        "__|_________\n\n"

case9 :: String 
case9 = "Amount of wrong letters: 9 \n\n" ++
        "  _______\n" ++
        "  |/   | \n" ++
        "  |    O \n" ++
        "  |   \\|/\n" ++
        "  |    | \n" ++
        "  |   / \\\n" ++
        "__|_________\n\n"

case10 :: String
case10 = "Amount of wrong letters: 10 \n\n" ++ 
         "  _______\n" ++
         "  |/   | \n" ++
         "  |    X \n" ++
         "  |   \\|/\n" ++
         "  |    | \n" ++
         "  |   / \\\n" ++
         "__|_________\n\n" 


logo :: String
logo =  "--------------------------------------------\n" ++
        "| #  #   #   #   #  #### #   #   #   #   # |\n" ++
        "| #  #  # #  ##  # #     ## ##  # #  ##  # |\n" ++ 
        "| #### ##### # # # #  ## # # # ##### # # # |\n" ++
        "| #  # #   # #  ## #   # #   # #   # #  ## |\n" ++
        "| #  # #   # #   #  ###  #   # #   # #   # |\n" ++  
        "--------------------------------------------\n\n"


intro :: String -> String
intro xs =   "Welcome to the game Hangman!\n\n" ++
             "The objective in this game is to guess the word.\n" ++
             "You can enter both uppercase and lowercase letters.\n" ++
             "If you think you know the word, you can type it in.\n" ++
             "You will lose if you have guessed 10 letters wrong.\n\n" ++
             "This is the word you need to guess: " ++ xs ++ "\n\n"

reveal :: Char -> Char -> Char
reveal c x
        | c == x = c
        | otherwise = '.'

currentWord :: String -> String -> String
currentWord [] word = map (\x -> '.') word
currentWord guess word = foldr mergeString [] list where list = [[reveal c x | x <- word] | c <- guess]

mergeString :: String -> String -> String
mergeString [] [] = []
mergeString x [] = x
mergeString [] x = x
mergeString (x:xs) (y:ys) = (compareChar x y) : (mergeString xs ys)

compareChar :: Char -> Char -> Char
compareChar ('.') x = x 
compareChar x ('.') = x
compareChar _ _ = '.'

getListLength :: String -> Int
getListLength s = length $ splitOn "|" s

getWord :: String -> Int -> String
getWord s num = s' !! num where s' = splitOn "|" s

checkGuess :: Char -> String -> Bool
checkGuess guess word = elem guess word


checkAlpha :: String -> Bool
checkAlpha s = all (==True) (map isLetter s) 

checkInput :: IO String
checkInput = do
                line <- getLine
                if checkAlpha line then return line else do 
                                                            putStrLn ("Only alphanumeric symbols are allowed (a-z, A-Z), try again:\n")
                                                            checkInput 

getHangman :: Int -> String
getHangman n 
            | n == 0 = case0
            | n == 1 = case1
            | n == 2 = case2
            | n == 3 = case3
            | n == 4 = case4 
            | n == 5 = case5
            | n == 6 = case6
            | n == 7 = case7
            | n == 8 = case8
            | n == 9 = case9
            | n == 10 = case10

getFst :: (a, b, c, d) -> a
getFst (a, _, _, _) = a

getSnd :: (a, b, c, d) -> b
getSnd (_, b, _, _) = b

getThrd ::(a, b, c, d) -> c
getThrd (_, _, c, _) = c

getFrth :: ( a, b, c, d) -> d
getFrth (_, _, _, d) = d

resultsTitle:: String
resultsTitle = "---------------\n" ++
               "--- Results ---\n" ++
               "---------------\n\n"

results :: Bool -> String -> String
results (True) word = resultsTitle ++ "Congratulations you guessed the right word!\n\n"
results (False) word = resultsTitle ++ "You guessed the wrong word. The word was "  ++ word ++ " better luck next time!\n\n"

-- First Bool is won the game T / F Second Bool is game over T / F
checkGame :: Int -> String -> String -> (Bool, Bool)
checkGame (10) partialWord word = (False, True)
checkGame wrongGuesses partialWord word 
                                    | partialWord == word = (True, True)
                                    | otherwise = (False, False)

playGame' :: Int -> Int -> String -> String -> String -> (Bool, Bool) -> IO (Int, Int, String, (Bool, Bool))
playGame' turn n [] word partialWord status = return (turn, n, partialWord, status)
playGame' turn n (c:cs) word partialWord status
                    | checkGuess c word = do
                                            putStrLn ("\nThat letter was correct.\n")
                                            putStrLn ("The word including the letters you guessed: " ++ (currentWord (c:partialWord) word) ++ "\n")
                                            let check = checkGame n (currentWord (c:partialWord) word) word
                                            putStrLn $ getHangman n
                                            if check == (True, True) || check == (False, True) 
                                                then return (turn, n, (c:partialWord), check)
                                                else playGame' (turn+1) n cs word (c:partialWord) (False, False)
                    | otherwise = do
                                     putStrLn ("\nThat letter was incorrect.\n")
                                     putStrLn ("The word including the letters you guessed: " ++ (currentWord partialWord word) ++ "\n")
                                     let check = checkGame (n+1) (currentWord partialWord word) word
                                     putStrLn $ getHangman $ n+1
                                     if check == (True, True) || check == (False, True) 
                                                then return ((turn+1), (n+1), partialWord, check)
                                                else playGame' (turn+1) (n+1) cs word partialWord (False, False)

 -- play the game for each character in the line
playGame :: Int -> Int -> String -> (Bool, Bool) -> String -> IO ()
playGame turn n partialWord status word = do
                                    putStrLn $ show turn ++ ".     Enter the letter(s) you want to guess: "
                                    line  <- checkInput
                                    state <- playGame' turn n line word partialWord status
                                    let check = getFrth state
                                    if check == (True, True) || check == (False, True) 
                                        then putStrLn (results (fst check) word)
                                        else playGame (getFst state) (getSnd state) (getThrd state) (getFrth state) word

main = do 
        putStrLn logo
        s <- readFile "words.txt"
        num <- randomRIO (0, getListLength s) :: IO Int
        putStrLn $ intro $ currentWord [] $ getWord s num
        playGame 0 0 [] (False, False) (getWord s num)


