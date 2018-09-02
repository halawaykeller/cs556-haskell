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

currentWord :: String -> String
currentWord = map (\x -> '.') 

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
                if checkAlpha line then return line else checkInput

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

{- 
 State of the world:
    Current word
    Guess word
    Number of incorrect guess
    Incorrectly guessed letters
    If the game is won or not
    Partially revealed word 
-}


playGame' :: Int -> Int -> String -> String -> IO (Int, Int)
playGame' turn n [] word = return (turn, n)
playGame' turn n (c:cs) word 
                    | checkGuess c word = do
                                            putStrLn $ show turn ++ ".     Enter the letter(s) you want to guess: "
                                            putStrLn ("\nThat letter was correct.\n\n")
                                            putStrLn $ getHangman n
                                            playGame' (turn+1) n cs word 
                    | otherwise = do
                                     putStrLn ("\nThat letter was incorrect.\n\n")
                                     putStrLn $ getHangman $ n+1
                                     playGame' (turn+1) (n+1) cs word

 -- play the game for each character in the line
playGame :: Int -> Int -> String -> IO ()
playGame turn n word = do
                        putStrLn $ show turn ++ ".     Enter the letter(s) you want to guess: "
                        line  <- checkInput
                        state <- playGame' turn n line word 
                        playGame (fst state) (snd state) word



main = do 
        putStrLn logo
        s <- readFile "words.txt"
        num <- randomRIO (0, getListLength s) :: IO Int
        putStrLn $ intro $ currentWord $ getWord s num

        playGame 0 0 (getWord s num)


