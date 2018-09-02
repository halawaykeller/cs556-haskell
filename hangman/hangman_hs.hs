import Data.List.Split
import System.Random
import Data.Maybe (listToMaybe)

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

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads

-- getWord :: [String] -> Int -> String
-- getWord s num = s !! num


playGame :: Int -> IO ()
playGame n = do
            putStrLn $ show n ++ ".     Enter the letter(s) you want to guess: "
            maybeInt <- fmap maybeRead getLine :: IO (Maybe String)
            maybe  (putStrLn "Only alphanumeric symbols are allowed (a-z, A-Z), try again: ")
                   (putStrLn . ("The Int is " ++) . show)
                    maybeInt 
            -- putStrLn v
            playGame (n+1)






main = do 
        putStrLn logo
        s <- readFile "words.txt"
        num <- randomRIO (0, getListLength s) :: IO Int
        putStrLn $ intro $ currentWord $ getWord s num
        playGame 0


