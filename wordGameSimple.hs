import System.IO
import System.Exit
import System.Random
import Data.Char(toLower)
import Data.Set(fromList, size)
import Control.Monad(when)

letters = ['a'..'z']

badNumLetters = "Wrong number of letters."
notAWord = "Not a valid word."
winMessage = "Congratulations! You win!"

prompt = "Enter a word to get its score or q to quit."

type Word = String
type Issue = String
type WordMaster = (Word -> Either Issue Int)

numCommonLetters :: Word -> Word -> Int
numCommonLetters goal guess = length $ filter (`elem` guess) goal

makeWordMaster :: [Word] -> Word -> WordMaster
makeWordMaster validGuesses goal guess
    | (length guess) /= (length goal) = Left badNumLetters
    | guess == goal                   = Left winMessage
    | not $ guess `elem` validGuesses = Left notAWord
    | otherwise                       = Right $ numCommonLetters goal guess

compatible :: Word -> Int -> Word -> Bool
compatible guess score goal = (numCommonLetters goal guess) == score

getValidGuesses :: IO [Word]
getValidGuesses = do 
    contents <- readFile "validguesses.txt"
    return (lines contents)

startGame :: [Word] -> Word -> IO ExitCode
startGame validGuesses word = do
    putStrLn $ "Playing with a " ++ (show $ length word) ++ " letter word."
    let wordMaster = makeWordMaster validGuesses word
    mainLoop wordMaster word

mainLoop :: WordMaster -> Word -> IO ExitCode
mainLoop master word = do
    putStrLn prompt
    input <- getLine
    let guess = map toLower input
    when (guess == "q") $ do
        putStrLn $ "The word was " ++ word
        exitSuccess
    let result = master guess
    case result of 
        Left issue -> do
            putStrLn issue
            if issue == winMessage 
                then exitSuccess
                else mainLoop master word
        Right common -> do
            putStrLn $ "The word " ++ guess ++ " has " 
                    ++ (show common) ++ " letters in common"
            mainLoop master word 

chooseRandomWord :: [Word] -> StdGen -> Word
chooseRandomWord validWords gen = 
    let (index, _) = randomR (0, length validWords) gen
    in  validWords !! index

filterValidMasterWords :: [Word] -> [Word]
filterValidMasterWords guesses =
    let uniqueChars = size . fromList
        noDuplicates guess = uniqueChars guess == length guess
    in  filter noDuplicates guesses

main = do
    gen <- getStdGen
    validGuesses <- getValidGuesses
    let validMasterWords = filterValidMasterWords validGuesses
        word = chooseRandomWord validMasterWords gen
    startGame validGuesses word
