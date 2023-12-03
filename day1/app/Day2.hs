module Day2 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.Char (digitToInt)
import qualified Data.Map as Map

data Colour = Red | Green | Blue deriving (Show, Eq, Ord)

type ColourPair = (Colour, Int)

type MaxSet = Map.Map  Colour Int

type Parser = Parsec Void String

answer :: IO ()
answer = do
    inp <- lines <$> readFile "text\\day2sample.txt"
    print "wow"

maxset :: MaxSet
maxset = Map.fromList [(Red, 12), (Green, 13), (Blue, 14)]

gameParser :: Parser Char
gameParser = string "Game" *> space1 *> digitChar <* char ':' <* space1

colourParser :: String-> Parser (String, Int)
colourParser s = do _ <- space
                    num <- digitChar
                    _ <- space1
                    str <- string s
                    return (str, digitToInt num) :: Parser (String, Int)


redParser, greenParser, blueParser :: Parser ColourPair
redParser = colourParser "red" >>= \(_, num) -> return (Red, num) <?> "Red"
greenParser = colourParser "green" >>= \(_, num) -> return (Green, num) <?> "Green"
blueParser = colourParser "blue" >>= \(_, num) -> return (Blue, num) <?> "Blue"


valueParser :: Parser ColourPair
valueParser = choice [try redParser, try greenParser, try blueParser]

singleSet :: Parser ColourPair
singleSet = try oneset <|> endset
            where oneset = do v <- valueParser
                              comma <- try $ lookAhead $ char ','
                              if comma == ','
                              then return v <* char ',' <* space
                              else return v
                  endset = valueParser <* eof


manySet :: Parser [ColourPair]
manySet = manyTill singleSet (char ';') <|> many singleSet

setParser :: Parser [ColourPair]
setParser = manySet

allsets :: Parser [ColourPair]
allsets = concat <$> many setParser

isPossibleToMax :: ColourPair -> Bool
isPossibleToMax cp = Just (snd cp) <= val
                    where val = Map.lookup (fst cp) maxset

checkSet :: [ColourPair] -> Bool
checkSet = all isPossibleToMax

countTrue :: [Bool] -> Integer
countTrue = foldr (\x acc -> if x then acc + 1 else acc) 0


ezTest = parseTest (gameParser *> manySet)

testLine, testInp :: String
testLine = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
testInp = "Game 1: 3 blue"