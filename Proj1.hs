--  File     : Card.hs
--  Author   : Tiannan Sha
--  Purpose  : An implementation of answerer and guesser functions

-- | This code ... blah blah

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List

----------------------type synonyms------------------------ 

type AnswerCards = [Card]
type GuessCards = [Card]
type Feedback = (Int,Int,Int,Int,Int)	
-- GameState stores number of cards in the answer and all the valid guesses
data GameState = GameState Int [GuessCards] 


-- returns (numEqCard, numLsRank, numEqRank, numGtRank, numEqSuit)
feedback :: AnswerCards -> GuessCards -> Feedback
feedback as gs = 
	(numEq as gs, 
	length (filter (< minimum gRanks) aRanks), 
	numEq aRanks gRanks,
	length (filter (> maximum gRanks) aRanks),
	numEq aSuits gSuits)
	where 
		aRanks = map getRank as
		gRanks = map getRank gs
		aSuits = map getSuit as
		gSuits = map getSuit gs

-- this function takes an Answer (a list of cards) and a Guess (a list of cards) and returns the number of cards that are same in both lists
numEq :: Eq a => [a] -> [a] -> Int
numEq as gs = length as - length (as\\gs)  -- as\\gs delete one ocurrence of every element in gs

getSuit :: Card -> Suit
getSuit (Card s _) = s

getRank :: Card -> Rank
getRank (Card _ r) = r

---------------------------------------------------------------

--inital guess' ranks should evenly divid the rank space
initialGuess :: Int -> (GuessCards, GameState)
initialGuess 2 = ([Card Club R6, Card Heart Jack], 
	(GameState 2 [[Card Club R6, Card Heart Jack], [Card Club R6, Card Heart Jack]]))

nextGuess :: (GuessCards,GameState) -> Feedback -> (GuessCards,GameState)
nextGuess _ _ = ([Card Club R6, Card Heart Jack], 
	(GameState 2 [[Card Club R6, Card Heart Jack], [Card Club R6, Card Heart Jack]]))
