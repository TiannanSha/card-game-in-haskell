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
feedback as gs = (numEq as gs, 0, numEq (map getRank as) (map getRank gs),0,numEq (map getSuit as) (map getSuit gs))

-- this function takes an Answer (a list of cards) and a Guess (a list of cards) and returns the number of cards that are same in both lists
numEq :: Eq a => [a] -> [a] -> Int
numEq as gs = length as - length (as\\gs)  -- as\\gs delete one ocurrence of every element in gs

--numRankEq :: AnswerCards -> GuessCards -> Int
--numRankEq as gs = legnth as - length (deleteFirstsBy as gs)

getSuit :: Card -> Suit
getSuit (Card s _) = s

getRank :: Card -> Rank
getRank (Card _ r) = r

---------------------------------------------------------------

initialGuess :: Int -> (GuessCards, GameState)
initialGuess _ = ([Card Club R2, Card Heart Ace], 
	(GameState 2 [[Card Club R2, Card Heart Ace], [Card Club R3, Card Heart Ace]]))

nextGuess :: (GuessCards,GameState) -> Feedback -> (GuessCards,GameState)
nextGuess _ _ = ([Card Club R2, Card Heart Ace], 
	(GameState 2 [[Card Club R2, Card Heart Ace], [Card Club R3, Card Heart Ace]]))
