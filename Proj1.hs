--  File     : Card.hs
--  Author   : Tiannan Sha
--  Purpose  : An implementation of answerer and guesser functions

-- | This code ... blah blah

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List

-------type synonyms------- 

type Answer = [Card]
type Guess = [Card]
type Feedback = (Int,Int,Int,Int,Int)	
-- GameState stores number of cards in the answer and all the valid guesses
data GameState = GameState Int [Guess] 



feedback :: Answer -> Guess -> Feedback
feedback _ _ = (0,0,0,0,0)

initialGuess :: Int -> (Guess, GameState)
initialGuess _ = ([Card Club R2, Card Heart Ace], 
	(GameState 2 [[Card Club R2, Card Heart Ace], [Card Club R3, Card Heart Ace]]))

nextGuess :: (Guess,GameState) -> Feedback -> (Guess,GameState)
nextGuess _ _ = ([Card Club R2, Card Heart Ace], 
	(GameState 2 [[Card Club R2, Card Heart Ace], [Card Club R3, Card Heart Ace]]))
