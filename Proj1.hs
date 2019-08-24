--  File     : Card.hs
--  Author   : Tiannan Sha
--  Purpose  : An implementation of answerer and guesser functions

-- | This code ... blah blah

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List

-------type synonyms------- 

type answer = [Card]
type guess = [Card]
type feedback = (Int,Int,Int,Int,Int)	
-- GameState stores number of cards in the answer and all the valid guesses
data GameState = GameState Int [guess] 



feedback :: answer -> guess -> feedback
feedback _ _ = (0,0,0,0,0)

initialGuess :: Int -> (guess, GameState)
initialGuess _ = ([Card Club R2, Card Heart Ace], 
	(2,[[Card Club R2, Card Heart Ace], [Card Club R3, Card Heart Ace]]))

nextGuess :: (guess,GameState) → feedback → (guess,GameState)
nextGuess _ _ = ([Card Club R2, Card Heart Ace], 
	(2,[[Card Club R2, Card Heart Ace], [Card Club R3, Card Heart Ace]]))

