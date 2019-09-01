--  File     : Card.hs
--  Author   : Tiannan Sha
--  Purpose  : An implementation of answerer and guesser functions

-- | This code ... blah blah

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List

-----------------------type synonyms--------------------------- 

type AnswerCards = [Card]
type GuessCards = [Card]
type Feedback = (Int,Int,Int,Int,Int)	

-- GameState stores number of cards in the answer and all the valid guesses
data GameState = GameState Int [Feedback] [GuessCards] [GuessCards] 

---------------------------------------------------------------

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

-- this function takes two lists and return number of shared elements in a 
-- one-to-one fashion. that is, numEq [1,1,1] [1,1] = 2
numEq :: Eq a => [a] -> [a] -> Int
numEq as gs = length as - length (as\\gs)  -- as\\gs delete one ocurrence of every element in gs

getSuit :: Card -> Suit
getSuit (Card s _) = s

getRank :: Card -> Rank
getRank (Card _ r) = r

---------------------------------------------------------------

--inital guess' ranks should evenly divide the rank space
initialGuess :: Int -> (GuessCards, GameState)
initialGuess 2 
	= (
		[Card Club R6, Card Heart Jack], 
		GameState 2 [] [] (enumAll 2)
	  )

-- first add the latest Feedback to the game state
-- for all the possible guesses in the old game state,
-- check its consistency against all the past [GuessCards] and [Feedback]
-- add a guess to new gameState iff it's consistent
nextGuess :: (GuessCards,GameState) -> Feedback -> (GuessCards,GameState)
nextGuess (gs, GameState num fs pastGss posGss) f 
	= let 
		newFs = f:fs
		newPastGss = gs:pastGss
		newPosGss = [posGs | posGs<-posGss, isConsistent posGs newFs newPastGss]
	  in 
	  	(head newPosGss, GameState num newFs newPastGss newPosGss)

isConsistent :: GuessCards->[Feedback]->[GuessCards]->Bool
isConsistent posGs fs pastGss = 
	and [feedback posGs pastGs == f | (f,pastGs)<-zip fs pastGss]

--_isConsistent :: GuessCards->GuessCards->Feedback->Bool
--_isConsistent oldGs posGs f = f==feedback posGs oldGs

-- this function takes the number of cards in one guess 
-- returns all the possible guesses as list of guesses
-- use c1 to pick the "smallest" card, c2 to pick the second smallest card and
-- c3 to pick the third smallest card and so on
-- test correctness: length (enumAll 4) = 270725
enumAll :: Int -> [GuessCards]
enumAll x
	| x==2 = [[c1,c2] | c1<-[(Card Club R2)..(Card Spade King)], 
						c2<-[succ(c1)..(Card Spade Ace)]
			 ]
	| x==3 = [[c1,c2,c3] | c1<-[(Card Club R2)..(Card Spade Queen)],
						   c2<-[succ(c1)..(Card Spade King)],
						   c3<-[succ(c2)..(Card Spade Ace)]
			 ]
	| x==4 = [[c1,c2,c3,c4] | c1<-[(Card Club R2)..(Card Spade Jack)],
						   c2<-[succ(c1)..(Card Spade Queen)],
						   c3<-[succ(c2)..(Card Spade King)],
						   c4<-[succ(c3)..(Card Spade Ace)]
			 ]
