--  File     : Proj1.hs
--  Author   : Kyle Zsembery
--  Purpose  : Implementation of a guessing card game.

-- DESCRIPTION OF GAME AND HOW THIS PROGRAM PLAYS IT HERE

module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List

-- ========= METHODS FOR ANSWERER ========= --
type FeedbackScore = (Int, Int, Int, Int, Int)

feedback :: [Card] -> [Card] -> FeedbackScore
feedback target guess = (matches, lowerRank, sameRank, higherRank, sameSuit)
    where matches = length $ intersect guess target
          lowestRankGuess = minimum (map rank guess)
          lowerRank = length [ rank | (Card suit rank) <- target, rank < lowestRankGuess ]
          sameRank = sameF rank target guess
          highestRankGuess = maximum (map rank guess)
          higherRank = length [ rank | (Card suit rank) <- target, rank > highestRankGuess ]
          sameSuit = sameF suit target guess

sameF :: Eq a => (b -> a) -> [b] -> [b] -> Int
sameF f target guess = min (length firstIntersect) (length secondIntersect)
    where firstIntersect = intersect (map f guess) (map f target)
          secondIntersect = intersect (map f target) (map f guess)

-- ========= END METHODS FOR ANSWERER ========= --

-- ========= METHODS FOR GUESSER ========= --

-- Holds all remaining possible guesses
-- data GameState = [[Card]]
--     deriving (Show)

type GameState = [[Card]]

-- Takes number of cards in answer as input. Returns a starting guess and the GameState
-- This guess aims to reduce the space of sets possible when the return answer is given.
initialGuess :: Int -> ([Card], GameState)
initialGuess n 
    | n == 2 = ([Card Club R5, Card Diamond R10], state)
    | n == 3 = ([Card Club R5, Card Diamond R8, Card Heart Jack], state)
    | n == 4 = ([Card Club R4, Card Diamond R7, Card Heart R10, Card Spade Queen], state)
    | otherwise = error "n must be 2, 3 or 4"
    where
        cards = [Card s r| s <- [Club ..], r <- [R2 ..]]
        state = subsets n cards


-- Takes the previous guess and game state (as a pair) and the feedback from the last guess
-- Returns another guess as List of cards and game state
nextGuess :: ([Card], GameState) -> FeedbackScore -> ([Card], GameState)
nextGuess prevGuessWithState prevFeedback
        = (newGuess, newState)
        where newState = eliminateGuesses prevGuessWithState prevFeedback
              newGuess = newState !! ((length newState - 1) `div` 2)

-- Removes any guesses that can not possibly be in the solution
-- Input: (Previous guess, Remaining possible guesses), feedback
-- Output: Updated possible guesses
-- Make the previous guess the target and then only take those cards with same score
eliminateGuesses :: ([Card], GameState) -> FeedbackScore -> GameState
eliminateGuesses (prev_guess, state) prev_feedback
    = [guess | guess <- state, checkGuess prev_guess prev_feedback guess]



-- Takes a previous guess, a feedback score for that guess, and a new guess, returning a bool
-- Helper function for eliminateGuesses
-- Returns true if the tuple is equal in fields 1, 2, 4 and >= on 3 and 5 (unless 0)

-- NB: THERE IS A WAY BETTER WAY OF DESTRUCTURING PREV_FEEDBACK i.e. pattern matching
checkGuess :: [Card] -> FeedbackScore -> [Card] -> Bool
checkGuess prev_guess prev_feedback guess = matches && lower && sameRank && higher && sameSuit
    where 
        this_feedback = feedback guess prev_guess
        matches = frst prev_feedback == frst this_feedback
        lower = scnd prev_feedback == scnd this_feedback
        this_feedback_same_rank = thrd this_feedback
        sameRank = if thrd prev_feedback == 0
                   then this_feedback_same_rank == 0
                   else this_feedback_same_rank >= thrd prev_feedback
        higher = frth prev_feedback == frth this_feedback
        this_feedback_same_suit = ffth this_feedback
        sameSuit = if ffth prev_feedback == 0
                   then this_feedback_same_suit == 0
                   else this_feedback_same_suit >= ffth prev_feedback


-- Tuple accessor functions
frst :: (a, b, c, d, e) -> a
frst (a, b, c, d, e) = a

scnd :: (a, b, c, d, e) -> b
scnd (a, b, c, d, e) = b

thrd :: (a, b, c, d, e) -> c
thrd (a, b, c, d, e) = c

frth :: (a, b, c, d, e) -> d
frth (a, b, c, d, e) = d

ffth :: (a, b, c, d, e) -> e
ffth (a, b, c, d, e) = e


-- Make testing faster
initGuess = initialGuess 2
ans = [Card Club R3, Card Heart R4]
prevFeedback = feedback ans $ fst initGuess


-- Below was taken from: https://stackoverflow.com/a/52602906
-- Every set contains a unique empty subset.
subsets 0 _ = [[]]

-- Empty sets don't have any (non-empty) subsets.
subsets _ [] = []

-- Otherwise we're dealing with non-empty subsets of a non-empty set.
-- If the first element of the set is x, we can get subsets of size n by either:
--   - getting subsets of size n-1 of the remaining set xs and adding x to each of them
--     (those are all subsets containing x), or
--   - getting subsets of size n of the remaining set xs
--     (those are all subsets not containing x)
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs
