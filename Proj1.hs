--  File     : Proj1.hs
--  Author   : Kyle Zsembery <k.zsembery@student.unimelb.edu.au>
--  Purpose  : Implementation of a guessing card game.

-- ============= Game Description ===============
-- A guesser (G) and an answerer A both have a standard 52 card deck of playing
-- cards
-- A chooses n cards (where n is 2, 3 or 4) from the deck
-- G must guess the cards A holds by showing A a guess of n cards from their
-- deck
-- A provides feedback as described by the feedback function to G
-- G then guesses again. This program aims to provide a (near) optimal algorithm
-- for G, in that it will take G the fewest guesses on average to guess solution

-- This program implements the answerer as "feedback" and the guesser as 
-- initGuess and nextGuess (along with helper functions)
-- ==============================================

module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List
import Data.Ord

-- ========= METHODS FOR ANSWERER ========= --

-- Stores feedback in a quintuple with element meanings as follows:
-- 1. Number of exact card matches
-- 2. Number of cards in the answer that are lower than the lowest card 
-- in the guess
-- 3. Number of cards in the guess and answer that match for rank
-- 4. Number of cards in the answer that are higher than the highest card in 
-- the guess
-- 5. Number of cards in the answer and guess that are of the same suit
type FeedbackScore = (Int, Int, Int, Int, Int)

-- Calculates the feedback score, by the rules above
-- Input: An n-card guess, an n-card answer
-- Output: A feedback score, as quintuple described above
feedback :: [Card] -> [Card] -> FeedbackScore
feedback target guess = (matches, lowerRank, sameRank, higherRank, sameSuit)
    where matches = matchLsts guess target
          lowestRankGuess = minimum (map rank guess)
          lowerRank = length [ rank | (Card suit rank) <- target, 
                               rank < lowestRankGuess ]
          sameRank = matchLsts (map rank target) (map rank guess)
          highestRankGuess = maximum (map rank guess)
          higherRank = length [ rank | (Card suit rank) <- target, 
                                rank > highestRankGuess ]
          sameSuit = matchLsts (map suit target) (map suit guess)

-- Count the number of elements that are the same in the lists, no replacement
-- Input: 2 lists
-- Output: An int of the count of the number of common items (no replacement)
matchLsts :: Eq b => [b] -> [b] -> Int
matchLsts _ [] = 0
matchLsts [] _ = 0
matchLsts target (c:cards)
    | elem c target = 
        1 + matchLsts (delete c target) cards
    | otherwise = matchLsts target cards

-- ========= METHODS FOR GUESSER ========= --

-- Contains a list of the remaining possible answers (and therefore guesses) 
-- that can be made
-- The aim of this program is to reduce this list as quickly as possible based 
-- on feedback
type GameState = [[Card]]

-- The program runs for too long (>10s for a solution) if the optimal guess
-- algorithm is run without this flag
MAX_STATE_LEN_FOR_OPTIMAL_GUESS = 500

-- Takes number of cards in answer as input. Returns a starting guess and the
-- GameState. This guess aims to reduce the space of sets possible when the 
-- return answer is given.
-- Initial guess is hardcoded to save computation time
-- Input: n (size of answer)
-- Output: (initial guess, all possible answers)
initialGuess :: Int -> ([Card], GameState)
initialGuess n
    | n == 2 = ([Card Club R5, Card Diamond R10], state)
    | n == 3 = ([Card Club R5, Card Diamond R8, Card Diamond Jack], state)
    | n == 4 = ([Card Club R4, Card Diamond R7, Card Club R10, 
                 Card Diamond Queen], state)
    | otherwise = error "n must be 2, 3 or 4"
    where
        cards = [Card s r| s <- [Club ..], r <- [R2 ..]]
        state = subsets n cards


-- Takes the previous guess and game state (as a pair) and the feedback from 
-- the last guess
-- Returns another guess as List of cards and game state
-- Takes middle of state if the statelength > 500 to save computation time,
-- otherwise choose optimal guess
-- Input: (previous guess, current state), feedback on previous guess
-- Output: (new guess, updated (reduced) state)
nextGuess :: ([Card], GameState) -> FeedbackScore -> ([Card], GameState)
nextGuess prevGuessWithState prevFeedback
        = (newGuess, newState)
        where newState = eliminateGuesses prevGuessWithState prevFeedback
              nSLen = length newState
              newGuess = if nSLen > MAX_STATE_LEN_FOR_OPTIMAL_GUESS
                         then (sort newState) !! (nSLen `div` 2)
                         else optimalGuess newState newState

-- We know guess is somewhere in the range of GameState, return the optimal one
-- By averaging the number of potential spaces, given the feedback, after that 
-- guess is made
-- Input: current state, current state
-- Output: the optimal guess
optimalGuess :: GameState -> GameState -> [Card]
optimalGuess guesses answers = snd (minimumBy (comparing fst) 
              [ guessAverageSpace g answers | g <- guesses ])

-- The feedback is any possible for that guess, and when unwrapped, eliminates
-- based on that feedback for each possible
-- answer against that single guess
-- Input: a single guess, current state (i.e. possible answers)
-- Output: (Avg space of result, guess)
guessAverageSpace :: [Card] -> GameState -> (Int, [Card])
guessAverageSpace guess answers = ((sum [
      (length (eliminateGuesses (guess, answers) (aFeedback))) |
       aFeedback <- feedbacks]) `div` (length feedbacks),
       guess)
    where feedbacks = getAllFeedbacks guess answers

-- Gets all the possible feedbacks for each posssible guess
-- i.e. a guess will have feedbacks only relevant to the potential answers,
-- which changes (game state is reduced on each iteration)
-- Doesn't actually matter what the answer is, since it's only the feedback that
-- actually narrows the search space. Hence, no need to track answer and thus
-- we can also remove duplicates
-- Input: A guess, A list of remaining possible answers
-- Output: List of possible feedback quintuples, based on possible answers being
-- compared to the guess under analysis
getAllFeedbacks :: [Card] -> GameState -> [FeedbackScore]
getAllFeedbacks guess answers = nub [feedback ans guess | ans <- answers ]

-- Removes any guesses that can not possibly be in the solution
-- Input: (Previous guess, Remaining possible guesses), feedback
-- Output: Updated possible guesses
-- Make the previous guess the target and then only take those cards with same
-- score
eliminateGuesses :: ([Card], GameState) -> FeedbackScore -> GameState
eliminateGuesses (prevGuess, state) prevFeedback
    = [guess | guess <- state, checkGuess prevGuess prevFeedback guess]

-- Takes a previous guess, a feedback score for that guess, and a new guess, 
-- returning a bool
-- Helper function for eliminateGuesses
-- Returns True if the tuple is equal in fields 1, 2, 4 and >= on 3 and 5
-- (unless 0)
-- The >= is required for 3 and 5 because, by example:
-- Guess: ["AC", "KC", "AD", "2D"], Answer: ["2C", "4C", "5C", "6C"] ->
-- Feedback for same suit = 2 (5th element in the quintuple)
-- We must still keep all guesses containing 2, 3 and 4 Clubs (or Diamonds)
-- But we know that anything less than 2 clubs or 2 diamonds can be removed
-- Input: The previous guess, a feedback score, a new guess
-- Output: Boolean, as to whether the new guess produces feedback that aligns
-- with what is possible given the original feedback
checkGuess :: [Card] -> FeedbackScore -> [Card] -> Bool
checkGuess prevGuess prevFeedback guess = matches && lower && sameRank && 
    higher && sameSuit
    where 
        thisFeedback = feedback guess prevGuess
        matches = frst prevFeedback == frst thisFeedback
        lower = scnd prevFeedback == scnd thisFeedback
        thisFeedbackSameRank = thrd thisFeedback
        sameRank = if thrd prevFeedback == 0
                   then thisFeedbackSameRank == 0
                   else thisFeedbackSameRank >= thrd prevFeedback
        higher = frth prevFeedback == frth thisFeedback
        thisFeedbackSameSuit = ffth thisFeedback
        sameSuit = if ffth prevFeedback == 0
                   then thisFeedbackSameSuit == 0
                   else thisFeedbackSameSuit >= ffth prevFeedback

-- =============== Quintuple Accessor Functions ===============
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

-- Generate all possible subsets of size n
-- Input: n (size of subsets), list of items to take subset of
-- Output: List of subests of the list of items of size n
-- Below was taken from: https://stackoverflow.com/a/52602906
subsets :: Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs