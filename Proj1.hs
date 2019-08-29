--  File     : Proj1.hs
--  Author   : Kyle Zsembery
--  Purpose  : Implementation of a guessing card game.

-- DESCRIPTION OF GAME AND HOW THIS PROGRAM PLAYS IT HERE

-- !!!! THIS WAS A 14 GUESS TEST, WHY, WTF FUCKS UP?
-- ./Proj1Test "2C" "2D" "7H" "AS"

module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List
import Data.Ord

-- NEXT: RUN OPTIMAL GUESS DEGUG FOR THE TESTING ANSWERS AND SEE HOW LONG IT TAKES/IF IT GETS STUCK

-- ========= METHODS FOR ANSWERER ========= --
type FeedbackScore = (Int, Int, Int, Int, Int)

feedback :: [Card] -> [Card] -> FeedbackScore
feedback target guess = (matches, lowerRank, sameRank, higherRank, sameSuit)
    where matches = length $ intersect guess target
          lowestRankGuess = minimum (map rank guess)
          lowerRank = length [ rank | (Card suit rank) <- target, rank < lowestRankGuess ]
          sameRank = sameF (map rank target) (map rank guess)
          highestRankGuess = maximum (map rank guess)
          higherRank = length [ rank | (Card suit rank) <- target, rank > highestRankGuess ]
          sameSuit = sameF (map suit target) (map suit guess)

-- How many cards of the same F function (which strips out a value)
-- Can we use id function for checking exact matches???
-- sameF :: Eq a => (b -> a) -> [b] -> [b] -> Int
sameF _ [] = 0
sameF [] _ = 0
sameF target (c:cards)
    | elem c target = 
        1 + sameF (delete c target) cards
    | otherwise = sameF target cards


    -- where firstIntersect = intersect (map f guess) (map f target)
    --       secondIntersect = intersect (map f target) (map f guess)

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
    | n == 3 = ([Card Club R5, Card Diamond R8, Card Diamond Jack], state)
    | n == 4 = ([Card Club R4, Card Diamond R7, Card Club R10, Card Diamond Queen], state)
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
              newGuess = optimalGuess newState newState
            --   newGuess = newState !! ((length newState - 1) `div` 2)


-- We know guess is somewhere in the range of GameState, return the best one
-- By averaging the number of potential spaces after that guess is made
optimalGuess :: GameState -> GameState -> [Card]
optimalGuess guesses answers = snd (minimumBy (comparing fst) [ guessAverageSpace g answers | g <- guesses ])


optimalGuessDebug :: GameState -> GameState -> [(Int, [Card])]
optimalGuessDebug guesses answers = [ guessAverageSpace g answers | g <- guesses ]

-- FIX TYPE SIGNATURE HERE, THE / IS NOT WORKING
-- What's the average size of the possible guessSpaces, if I take this guess

-- The feedback is any possible for that guess, and when unwrapped, eliminates based on that feedback for each possible
-- answer against that single guess
-- Output: (Avg space of result, guess)
guessAverageSpace guess answers = ((sum [(length (eliminateGuesses (guess, answers) (a_feedback))) | a_feedback <- feedbacks]) `div` (length feedbacks), guess)
    where feedbacks = getAllFeedbacks guess answers
    

-- Gets all the possible feedbacks for each posssible guess
-- i.e. a guess will have feedbacks only relevant to the potential answers, which changes (game state is reduced on
-- each iteration)
-- Doesn't actually matter what the answer is, since it's only the feedback that actually narrows
-- the search space. Hence, no need to track answer and thus can also remove duplicates
getAllFeedbacks :: [Card] -> GameState -> [FeedbackScore]
getAllFeedbacks guess answers = nub [feedback ans guess | ans <- answers ]

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
n = 4
ans = [Card Club R2, Card Diamond R2, Card Heart R7, Card Spade Ace]
initGuess = initialGuess n
fstFeedback = feedback ans (fst initGuess)
fstState = eliminateGuesses initGuess fstFeedback
fstInFstState = [Card Club R2, Card Club R3, Card Club R7, Card Club King]
allPossibleFeedbacksForFstInFstState = getAllFeedbacks fstInFstState fstState
fstPossibleFeedback = allPossibleFeedbacksForFstInFstState !! 0
avgSpaceIfGuessFstInFstState = guessAverageSpace fstInFstState fstState



-- The run forerver begins here
-- sndGuess = optimalGuess fstState fstState
-- 


-- guess1 = [Card Club R3, Card Club King, Card Diamond R5, Card Diamond R7]
-- guess2 = [Card Club R2, Card Club R4, Card Club Ace, Card Diamond R4]

-- fstFeedback = feedback ans $ fst initGuess
cards = [Card s r| s <- [Club ..], r <- [R2 ..]]
state = subsets n cards


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
