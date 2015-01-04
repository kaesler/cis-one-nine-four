module Hanoi where

type Peg = String
type Move = (Peg, Peg)

-- Given the number of discs and names for the three pegs, hanoi
-- should return a list of moves to be performed to move the stack of
-- discs from the first peg to the second.

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 src dst _ = [(src, dst)]
hanoi n src dst other = (hanoi (n - 1) src other dst) ++ [(src, dst)] ++ (hanoi (n - 1) other dst src)

