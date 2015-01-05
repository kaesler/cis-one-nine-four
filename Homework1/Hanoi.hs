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

-- 4 peg version
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 src dst _ _ = [(src, dst)]
hanoi4 2 src dst alt1 _ = [(src, alt1), (src, dst), (alt1, dst)]
-- hanoi4 3 src dst alt1 alt2 = [(src, alt2), (src, alt1), (src, dst), (alt1, dst), (alt2, dst)]
-- n > 2
-- so move all but bottom 2 to alt1
--    move bottom two to dst without using alt1
--      src alt2
--      src dst
--      alt2 dst
--    move all at alt1 over to dst
hanoi4 n src dst alt1 alt2 =
  (hanoi4 (n - 2) src alt1 dst alt2)
   ++ [(src, alt2), (src, dst), (alt1, dst)]
   ++ (hanoi4 (n - 2) alt1 dst alt2 src)
