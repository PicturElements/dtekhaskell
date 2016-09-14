module BlackJack where

import Cards
import Wrapper
import Test.QuickCheck --hiding (shuffle)

--size hand2
--    = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)
--    = 1 + size (Add (Card Jack spades) Empty)
--    = 1 + 1 + size (Empty)
--    = 1 + 1 + 0 = 2

aCard1 :: Card
aCard1 = Card King Hearts

aCard2 :: Card
aCard2 = Card (Numeric 9) Diamonds

aHand :: Hand
aHand = Add (aCard1)
          (Add aCard2 Empty)

aHand2 = Add (Card Ace Diamonds)
          (Add (Card Ace Clubs) Empty)

aHand3 = Add (Card King Hearts)
          (Add (Card Ace Clubs)
           (Add (Card King Spades) Empty))

--prop_valueRank :: Rank -> Bool
--prop_valueRank a = valueRank (Numeric a) == a || valueRank King == 10 ||
--                   valueRank Ace == 11

valueRank :: Rank -> Integer
valueRank (Numeric n)                = n
valueRank Ace                        = 11
valueRank _                          = 10

--prop_valueCard (Card a s) = valueCard (Card (Numeric a) Clubs) == a

valueCard :: Card -> Integer
valueCard (Card r s)         = valueRank r

numberOfAces :: Hand -> Integer
numberOfAces Empty                      = 0
numberOfAces (Add (Card Ace suit) hand) = 1 + numberOfAces hand
numberOfAces (Add card hand)            = 0 + numberOfAces hand


--Calculates value of a handp1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3
value :: Hand -> Integer
value Empty = 0
value (Add card hand) | ((valueCard (card) + value hand) > 21)
			= valueCard (card)
                        - (10 * numberOfAces (Add card hand)) + value hand
value (Add card hand) | otherwise = valueCard (card) + value hand


gameOver :: Hand -> Bool
gameOver (Add card hand) = value (Add card hand) > 21

winner :: Hand -> Hand -> Player
winner (Add card1 hand1) (Add card2 hand2)
    | gameOver (Add card1 hand1) = Bank
    | (value (Add card1 hand1) < value (Add card2 hand2)) &&
      (gameOver (Add card2 hand2) == False) = Bank
    | otherwise = Guest

--Task C
(<+) :: Hand -> Hand -> Hand
(<+) Empty h2 = h2
(<+) h1 Empty = h1
(<+) (Add c h) h2 = Add c (h <+ h2)
 
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = ((size h1) + (size h2)) == size (h1 <+ h2)

--Task D
fullDeck :: Hand
fullDeck = (allCardsInSuit Clubs <+
            allCardsInSuit Diamonds <+
            allCardsInSuit Spades <+
            allCardsInSuit Hearts)

allCardsInSuit :: Suit -> Hand
allCardsInSuit s = (Add (Card Ace s)
                   (Add (Card (Numeric 2) s)
                   (Add (Card (Numeric 3) s)
                   (Add (Card (Numeric 4) s)
                   (Add (Card (Numeric 5) s)
                   (Add (Card (Numeric 6) s)
                   (Add (Card (Numeric 7) s)
                   (Add (Card (Numeric 8) s)
                   (Add (Card (Numeric 9) s)
                   (Add (Card (Numeric 10) s)
                   (Add (Card Jack s)
                   (Add (Card Queen s)
                   (Add (Card King s) Empty)))))))))))))

draw :: Hand -> Hand -> Hand
draw h deck | 
