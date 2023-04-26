module Uebung01_05 where 
    
{-
#######################
5
Blackjack
#######################
-}

-- 1
-- Create Suits, Rank and Card values
-- deriving creates the called typeclasses (e.q. Eq, Show) automatically
-- show with myCard = Card Hearts Five
data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Show, Enum, Eq)



data Rank = Two | Three | Four | Five | Six | Seven 
                | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Enum, Eq)

data Card = Card Rank Suit
    deriving(Eq)
    

instance Show Card where
  show (Card value rank) = show value ++ " of " ++ show rank

-- 2
-- get Value of Card in Blackjack (Ace counts as 11)
getCardValue :: Card -> Int
getCardValue (Card rank _) = case rank of
    Ace     -> 11
    _       -> min 10 (fromEnum rank + 2)
    

-- 3
-- define Hand
-- Hand can be empty or combination of Card + remaining cards
-- implement function (<+>) to combine two hands
-- has to use newtype because new definition from Card is created
newtype Hand = Hand [Card]
  deriving (Show, Eq)
    

(<+>) :: Hand -> Hand -> Hand
(<+>) (Hand cards1) (Hand cards2) = Hand (cards1 ++ cards2)  


-- 4
-- fullDeck function
fullDeck :: Hand
fullDeck = Hand [Card rank suit |rank <- [Two .. Ace], suit <- [Hearts .. Spades]]

-- Get number of Aces from Hand
numOfAces :: Hand -> Int
numOfAces (Hand cards) = length [Card rank suit | (Card rank suit) <- cards, rank == Ace ]

-- value of Cards in Hand
-- Write Function to getValue of Cards in Hand 
getValue :: Hand -> Int
getValue (Hand cards) =
  if sum[getCardValue card |card <- cards] <= 21 then
    sum[getCardValue card |card <- cards]
    else
      minmax (sum[getCardValue card |card <- cards]) (numOfAces (Hand cards))


minmax :: Int -> Int -> Int
minmax val ace |  val<=21 || ace == 0 = val
minmax val ace                        = minmax (val - 10) (ace - 1)


-- test
{-
c1 = Card Ace Hearts
c2 = Card Ace Spades
c3 = Card Two Diamonds
hand = Hand [c1,c2,c3]
λ: 14
-}

