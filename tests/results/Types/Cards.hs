module Types.Cards where


deck :: [Card]
deck = [(s, v) | s <- suits, v <- values]

type Card = (Suit, CardValue)

data Suit = Club | Diamond | Heart | Spade
     deriving (Show, Eq, Ord)

suits :: [Suit]
suits = [Club, Diamond, Heart, Spade]
 
data CardValue = Two | Three | Four
     | Five | Six | Seven | Eight | Nine | Ten 
     | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord)

values :: [CardValue]
values = [Ace, Two, Three, Four, Five, Six, Seven, Eight,
          Nine, Ten, Jack, Queen, King]
