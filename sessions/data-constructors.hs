module DataConstructors where

data Letter = Letter Char

a = Letter 'a'

data List a = Cons a (List a) | Nil deriving Show

nil = Nil
singleList = Cons 'S' Nil
singleLetter = Cons a Nil

singleListAB = ConsA 'S' ABNil
firstNumbers = Cons 1 (Cons 2 (Cons 3 Nil))
firstLetters = Cons 'a' (Cons 'b' (Cons 'c' Nil))

type PersonName = Name
type Instrument = String

data MusicBand = MkBand Name [(PersonName,Instrument)]

guitar = "Guitar"
keyboard = "Keyboard"
bass = "Bass"
drums = "Percussion"

pinkFloyd = MkBand "Pink Floyd" 
            [ ("Roger Waters", bass)
              , ("Nick Mason", drums)
              , ("David Gilmour", guitar)
              , ("Richard Wright", keyboard)]

data ListAB a b = ConsA a (ListAB a b)
                | ConsB b (ListAB a b)
                | ABNil deriving (Show,Eq)
                
toAB_AList :: List a -> ListAB a b
toAB_AList Nil = ABNil
toAB_AList (Cons x xs) = let xs' = toAB_AList xs in (ConsA x xs')

toAB_BList :: List b -> ListAB a b
toAB_BList Nil = ABNil
toAB_BList (Cons y ys) = let ys' = toAB_BList ys in (ConsB y ys')

firstNumbersAB = ConsA 1 (ConsA 2 (ConsA 3 ABNil))
firstLettersAB = ConsB 'a' (ConsB 'b' (ConsB 'c' ABNil))

concatenateAB :: ListAB a b -> ListAB a b -> ListAB a b
ABNil `concatenateAB` ys = ys
concatenateAB (ConsA x xs) ys = let xsys = xs `concatenateAB` ys in (ConsA x xsys)

truth1 = (toAB_AList firstNumbers :: ListAB Int Int) == (firstNumbersAB :: ListAB Int Int)
truth2 = (toAB_BList firstLetters :: ListAB Char Char) == (firstLettersAB :: ListAB Char Char)

-- Books

type Author = PersonName
type Name = String
type ISBN = Integer

data Book = MkBook Name Author ISBN

myFirstBook = MkBook "The biography of José Mourinho" "Some author" 2040240

-- | SPJ's book
data Tree = Leaf Int | Branch Tree Tree
tree1 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)
reflect (Leaf n) = Leaf n
reflect (Branch t1 t2) = Branch (reflect t2) (reflect t1)

data Color = VERMILLION | PUCE | LAVENDER
puce = PUCE
