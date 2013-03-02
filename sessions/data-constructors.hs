module DataConstructors where

data Letter = Letter Char

a = Letter 'a' -- DONE

data List a = Cons a (List a) | Nil deriving Show

nil = Nil -- DONE
singleList = Cons 'S' Nil -- DONE
singleLetter = Cons a Nil -- DONE

singleListAB = ConsA 'S' ABNil -- NOT DONE
firstNumbers = Cons 1 (Cons 2 (Cons 3 Nil))  -- DONE
firstLetters = Cons 'a' (Cons 'b' (Cons 'c' Nil)) -- DONE

type PersonName = Name
type Instrument = String
data MusicBand = MkBand Name [(PersonName,Instrument)]

guitar = "Guitar"
keyboard = "Keyboard"
bass = "Bass"
drums = "Percussion"

roger = ("Roger Waters", bass)
pinkFloyd = MkBand "Pink Floyd" 
            [ roger
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

myFirstBook = MkBook "The biography of José Mourinho" "Some author" 2040240 -- DONE

-- | SPJ's book
data Tree = Leaf Int | Branch Tree Tree
leaf1 = Leaf 1
tree10 = Branch (Leaf 10) (Leaf 20)
tree1 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)

reflect (Leaf 0) = Leaf 0
reflect (Leaf n) = Leaf n
reflect (Branch t1 t2) = Branch (reflect t2) (reflect t1)

data Color = VERMILLION | PUCE | LAVENDER
puce = PUCE

data Triple a b c = Triple a b c
triple = Triple 'c' 10 "String"

emptyList = []
one = [1]
onetwo = [1,2]
onetwothree = [1,2,3]

reflect1 = reflect leaf1
