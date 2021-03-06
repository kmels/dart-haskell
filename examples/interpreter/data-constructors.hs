module DataConstructors where

data Letter = Letter Char

a = Letter 'a' -- DONE


---------------------------------------- My List ----------------------------------------
data List a = Cons a (List a) | Nil deriving (Show,Eq)

nil = Nil -- DONE
singleList = Cons 'S' Nil -- DONE
singleLetter = Cons a Nil -- DONE
firstNumbers = Cons 1 (Cons 2 (Cons 3 Nil))  -- DONE
firstLetters = Cons 'a' (Cons 'b' (Cons 'c' Nil)) -- DONE

singleListAB = ConsA 'S' ABNil -- NOT DONE

ltruth1 = singleList == singleList
------------------------------- Haskell List + Tuples ----------------------------------------
type PersonName = Name
type Instrument = String
data MusicBand = MkBand Name [(PersonName,Instrument)]

-- DONE
guitar = "Guitar"
keyboard = "Keyboard"
bass = "Bass"
drums = "Percussion"

roger = ("Roger Waters", bass)  -- DONE
pinkFloyd = MkBand "Pink Floyd"  -- DONE, needs pretty printing
            [ roger
              , ("Nick Mason", drums)
              , ("David Gilmour", guitar)
              , ("Richard Wright", keyboard)]
pfMembers = ["Roger","David","Richard","Nick"]

----------------------------------- Two-type list ----------------------------------------
data ListAB a b = ConsA a (ListAB a b)
                | ConsB b (ListAB a b)
                | ABNil deriving (Show,Eq)
  
firstNumbersAB = ConsA 1 (ConsA 2 (ConsA 3 ABNil))
firstLettersAB = ConsB 'a' (ConsB 'b' (ConsB 'c' ABNil))
                
truth0 = (firstNumbersAB :: ListAB Int Int) == (firstNumbersAB :: ListAB Int Int)

toAB_AList :: List a -> ListAB a b
toAB_AList Nil = ABNil
toAB_AList (Cons x xs) = (ConsA x (toAB_AList xs))

toAB_BList :: List b -> ListAB a b
toAB_BList Nil = ABNil
toAB_BList (Cons y ys) = (ConsB y (toAB_BList ys))

firstNumbersAB' :: ListAB Int b
firstNumbersAB' = toAB_AList firstNumbers

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
leaf1 = Leaf 1 -- DONE
tree10 = Branch (Leaf 10) (Leaf 20) -- DONE
tree1 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3) -- DONE

reflect (Leaf 0) = Leaf 0 -- DONE
reflect (Leaf n) = Leaf n -- DONE
reflect (Branch t1 t2) = Branch (reflect t2) (reflect t1) -- DONE

data Color = VERMILLION | PUCE | LAVENDER -- DONE
puce = PUCE

data Triple a b c = Triple a b c -- DONE
triple = Triple 'c' 10 "String" -- DONE

emptyList = []
one = [1] -- DONE, needs pretty printing
onetwo = [1,2] -- DONE, needs pretty printing
onetwothree = [1,2,3] -- DONE, needs pretty printing

reflect1 = reflect leaf1 -- DONE
reflect2 = reflect tree10 -- DONE

-- Problem 1: Types as values in lambda abstractions (
-- Problem 2: Laziness is broken...
-- Problem 3: Load GHC's libraries that don't depend on primitives, to which grade?
