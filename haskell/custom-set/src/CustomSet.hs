module CustomSet
  ( delete,
    difference,
    empty,
    fromList,
    insert,
    intersection,
    isDisjointFrom,
    isSubsetOf,
    member,
    null,
    size,
    toList,
    union,
  )
where

import Prelude hiding (null)

newtype CustomSet a = Wrap [a]

instance Foldable CustomSet where
  foldr f acc (Wrap set) = foldr f acc set

instance (Eq a) => Eq (CustomSet a) where
  (==) (Wrap []) (Wrap []) = True
  (==) (Wrap []) _ = False
  (==) _ (Wrap []) = False
  (==) setA setB =
    let lenA = size setA
        lenB = size setB
        (setA', setB') = if lenA >= lenB then (setA, setB) else (setB, setA)
     in foldr (\x acc -> if member x setB' then acc && True else acc && False) True setA'

instance (Show a) => Show (CustomSet a) where
  show (Wrap set) = show set

delete :: (Eq a) => a -> CustomSet a -> CustomSet a
delete _ (Wrap []) = Wrap []
delete v (Wrap (x : rest)) = if x == v then Wrap rest else insert x $ delete v $ Wrap rest

difference :: (Eq a) => CustomSet a -> CustomSet a -> CustomSet a
difference = foldr delete

empty :: CustomSet a
empty = Wrap []

fromList :: (Eq a) => [a] -> CustomSet a
fromList = union empty . Wrap

insert :: (Eq a) => a -> CustomSet a -> CustomSet a
insert x (Wrap set) = if member x $ Wrap set then Wrap set else Wrap $ x : set

intersection :: (Eq a) => CustomSet a -> CustomSet a -> CustomSet a
intersection setA setB = foldl (\acc x -> if member x setB then insert x acc else acc) empty setA

isDisjointFrom :: (Eq a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = foldr (\x acc -> not (member x setB) && acc) True setA

isSubsetOf :: (Eq a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf setA setB = foldr (\x acc -> member x setB && acc) True setA

member :: (Eq a) => a -> CustomSet a -> Bool
member x = foldr (\y acc -> (x == y) || acc) False

null :: CustomSet a -> Bool
null (Wrap []) = True
null _ = False

size :: CustomSet a -> Int
size (Wrap []) = 0
size (Wrap (_ : rest)) = 1 + size (Wrap rest)

toList :: CustomSet a -> [a]
toList (Wrap set) = set

union :: (Eq a) => CustomSet a -> CustomSet a -> CustomSet a
union = foldr insert
