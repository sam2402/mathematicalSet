-- Implemented as a binary tree
data Set a = Empty | Node a (Set a) (Set a)
  deriving (Show)

-- Returns an ordered list given a Set
-- {2,1,4,3} => [1,2,3,4]
toList :: Ord a => Set a -> [a]
toList Empty = []
toList (Node a t1 t2) = toList t1 ++ [a] ++ toList t2

-- Converts a list to a Set while removing duplicates
--[1, 2, 3, 4, 4] => {1,2,3,4}
fromList :: Ord a => [a] -> Set a
fromList [] = Empty
fromList xs = foldr insert Empty xs

-- Tests if two sets have the same elements (pointwise equivalent).
instance (Ord a) => Eq (Set a) where
  Empty == Empty = True
  Empty == Node {} = False
  Node {} == Empty = False
  (Node a t1 t2) == (Node b t3 t4) = toList (Node a t1 t2) == toList (Node b t3 t4)

-- Returns the empty set
empty :: Set a
empty = Empty

-- Tests if the set is empty
isNull :: Set a -> Bool
isNull Empty = True
isNull Node {} = False

-- Builds a one element Set
singleton :: a -> Set a
singleton x = Node x Empty Empty

-- Inserts an element *x* of type *a* into a Set while insuring that there are no duplicates
insert :: (Ord a) => a -> Set a -> Set a
insert x Empty = Node x Empty Empty
insert x (Node a t1 t2)
  | x < a = Node a (insert x t1) t2
  | x > a = Node a t1 (insert x t2)
  | otherwise = Node a t1 t2

-- Joins two Sets while insuring that there are no duplicates.
union :: (Ord a) => Set a -> Set a -> Set a
union Empty Empty = Empty
union (Node a t1 t2) Empty = Node a t1 t2
union Empty (Node a t1 t2) = Node a t1 t2
union (Node a t1 t2) (Node b t3 t4) = union (insert b (Node a t1 t2)) (t3 `union` t4)

-- Returns, as a Set, the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection Empty Empty = Empty
intersection Empty Node {} = Empty
intersection Node {} Empty = Empty
intersection (Node a t1 t2) (Node b t3 t4) =
  if member a (Node b t3 t4)
    then singleton a `union` intersection t1 (Node b t3 t4) `union` intersection t2 (Node b t3 t4)
    else intersection t1 (Node b t3 t4) `union` intersection t2 (Node b t3 t4)

-- Returns all the elements in *s1* not in *s2*
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
difference Empty Empty = Empty
difference Empty Node {} = Empty
difference (Node a t1 t2) Empty = Node a t1 t2
difference (Node a t1 t2) (Node b t3 t4)
  | member a (Node b t3 t4) = difference (deleteRoot (Node a t1 t2)) (Node b t3 t4)
  | otherwise = Node a (difference t1 (Node b t3 t4)) (difference t2 (Node b t3 t4))

-- Tests whether an element is in the Set?
member :: (Ord a) => a -> Set a -> Bool
member _ Empty = False
member x (Node a t1 t2) = x == a || member x t1 || member x t2

-- Returns the number of elements in the Set?
cardinality :: Set a -> Int
cardinality Empty = 0
cardinality (Node _ t1 t2) = 1 + cardinality t1 + cardinality t2

-- Applies a function to every element in the Set
setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap _ Empty = Empty
setmap f (Node a t1 t2) = Node (f a) (setmap f t1) (setmap f t2)

-- Right folds a Set using a function *f*
setfoldr :: (a -> b -> b) -> b -> Set a -> b
setfoldr _ acc Empty = acc
setfoldr f acc (Node a t1 t2) = f a (setfoldr f (setfoldr f acc t1) t2)

-- Removes an element from the Set
-- Returns the set unaltered if *x* is not present
removeSet :: (Ord a) => a -> Set a -> Set a
removeSet x (Node a t1 t2)
  | not (x `member` Node a t1 t2) = Node a t1 t2
  | x < a = Node a (removeSet x t1) t2
  | x > a = Node a t1 (removeSet x t2)
  | x == a = deleteRoot (Node a t1 t2)
  | otherwise = Node a t1 t2

-- Auxillary Functions

-- Deletes the root of a binary tree
deleteRoot :: Ord a => Set a -> Set a
deleteRoot (Node _ t1 t2)
  | t1 == Empty = t2
  | t2 == Empty = t1
  | otherwise = Node (leftMost t2) t1 (removeLeftMost t2)

-- Returns the left most element in the binary tree
leftMost :: Set a -> a
leftMost (Node a Empty _) = a
leftMost (Node _ t1 _) = leftMost t1

-- Removes the left most element in the binary tree
removeLeftMost :: Set a -> Set a
removeLeftMost (Node _ Empty Empty) = Empty
removeLeftMost (Node _ Empty t2) = t2
removeLeftMost (Node a t1 t2) = Node a (removeLeftMost t1) t2