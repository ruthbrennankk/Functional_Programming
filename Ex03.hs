{- brennar5 Ruth Anne Brennan -}
module Ex03 where
import Data.List ((\\))

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !

-- Binary Tree
data BT a b
  = Leaf
  | Branch (BT a b) a b (BT a b)
  deriving (Eq, Show)

-- association list
type Assoc a b = [(a,b)]

-- lookup binary (search) tree
lkpBST :: Ord a1 => BT a1 a -> a1 -> Maybe a
lkpBST Leaf _  =  Nothing
lkpBST (Branch left k d right) k'
 | k < k'     =  lkpBST left k'
 | k > k'     =  lkpBST right k'
 | otherwise  =  Just d

-- Coding Part 1 (13 Marks)

-- insert into binary (search) tree
insBST :: Ord a => a -> b -> BT a b -> BT a b

-- insBST _ _ _  =  error "insBST not yet implmented"

insBST a b Leaf = Branch Leaf a b Leaf
insBST a b (Branch leftBST k d rightBST)
    | a < k     = Branch (insBST a b leftBST) k d rightBST
    | a > k     = Branch leftBST k d (insBST a b rightBST)
    | otherwise = Branch leftBST a b rightBST

-- Coding Part 2 (6 Marks)

-- convert an association list to a binary search tree
assoc2bst :: Ord a => Assoc a b -> BT a b

--assoc2bst _ = error "assoc2bst not yet implemented"

assoc2bst [] = Leaf
assoc2bst [(x,y)] = insBST x y Leaf
assoc2bst ((x,y):xs) = insBST x y (assoc2bst xs)

-- Coding Part 3 (6 Marks)

-- convert a binary search tree into an (ordered) association list
bst2assoc :: Ord c =>  BT c e -> Assoc c e

--bst2assoc _ = error "bst2assoc not yet implemented"

bst2assoc Leaf = []
bst2assoc (Branch Leaf a b Leaf) = [(a,b)] 
bst2assoc (Branch leftBST a b rightBST) = bst2assoc leftBST ++ [(a,b)] ++ bst2assoc rightBST




