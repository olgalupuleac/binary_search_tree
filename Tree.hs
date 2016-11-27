import Prelude hiding (lookup)

-- –еализовать двоичное дерево поиска без балансировки (4 балла)
data BinaryTree k v = Empty | Node k v (BinaryTree k v) (BinaryTree k v) deriving Show

-- УOrd k =>Ф требует, чтобы элементы типа k можно было сравнивать 
lookup :: Ord k => k -> BinaryTree k v -> Maybe v
lookup x (Node k v left right) | x == k  = Just v
                               | x > k   = lookup x right
                               | x < k   = lookup x left
lookup _ _                               = Nothing

insert :: Ord k => k -> v -> BinaryTree k v -> BinaryTree k v
insert x y Empty                          = Node x y Empty Empty 
insert x y (Node k v left right) | x > k  = Node k v left (insert x y right)
                                 | x < k  = Node k v (insert x y left) right
                                 | x == k = Node k y left right

delete :: Ord k => k -> BinaryTree k v -> BinaryTree k v
delete x (Node k v Empty right)  | x == k = right
                                 | x > k  = Node k v Empty (delete x right)
delete x (Node k v left right)   | x == k = let (k1, v1) = find_right left            
                                            in Node k1 v1 (delete k1 left) right
                                 | x > k  = Node k v left (delete x right)
                                 | x < k  = Node k v (delete x left) right
                                 where find_right (Node k v _ Empty) = (k, v)
                                       find_right (Node _ _ _ right) = find_right right