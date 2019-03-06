module BinaryTrees where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = x : preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) = inorder left ++ x : inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) = postorder left ++ postorder right ++ [x]

foldTree :: (a -> b -> b)
          -> b
          -> BinaryTree a
          -> b
foldTree _ x Leaf = x
foldTree f z (Node left x right) = f x (foldTree f (foldTree f z left) right)

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
      2
      (Node Leaf 3 Leaf)

bigTree :: BinaryTree Integer
bigTree =
  Node testTree 4 (Node Leaf 5 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "Bad news bears."

testFold :: IO ()
testFold =
  if foldTree (+) 0 bigTree == 15
  then putStrLn "FoldTree fine! 🎉"
  else putStrLn "Bad news bears."

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder
  testFold
