{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.List

main :: IO ()
main =do
  putStrLn "Тесты к задаче №9"
  print (separateList [1,2,3,4,5])
  print(separateList [])
  print(separateList [1])
  putStrLn "Тесты к задаче №11"
  print (resultOfSeparetingFromBegining 2 [1,2,3,4,5])
  print(resultOfSeparetingFromBegining 2 [])
  print(resultOfSeparetingFromBegining 2 [1])
  putStrLn "Тесты к задаче №16"
  print (addToEnd [-1,-2,-3,-4,-5] [1,2,3,4,5])
  print(addToEnd [] [])
  print(addToEnd [2] [1])
  putStrLn "Тесты к задаче №20"
  print (properAnswer [firstAtom [['a', 'b'], "c", "d"]])
  print (properAnswer [firstAtom ["s"]])
  putStrLn "Тесты к задаче №26"
  print(pairSeparate [1,2,3,4,5,6])
  print(pairSeparate [1])
  print(pairSeparate [])
  putStrLn "Тесты к задаче №29"
  print(listDepth [[1,2,3], [4,3,2], [9,1,2,3]])
  print(listDepth [])
  print(listDepth [[1]])
  putStrLn "Тесты к задаче №36"
  print(notCommonElements [1,2,3] [4,5,6])
  print(notCommonElements [1,2,3] [1,6,7])
  print(notCommonElements [1] [])
{-
Oпределите функцию, разделяющую исходный список на два подсписка. В
первый из них должны попасть элементы с нечетными номерами, во второй 
элементы с четными номерами.
-}
separateList :: [Int] -> [[Int]]
separateList [] = [[]]
separateList [x] = [[x],[]]
separateList (x:y:xs) = [x:xp, y:yp] where [xp, yp] = separateList xs

{-
Определите функцию, осуществляющую разделение исходного списка на два
подсписка. В первый из них должно попасть указанное количество элементов
с начала списка, во второй  оставшиеся элементы.
-}

separateListFromBegining :: Int -> [Int] -> [Int]
separateListFromBegining 0 _ = []
separateListFromBegining _ [] = []
separateListFromBegining n (x:xs) = x : separateListFromBegining (n-1) xs

resultOfSeparetingFromBegining :: Int -> [Int] -> [[Int]]
resultOfSeparetingFromBegining n ans = [separateListFromBegining n ans, drop n ans]
{-
Определите функцию, добавляющую элементы одного списка во второй спи-
сок, начиная с заданной позиции.
-}
reverseFunc :: [Int] -> [Int]
reverseFunc [] = []
reverseFunc (x:xs) = reverseFunc xs ++ [x]

addToEnd :: [Int] -> [Int] -> [Int]
addToEnd [] [] = []
addToEnd [] _ = []
addToEnd xs ys = ys ++ reverseFunc xs
{-
Определите функцию ПЕРВЫЙ-АТОМ, результатом которой будет первый атом
списка.
-}
firstAtom :: [String] -> String
firstAtom [] = error "Empty string"
firstAtom [x] = x
firstAtom xs = firstAtom [head xs]

properAnswer :: [String] -> Char
propetAnswer [] = error "Empty string"
properAnswer ["x"] = 'x'
properAnswer [xs] = head xs
{-Определите функцию, разбивающую список (a b с d...)
на пары ((а b) (с d)...).
-}

pairSeparate :: [Int] -> [[Int]]
pairSeparate [] = []
pairSeparate [x] = [[x]]
pairSeparate (x:y:xs) = [x,y] : pairSeparate xs
{-
  Определите функцию, вычисляющую глубину списка (самой глубокой ветви).
-}
listDepth :: [[Int]] -> [Int]
listDepth [] = []
listDepth ([x]) = x
listDepth (x:xs) = listDepth xs
{-
Определите предикат НЕПЕРЕСЕКАЮЩИЕСЯ, проверяющий, что два множества не
пересекаются, т.е. у них нет общих элементов.
-}

notCommonElements :: [Int] -> [Int] -> Bool
notCommonElements [] [] = False
notCommonElements x y =
   let arrayLength = length x
   in
      if | length (x \\ y) == arrayLength -> True
         | otherwise -> False
{-
Определите функционал (APL-APPLY f x), который применяет каждую функ-цию fi
списка f1 f2 ... fn)  к соответствующему элементу списка  x = (x1 x2 ... xn) и возвращает список, сформированный из результатов.
-}
