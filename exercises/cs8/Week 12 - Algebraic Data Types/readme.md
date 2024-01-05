# Седмица 12 - Алгебрични типове данни

## Задача 01 - Потребителски типове
Напишете клас `Number`, който наследява `Eq` и `Show` и съдържа следните функции:
- `plus::t -> t -> t`
- `mult::t -> t -> t`
- `fromInt::Int -> t`

Напишете следните потребителски типове:
- `Nat` - представлява естествено число. Типът да наследява класовете `Ord` и `Number` (`fromInt` за отрицателни числа да връща числото по модул);
- `Complex` - представлява комплексно число. Типът да наследява класа `Number`;
- `Matrix` - представлява матрица. Типът да наследява класовете `Functor`, `Foldable` и `Show`;
- `BinaryTree` - представлява двоично дърво. Типът да наследява класовете `Functor`, `Foldable` и `Show`;
- `BST` - представлява двоично наредено дърво. Типът да наследява класовете `Foldable` и `Show`;
- `Map` - представлява структурата от данни `map`. Направете 2 реализации - чрез асоциативен списък и чрез двоично наредено дърво. Типът да наследява класовете `Foldable` и `Show`;
- `Graph` - представлява граф, реализиран чрез спсисък на съседство. Типът да наследява класа `Show`.

## Задача 02 - Типът Maybe
Напишете следните функции, като използвате типа `Maybe`:
- `fromInt'::Int -> Maybe Nat` - превръща `Int` число в `Nat`, като ако числото е отрицателно да връща `Nothing`;
- `sub::Nat -> Nat -> Maybe Nat` - намира разликата на 2 естествени числа, само когато тя е естествено число;
- `find::(t -> Bool) -> [t] -> Maybe t` - намира първото срещане на елемент в списък по подаден предикат;
- `slice::String -> Nat -> Nat -> Maybe String` - по подаден символен низ и две естествени числа `start` и `end`, връща частта от низа, в която елементите са между индексите [start, start + end). Ако интервала не е дефиниран коректно, функцията да връща `Nothing`.

```haskell
testTree::BinaryTree Integer
testTree = Node 5 
                (Node 1 
                      (Node 4 
                            Empty 
                            (Node 13 Empty Empty)) 
                      (Node 3 Empty Empty)) 
                (Node 8 
                      (Node 0 
                            (Node 10 Empty Empty) 
                            (Node 9 Empty Empty)) 
                      (Node 11 Empty Empty))

bst :: BST Integer
bst = BSTNode 3 (BSTNode 1 
                         BSTEmpty 
                         (BSTNode 2 BSTEmpty BSTEmpty))
                (BSTNode 4 
                         BSTEmpty 
                         (BSTNode 5 BSTEmpty BSTEmpty))
```

## Задача 03 - Ротации
Напишете функции, които правят лява и дясна ротация на двоично дърво.

### Пример:
```haskell
ghci> rotateLeft testTree -- Node 8 (Node 5 (Node 1 (Node 4 Empty (Node 13 Empty Empty)) (Node 3 Empty Empty)) (Node 0 (Node 10 Empty Empty) (Node 9 Empty Empty))) (Node 11 Empty Empty)
ghci> rotateRight testTree -- Node 1 (Node 4 Empty (Node 13 Empty Empty)) (Node 5 (Node 3 Empty Empty) (Node 8 (Node 0 (Node 10 Empty Empty) (Node 9 Empty Empty)) (Node 11 Empty Empty)))
```

## Задача 04 - Нови листа
Напишете функция, която заменя всяко листо на двоично дърво с дърво, съдържащо само 2 листа, като всички възли в него имат стойност, същата като тази на замененото листо.

### Пример:
```haskell
ghci> bloom testTree -- Node 5 (Node 1 (Node 4 Empty (Node 13 (Node 13 Empty Empty) (Node 13 Empty Empty))) (Node 3 (Node 3 Empty Empty) (Node 3 Empty Empty))) (Node 8 (Node 0 (Node 10 (Node 10 Empty Empty) (Node 10 Empty Empty)) (Node 9 (Node 9 Empty Empty) (Node 9 Empty Empty))) (Node 11 (Node 11 Empty Empty) (Node 11 Empty Empty)))
```

## Задача 05 - Път от корен до листо
Напишете функция, която по подадено двоично дърво, връща списък от всички пътища от корена до някое листо на дървото.

### Пример:
```haskell
ghci> paths testTree -- [[5,1,4],[5,1,4,13],[5,1,3],[5,8,0,10],[5,8,0,9],[5,8,11]]
```

## Задача 06 - Най-близък родител
```haskell
commonAncestor::Eq a => a -> a -> BinaryTree a -> Maybe a
```
Напишете функция, която по подадени 2 елемента и двоично дърво намира най-близкия общ родител на двата елемента в дървото. Ако няма такъв, функцията да връща `Nothing`.

### Пример:
```haskell
ghci> commonAncestor 9 11 testTree -- Just 8
ghci> commonAncestor 9 3 testTree -- Just 5
ghci> commonAncestor 9 50 testTree -- Nothing
```

## Задача 07 - Двоично наредено дърво
Напишете следните функции, които използват двоично наредено дърво:
- `insert::Ord t => t -> BST t -> BST t` - добавя елемент към двоично наредено дърво;
- `remove::Ord t => t -> BST t -> BST t` - премахва елемент от двоично наредено дърво;
- `kthSmallest::Ord t => BST t -> Int -> t` - намира `k`-тия по големина елемент в двоично наредено дърво (Бонус: напишете безопасен вариант с `Maybe` и `Nat`);
- `rangeSearch::Ord t => t -> t -> BST t -> [t]` - връща списък от тези елементи от двоично наредено дърво, които са част от подадения интервал;
- `kClosestElements :: Ord a => a -> Int -> BST a -> [a]` - връща списък от първите `k` елементи, които са най-близки до даден елемент в двоично наредено дърво.