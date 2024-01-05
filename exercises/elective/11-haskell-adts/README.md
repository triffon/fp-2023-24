# 11. Алгебрични Типове Данни (ADTs) и typeclasses в Haskell

![Haskell meme](https://www.explainxkcd.com/wiki/images/8/8d/haskell.png)

---

[Безсрамно откраднато от миналата година](https://github.com/triffon/fp-2022-23/blob/main/exercises/inf2/13/README.md) по модул форматиране :smile:

## [Tипове данни](../09/README.md#типове-данни)

```haskell
> :t True -- => True :: Bool
> :t 5 -- => 5 :: Int или 5 :: Integer
> :t 5.0 -- => 5.0 :: Float или 5.0 :: Double
> :t 'a'  -- => 'a' :: Char
> :t "Hello" -- => "Hello" :: String
> :t (True, 'a')  -- => (True, 'a') :: (Bool, Char)

-- функциите също имат тип
-- addThree е функция, която приема 3 аргумента от
-- тип Int и връща резултат от тип Int
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
```

## Синоним на тип

Типовите синоними ни позволяват да задаваме нови имена на вече съществуващи типове. Използваме ги за по-добра четимост на кода.

```haskell
-- еквивалентни и взаимнозаменяеми
type String = [Char]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

-- типовите синоними могат да бъдат
-- параметризирани
type AssocList k v = [(k,v)]
```

## Потребителски дефинирани типове

```haskell
-- дефинираме нови типове с data
-- преди = стои името на типа
-- след  = стоят констуркторите на стойности
-- Bool е изброен тип
data Bool = False | True

-- дефинираме нов тип - форма
-- формата може да бъде кръг или правоъгълник
data Shape = Circle Float Float Float | Rectangle Float Float Float Float

-- конструкторът Circle приема 3 аргумента от тип Float
-- координати на центъра на кръга
-- радиус на кръга

-- конструкторът Rectangle приема 4 аргумента от тип Float
-- координати на горен ляв ъгъл на правоъгълника
-- координати на долен десен ъгъл на правоъгълника

-- функция, която приема форма и връща лицето и
-- не може типовата декларация на функцията да е Circle -> Float,
-- защото Circle не е тип, Shape e
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- създадваме форми и намираме лицето им
surface $ Circle 10 20 10
surface $ Rectangle 0 0 100 100
```

<details>
  <summary>Запис с полета</summary>

  ```haskell
  -- тип Car, който се дефинира с компанията, модела и годината си
  -- не е веднага очевидно какво се очаква да получи конструкторът
  data Car = Car String String Int

  let car = Car "Ford" "Mustang" 1967

  -- трябва сами да си дефинираме getters
  -- за отделните полета на кола
  company :: Car -> String
  company (Car company _ _) = company

  -- по-добър вариант - запис с полета
  data Car =
    Car { company :: String,
          model :: String,
          year :: Int
        }

  -- автоматично създава gettres company, model и year
  let car = Car { company="Ford", model="Mustang", year=1967 }
  ```
</details>

## Типови променливи

Позволяват ни да обработваме елементи от различен типове по универсален начин.

```haskell
-- head е функция, която приема списък от каквито и да е
-- елементи и връща първия елемент от него;
-- "a" е типова променлива и може да бъде от всеки тип
> :t head -- => head :: [a] -> a

-- fst е функция, която приема наредена двойка от каквито и
-- да е елементи и връща първия елемент от нея;
-- "a" и "b" са типови променливи и могат да бъдат от всеки тип
> :t fst -- => fst :: (a, b) -> a
```

## [Класове от типове](http://learnyouahaskell.com/types-and-typeclasses#typeclasses-101)

Клас от типове наричаме интерфейс, който дефинира някакво поведение.

Някои основни класове от типове:

- `Eq` - включва типовете, чиито стойности могат да се сравняват, използвайки `==` и `/=`
- `Ord` - включва типовете, чиито стойности имат наредба и могат да се сравняват използвайки `>`, `<`, `>=`, `<=` и `compare`
- `Enum` - включва типовете, чиито стойности могат да бъдат изброени - изполваме ги в `range`, имат дефинирани предшественик (`pred`) и наследник (`succ`)
- `Num` - включва числените типове
- `Integral` - включва типовете `Int` и `Integer`
- `Floating` - включва типовечт `Float` и `Double`
- `Show` - включва типовете, чиито стойности могат да бъдат представени като низове, използвайки `show`
- `Read` - включва типовете, чиито стойности могат да бъдат прочетени от низове, използвайки `read`

```haskell
-- изрежда всички класове, които са инстанции на
-- класа от типове Eq (примерно: Int, Char, Bool)
> :info Eq
```

```haskell
-- функцията за сравнение == приема два аргумента от един и същи тип,
-- който принадлежи на класа от типове Eq (стойностите могат да бъдат сравнявани),
-- и връща булева стойност
> :t (==) -- => (==) :: (Eq a) => a -> a -> Bool
-- "Eq а" е класово ограничение
```

## Дефиниране на инстанции на клас от типове

Даден тип може да бъде направен инстанция на клас ако поддържа и имплементира неговото поведение.

Когато казваме, че даден тип е инстанция на клас от типове, имаме предвид, че може да използва функциите, които класа от типове дефинира.

```haskell
-- с data дефинираме нови типове
-- виж "Потребителски дефинирани типове" по-долу
data TrafficLight = Red | Yellow | Green

-- правим дефинирания от нас тип Traffic Light
-- инстанция на класа от типове Eq
-- така данни от тип TrafficLight ще могат да
-- бъдат сравнявани с == и /=
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

-- правим дефинирания от нас тип Traffic Light
-- инстанция на класа от типове Show
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"
```

## Автоматични инстанции на клас от типове

Haskell може автоматично да направи типа ни инстанция на всеки от следните класове от типове:  `Eq`, `Ord`, `Enum`, `Show`, `Read`.
За целта използваме `deriving`.

```haskell
data Person =
  Person { firstName :: String
         , lastName :: String
         , age :: Int
         } deriving (Eq, Show, Read)
```

## Типови параметри

```haskell
-- конструкторите на стойности приемат като аргумент стойности и връщат нова стойност
-- подобно, конструкторите на типове приемат като аргумент типове и връщат нов тип

-- конструкторите на типове са подобни на templates в c++

data Maybe a = Nothing | Just a

-- никоя стойност не може да е от тип Maybe,
-- но може да бъде от тип Maybe Int, Maybe Car, Maybe String, etc.

> :t Just 'a' -- => Just 'a' :: Maybe Char
```

## Рекурсивни типове

```haskell
data List a = EmptyList | Cons a (List a) deriving (Show, Read, Eq, Ord)

data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a) deriving (Eq, Show, Read)
```

## Двоични дървета

```haskell
data BinaryTree a = EmptyTree | Node { root :: a, left :: BinaryTree a, right :: BinaryTree a } deriving (Eq, Show, Read)

makeLeaf :: a -> BinaryTree a
makeLeaf x = Node x EmptyTree EmptyTree

mapBinaryTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBinaryTree _ EmptyTree = EmptyTree
mapBinaryTree f (Node x l r) =
    Node (f x) (mapBinaryTree f l) (mapBinaryTree f r)
```
---

## Задачи

```haskell
tree :: BinaryTree Int
tree =
  Node 1
    (Node 2
      (Node 3
        EmptyTree
        EmptyTree)
      EmptyTree)
    (Node 4
      EmptyTree
      EmptyTree)
```

1. Дефинирайте функция `depth tree`, която намира дълбочината на подаденото двоично дърво

2. Дефинирайте функция `countLeaves tree`, която намира броя на листата на подаденото двоично дърво

3. Дефинирайте функции `collectPreOrder tree` и `collectInOrder tree`, които връщат списък от елементите на дървото, обходено съотвено `root-left-right` и `left-root-right`

4. (**БОНУС** 0.5т) Дефинирайте функция `level tree index`, която връща списък от стойностите на възлите, намиращи се на дълбочина `index` от корена

5. Дефинирайте функция `prune tree`, която премахва всички листа в подаденото дърво

6. Дефинирайте функция `invert tree`, която връща огледалния образ на `tree`

7. (**БОНУС** 0.5т) Дефинирайте функция `path element tree`, която намира пътя до element в подаденото дърво или връща `[]`, ако такъв не съществува

8. Дефинирайте фунцкия `contains tree path`, която проверява дали пътят `path` - списък от стойности, се съдържа в подаденото дърво

9. (**БОНУС** 1т) Двоично наредено дърво е двоично дърво, в което:
    - елементите, <= от корена, се намират в лявото поддърво
    - елементите, > от корена, се намират в дясното поддърво

    Дефинирайте предикат `binarySearchTree tree`, който проверява дали подаденото дървото е наредено.

    ```haskell
    tree :: BinaryTree Int
    tree =
      Node 3
        (Node 1
          EmptyTree
          (Node 2
            EmptyTree
            EmptyTree))
        (Node 4
          EmptyTree
          (Node 5
            EmptyTree
            EmptyTree))
    ```

10. (**БОНУС** 2т) Дефинирайте функция `binarySearchTreeInsert tree element`, която вмъква елемента `element` в подаденото двоичното наредено дърво.
Новополученото дърво трябва да бъде двоично наредено.

11. (**БОНУС** 0.5т) Дефинирайте функция `treeSort lst`, която сортира подадения списък, използвайки двоично наредено дървo
