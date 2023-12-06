# Списъци и кортежи

### Задача 1
Дефинирайте функция, която обръща число:
 - на един ред
 - на един ред с "магия"

```haskell
print $ revOneLine 123 == 321
print $ revOneLineMagic 123 == 321
```

### Задача 2
Дефинирайте функция, която:
 - добавя 1 към всеки елемент на списък, използвайки частично прилагане
 - добавя 1 към число, използвайки частично прилагане
 - повдига число на квадрат и добавя едно, използвайки частично прилагане и композиция на функции

```haskell
print $ addOneXs [1, 2, 3, 4, 5] == [2, 3, 4, 5, 6]
print $ addOneN 5 == 6
print $ sqPlusOne 5 == 26
```

### Задача 3
Дефинирайте функция, която връща сумата на най-малкия и най-големия делител на цяло число, които са палиндроми и по-големи от 1.

```haskell
print $ sumMinMaxPalindromes 132465 == 8
print $ sumMinMaxPalindromes 654546 == 8
print $ sumMinMaxPalindromes 100001 == 100012
print $ sumMinMaxPalindromes 21612 == 21614
print $ sumMinMaxPalindromes 26362 == 26364
```

### Задача 4
Дефинирайте функция, която проверява дали цифрите на дадено цяло число са подредени в нарастващ ред.

> Забележка: Без използване на `div` и `mod`

```haskell
print $ isAscending 123 == True
print $ isAscending 122 == True
print $ isAscending 0 == True
print $ isAscending 10 == False
print $ isAscending 12340 == False
print $ isAscending 12349 == True
```

### Задача 5
Дефинирайте функция, която приема списък от цели числа и връща списък от списъци, всеки от който съдържа последователни числа.

```haskell
print $ pack [1, 2, 3, 7, 8 ,9] == [[1,2,3],[7,8,9]]
print $ pack [1, 7, 8 ,9] == [[1],[7,8,9]]
print $ pack [1, 9] == [[1],[9]]
```

### Задача 6
Дефинирайте функция, която приема дума и връща списък от наредени двойки от вида `(<буква>, <брой срещания на буквата>)`. Не правете разлика между малки и големи букви.

> Подсказка: вижте какво прави функцията `group`

```haskell
print $ countOccurrences "Test" == [('e',1),('s',1),('t',2)]
print $ countOccurrences "ThisIsAReallyLongWordContaingAlmostEveryCharacter" == [('a',6),('c',3),('d',1),('e',4),('g',2),('h',2),('i',3),('l',4),('m',1),('n',3),('o',4),('r',5),('s',3),('t',4),('v',1),('w',1),('y',2)]
```

### Задача 7
Дефинирайте функция, която приема два списъка `xs` и `ys` и проверява дали съществува число `n`, такова че **y<sub>i</sub> = n + x<sub>i</sub>** за всяко `i`.

> Подсказка: вижте какво прави функцията `zipWith`

```haskell
print $ isImage [] [] == True
print $ isImage [1, 2, 3] [2, 3, 4] == True
print $ isImage [1, 2, 3] [4, 6, 9] == False
print $ isImage [1, 2, 3] [2, 5, 4] == False
```

### Задача 8
Дефинирайте функция, която връща сумата на първите `n` прости числа, съдържащи цифрата `d`.

> Подсказка: вижте как можете да създадете безкраен списък чрез list comprehension

```haskell
print $ sumSpecialPrimes 5 2 == 392
print $ sumSpecialPrimes 5 3 == 107
print $ sumSpecialPrimes 10 3 == 462
```

### Задача 9
Дефинирайте функция, която приема цяло число и връща неговия растящ ляв суфикс. Растящ ляв суфикс на едно число и числото, което сформира строго растяща редица от цифри, прочетено от ляво надясно.

> Подсказка: вижте какво прави функциите `inits` и `tails`

```haskell
print $ ascendingleftSuffix 37563 == 36
print $ ascendingleftSuffix 32763 == 367
print $ ascendingleftSuffix 32567 == 7
print $ ascendingleftSuffix 32666 == 6
```

### Задача 10
Дефинирайте функция, която сумира уникалните числа в списък от списъци.

```haskell
print $ sumUnique [[1,2,3,2],[-4,-4],[5]] == 9 -- (= 1 + 3 + 5)
print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] == 0
print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] == 45
```

### Задача 11
Дефинирайте функция, която връща броя на различните символи/цифри (игнорираме малки и големи букви), които се срещат повече от веднъж в символен низ.

```haskell
print $ duplicateCount "" == 0 -- no characters repeats more than once
print $ duplicateCount "abcde" == 0
print $ duplicateCount "aabbcde" == 2 -- 'a' and 'b'
print $ duplicateCount "aabBcde" == 2 -- 'a' occurs twice and 'b' twice (`b` and `B`)
print $ duplicateCount "indivisibility" == 1 -- 'i' occurs six times
print $ duplicateCount "Indivisibility" == 1
print $ duplicateCount "aA11" == 2 -- 'a' and '1'
print $ duplicateCount "ABBA" == 2 -- 'A' and 'B' each occur twice
print $ duplicateCount "Indivisibilities" == 2 -- 'i' occurs seven times and 's' occurs twice
print $ duplicateCount ['a'..'z'] == 0
print $ duplicateCount (['a'..'z'] ++ ['A'..'Z']) == 26
```

### Задача 12
Дефинирайте функция, която приема символен низ и премахва последователни дублиращи се символа. Два символа са дублиращи, ако:
 - представят един и същи символ
 - са един до друг
 - първият е главна буква, вторият е малка буква или обратното

```haskell
print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD
                                                            ^^                 ^^                   ^^
```
