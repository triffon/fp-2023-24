# Седмица 09 - Потоци и Въведение в Хаскел

```scheme
(define-syntax delay
  (syntax-rules () ((delay x)
                    (lambda () x))))

(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t)
                    (cons h (delay t)))))

(define head car)
(define (tail s) ((force (cdr s)))) 
```

## Задача 01 - Взимане на елементи от поток
Напишете функция, която по подадено число `n` и поток, връща списък от първите `n` елемента на потока.

## Задача 02 - Естествени числа
Напишете функция, която генерира безкраен поток от всички естествени числа.

### Пример:
```scheme
(take 10 nats) ; -> '(0 1 2 3 4 5 6 7 8 9)
```

## Задача 03 - Map за потоци
Напишете функция `map-stream`, която е аналогът на `map`, но за потоци.

### Пример:
```scheme
(take 10 (map-stream (lambda (x) (* x x)) nats)) ; -> '(0 1 4 9 16 25 36 49 64 81)
```

## Задача 04 - Фибоначи
Напишете функция, която генерира безкраен поток от числата на Фибоначи

## Пример:
```scheme
(take 10 fib) ; -> '(0 1 1 2 3 5 8 13 21 34)
```

## Задача 05 - Триъгълник на Паскал
Напишете функция, която генерира безкраен поток от списъци, представляващи редовете от тригълника на Паскал.

## Пример:
```scheme
(take 5 pascal-triangle) ; -> '((1) (1 1) (1 2 1) (1 3 3 1) (1 4 6 4 1))
```

## Задача 06* - Безкрайни суми (второ контролно 2017)
Напишете функция, която по подадени 2 положителни числа `n` и `k` генерира безкраен поток, в който първото число е `k`, а всяко следващо е сумата на предните `n` числа.

### Пример:
```scheme
(take 10 (sum-last 3 5)) ; -> '(3 3 6 12 24 48 93 183 360 708)
```

# Haskell

## Задача 00 - Настройване на средата
Изтеглете `GHCup` от следния [линк](https://www.haskell.org/ghcup). Следвайте инструкциите за да инсталирате `GHC` (компилатор + интерпретатор за Haskell), `HLS` (haskell language server), `MSYS2`, ако сте на Windows и го нямате изтеглено все още, `cabal` и `stack` (последните 2 ще са ви необходими за проектите - това са инструменти за `build`-ване на проекти и менежиране на пакети). Ако сте на Windows и имате проблем с инсталацията, използвайте командите от следния [линк](https://www.haskell.org/ghcup/guide/#troubleshooting). За да се възползвате от всички функции на HLS, трябва да инсталирате следния [extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) за VS Code. При първоначалното му стартиране изберете опцията да използва `GHCup`, като може да се наложи да добавите ръчно пътя към него в настройките на extension-a.

## Задача 01 - Факториел
Напишете функция, която пресмята факториел. Реализирайте я по 3 начина - чрез `pattern matching`, `guards` и с `if then else`.

### Пример:
```haskell
ghci> factorial 5 -- 120
```

## Задача 02 - Квадратно уравнение
Напишете функция, която приема 3 аргумента - коефициентите на квадратно уравнение. Функцията да връща броя на корените на квадратното уравнение.

### Пример:
```haskell
ghci> roots 1 3 2 -- 2
ghci> roots 1 2 1 -- 1
ghci> roots 1 2 2 -- 0
```

## Задача 03 - Композиция
Напишете функция, която по подадена едноместна функция и естествено число `n`, връща `n`-кратната композиция на подадената функция

### Пример:
```haskell
ghci> (repeat succ 6) 7 -- 13
```

## Задача 04 - Сума на точни квадрати
Напишете функция, която приема 2 естествени числа - `a` и `b`. Функцията да намира сумата на всички числа в интервала [a, b], които са точни квадрати.

### Пример:
```haskell
ghci> sumSquares 1 20 -- 30
```

## Задача 05 - Модул на комплексно число
Напишете функция, която приема комплексно число като наредена двойка от реална и имагинерна част. Функцията да намира модулът на това число. Изпробвайте функцията `curry` върху вашата функция.

### Пример:
```haskell
ghci> modulus (3, 4) -- 5
```

## Задача 06* - Accumulate
Напишете функция `accumulate`, която прави същото като функцият `accumulate`, която реализирахме на `scheme`. Помислете как да я типизирате, така че да може да връща резултат от произволен тип.

### Пример:
```haskell
ghci> accumulate (+) 0 1 10 id succ -- 55
```