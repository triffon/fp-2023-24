# 12. Maybe, Either, Functor

![Hoogle meme](../assets/12-haskell-hoogle.png)

---

Днес ще се запознаем с два много простички типа, които ни дават огромна сила, а иммено:

```haskell
data Maybe' a = Nothing' | Just' a

data Either' e a = Left' e | Right' a
```

> [!NOTE]
> Това упражнение ще работим с тези версии на двата типа (завършващи на `'`, за може стандартната библиотека да не ни се пречка)

Чрез тях можем да изрязяваме, съответно, нетотални функции и функции, "хвърлящи exception-и".

```haskell
--                       VVVVVV
safeDiv :: Int -> Int -> Maybe' Int
safeDiv _ 0 = Nothing'
safeDiv x y = Just' $ x `quot` y

--                                VVVVVVV
safeDivWithError :: Int -> Int -> Either' String Int
safeDivWithError _ 0 = Left' "Cannot divide by zero"
safeDivWithError x y = Right' $ x `quot` y
```

Както всеки друг композитен тип, ние можем да pattern-match-ваме по тях:

```haskell
safeDivToMessage :: Maybe' Int -> String
safeDivToMessage Nothing' = "safeDiv returned Nothing"
safeDivToMessage (Just' res) = "safeDiv returned a result! Here it is: " ++ show res

safeDivWithErrorToMessage :: Either' String Int -> String
safeDivWithErrorToMessage (Left' err) = "safeDivWithError returned an error: " ++ err
safeDivWithErrorToMessage (Right' res) = "safeDivWithError returned a result! Here it is: " ++ show res
```

---

Миналият път си говорихме за класове от типове (typeclasses). Един по-сложен такъв, за който още не сме говорили, е `Functor`:

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

Той енкапсулира силата на даден "тип с дупка" (мислете си контейнер, който има е такъв типов параметър - `data Something a = ...`) да може да си променя едновременно елементите в себе си (колкото и в какъвто и формат да са)

```haskell
instance Functor Maybe' where
  fmap :: (a -> b) -> Maybe' a -> Maybe' b
  fmap f _ = undefined

instance Functor (Either' e) where
  fmap :: (a -> b) -> Either' e a -> Either' e b
  fmap f _ = undefined
```

> [!NOTE]
> "Тип с дупка" наричам(е) нещо от "kind" `* -> *` (`Type -> Type`). Пример за това е `Maybe'`, но не и `Either'`. `Either'` е от kind `* -> * -> *`, демек взима два типови параметъра допреди да стане на завършен тип. За да може да извадим нещо от kind `* -> *` от `Either'`, то трябва да запълним една от "дупките" му с някакъв свободно избран тип. Това играе ролята на висящото `e` в `instance Functor (Either' e)` - казваме, че за всеки тип `e`, `Either e` е `Functor`.
> Тези "kind"-ове може да проверявате с `:k` директивата в `GHCi` - `:k Either'`, `:k Either' e`, `:k Either' e a`

За задачите ще си направим и наш списък (че да не трябва да `import Prelude hiding (...)`-ваме отново):

```haskell
data List' a = Nil' | Cons' a (List' a)
```

# Задачи

1. Дефинирайте функциите `saveDiv2` и `saveDiv4` (деление без остатък на 2 и 4 респективно), като за `saveDiv4` ползвайте `saveDiv2` два пъти (някак)

```haskell
saveDiv2 6  -- Just 3
saveDiv2 11 -- Nothing
```

2. Дефинирайте функциите `listToMaybe :: [a] -> Maybe a` и `maybeToList :: Maybe a -> [a]`

3. Дефинирайте функция `getFirstAndLast :: (a -> Bool) -> [a] -> Maybe (a, a)`

```haskell
getFirstAndLast (>5) [1..10] -- (6,10)
```

4. Дефинирайте функция `untilJust :: (a -> Maybe b) -> (a -> a) -> a -> b`

```haskell
untilJust (\x -> safeDiv x 4) (+1) 5 -- 2
```

5. Дефинирайте функция `filterMap :: (a -> Maybe b) -> [a] -> [b]`, която `filter`-ва и `map`-ва едновременно

```haskell
filterMap saveDiv2 [1, 2, 3, 4, 6, 9, 12] -- [1, 2, 3, 6]
```

6. (**БОНУС** 1т) Дефинирайте функция `hop :: [Maybe a] -> Maybe [a]`

```haskell
hop [Just 2, Just 3, Just 4] == Just [2, 3, 4]
hop [Just 2, Nothing, Just 4] == Nothing
```

7. (**БОНУС** 1т) Дефинирайте `Functor` инстанции за `Maybe'`, `Either'` и `List'` (помислете защо функцията се казва `fmap`)

8. (**БОНУС** 3т) Дефинирайте функции от типовете `pure_ :: a -> _ a`, `flatten_ :: _ (_ a) -> _ a` и `andThen_ :: _ a -> (a -> _ b) -> _ b`, където `_` е съответно `Maybe'`, `Either' e` и `List'`. Помислете/обсъдете какво би било смислено да правят

```haskell
pureMaybe'  :: a -> Maybe'    a
pureEither' :: a -> Either' e a
pureList'   :: a -> List'     a

flattenMaybe'  :: Maybe'    (Maybe'    a) -> Maybe'    a
flattenEither' :: Either' e (Either' e a) -> Either' e a
flattenList'   :: List'     (List'     a) -> List'     a

andThenMaybe'  :: Maybe'    a -> (a -> Maybe'    b) -> Maybe'    b
andThenEither' :: Either' e a -> (a -> Either' e b) -> Either' e b
andThenList'   :: List'     a -> (a -> List'     b) -> List'     b
```
