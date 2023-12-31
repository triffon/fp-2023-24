# Мутиращи операции и потоци

### Задача 1
Реализиайте процедура, която чете единствен ред от стандартния вход, разделя го на отделни елементи относно символа ' ' и ги запазва в списък.

### Задача 2 :mirror:
Реализирайте програма, която отпечатва собствения си source code.

### Задача 3 :framed_picture:
След поредния невзет изпит Радо сериозно започва да се замисля дали ФМИ е за него... Вътрешно той знае за себе си, че не е роден да става програмист, а че неговото призвание е изкуството. Радо е голям почитател на обработването на изображения - да отрязва конкретни части от тях, да ги преоразмерява, да добавя ефекти и т.н. Покажете на Радо, че може да съчетава изкуството с програмирането, като му помогнете да си напише собствена програма, която прави един простичък, но полезен ефект - превръща цветни изображения в черно-бели такива. (*за да звучи по-fancy - от RGB в Grayscale*).

Тошко - най-добрият приятел на Радо, му подсказал, че най-лесно би постигнал това, като обработва `bmp` изображения. Той му обяснил, че това всъщност не са нищо повече от обикновени двоични файлове. Като един истински приятел, Тошко дори разяснил в подробности какви са особеностите на `bmp` изображенията:
- Състоят се от **две** основни части - служебна информация за изображението (**метаданни**) и матрица от пиксели (**практическото съдържание на изображението**)
- Не ни е необходима цялата информация от метаданните; трябват ни само:
    - размерът на файла (**позиция 2**, *4 байта*)
    - позицията, на която започва матрицата от пиксели (**позиция 10**, *4 байта*)
    - брой колони на матрицата от пиксели (**позиция 18**, *4 байта*)
    - брой редове на матрицата от пиксели (**позиция 22**, *4 байта*)
- Всеки пиксел се състои от *3 байта*, по един за всеки от основните три цвята - червено, зелено и синьо (**RGB**) (стойности между **0** и **255**)
- Всеки ред има **padding**, допълващ броя на байтовете на всеки ред да бъде число, делящо се на **4**
- Информацията в padding-a се игнорира при визуализиране на изображението

За да превърне RGB пиксел в Grayscale такъв, Радо попитал своята приятелка Габи, която е специалистка в алгоритмите. За да покаже колко много знае, тя му дала списък със 100 различни алгоритми:
- `Grayscale Value = (R + G + B) / 3`
- `Grayscale Value = (max(R, G, B) + min(R, G, B)) / 2`
- `Grayscale Value = 0.2126 * R + 0.7152 * G + 0.0722 * B`
- `Grayscale Value = 0.30 * R + 0.59 * G + 0.11 * B`
- *списъкът продължава с още 96 алгоритъма...*

Тъй като алгоритмите практически постигат почти един и същи резултат, няма особено голямо значение кой от тях ще избере Радо.

За да бъдете от истинска помощ на Радо, приемете две имена на двоични файлове на изображения в `bmp` формат от командния ред - първият трябва да бъде на съществуващо цветно RGB изображение, а във втория трябва да бъде запазеното конвертираното Grayscale изображение. Използвайки информацията, предоставена ви от Тошко и Габи, реализирайте алгоритъма за конвертиране чрез подходящи процедури.

### Задача 4
Реализирайте процедура `stream-range`, която приема две числа `a` и `b` и генерира поток, състоящ се от всички числа в интервала `[a;b]`.

```racket
(equal? (stream-take 5 (stream-range 5 100)) '(5 6 7 8 9))
(equal? (stream-take 8 (stream-range 1 3)) '(1 2 3))
```

### Задача 5
Реализирайте процедура `natural-numbers`, която генерира безкраен поток от естествени числа.

```racket
(equal? (stream-take 5 natural-numbers) '(1 2 3 4 5))
(equal? (stream-take 8 natural-numbers) '(1 2 3 4 5 6 7 8))
```

### Задача 6
Реализирайте процедура `stream-map`, която приема унарна процедура `f` и поток и я прилага върху елементите на потока.

```racket
(equal? (stream-take 5 (stream-map (λ (x) (* x 2)) natural-numbers)) '(2 4 6 8 10))
(equal? (stream-take 8 (stream-map (λ (x) (* x 2)) natural-numbers)) '(2 4 6 8 10 12 14 16))
```

### Задача 7
Реализирайте процедура `stream-filter`, която приема унарен предикат `pred?` и поток и връща поток, състоят се само от елементите, удовлетворяваши предиката.

```racket
(equal? (stream-take 5 (stream-filter odd? natural-numbers)) '(1 3 5 7 9))
(equal? (stream-take 8 (stream-filter even? natural-numbers)) '(2 4 6 8 10 12 14 16))
```