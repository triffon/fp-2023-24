import Data.Char

main :: IO()
main = do
    print $ complexOperation 10 2
    print $ complexOperation 10 0
    print $ complexOperation 10 (-2)

data Chain a = Singleton a | Append (Chain a) (Chain a)
    deriving (Show, Eq)

class Appendable c where
    prepend :: a -> c a -> c a
    append :: c a -> a -> c a
    concatenate :: c a -> c a -> c a

instance Appendable Chain where
    prepend x c = Append (Singleton x) c
    append c x = Append c (Singleton x)
    concatenate c1 c2 = Append c1 c2

data Validated e a = Valid a | Invalid (Chain e)
    deriving (Show, Eq)

instance Functor (Validated e) where
    fmap _ (Invalid c) = Invalid c
    fmap f (Valid x) = Valid (f x)

instance Applicative (Validated e) where
    pure x = Valid x
    (Valid f) <*> (Valid x) = Valid (f x)
    (Valid _) <*> (Invalid e) = Invalid e
    (Invalid e) <*> _ = Invalid e

instance Monad (Validated e) where
    return = pure
    (Valid x) >>= f = f x
    (Invalid e) >>= _ = Invalid e

safeDivide :: Double -> Double -> Validated String Double
safeDivide _ 0 = Invalid (Singleton "division by 0")
safeDivide x y = Valid (x / y)

safeRoot :: Double -> Validated String Double
safeRoot x
    | x < 0 = Invalid (Singleton "square root of a negative number")
    | otherwise = Valid (sqrt x)

complexOperation :: Double -> Double -> Validated String Double
complexOperation a b = do
    res1 <- safeDivide a b
    res2 <- safeRoot res1
    return res2

data Email = Email
  { user   :: String, 
    domain :: String
  } deriving (Show)

data Date = Date
  { day   :: Int, 
    month :: Int,
    year  :: Int
  } deriving (Show)

data User = User
  { userName       :: String, 
    userEmail      :: Email,
    passwordHash   :: String,
    birthday       :: Date,
    userPostalCode :: Maybe String
  } deriving (Show)

data RegistrationForm = RegistrationForm
  { name                 :: String, 
    email                :: String,
    password             :: String,
    passwordConfirmation :: String,
    birthDay             :: String,
    birthMonth           :: String,
    birthYear            :: String,
    postalCode           :: String
  } deriving (Show)

data DateError = YearIsNotAnInteger String | 
                 MonthIsNotAnInteger String | 
                 DayIsNotAnInteger String | 
                 MonthOutOfRange Int | 
                 DayOutOfRange Int | 
                 InvalidDate Int Int Int
                 deriving (Show)

data RegistrationFormError = NameIsEmpty | 
                             InvalidEmail String |
                             PasswordTooShort |
                             PasswordRequiresGreaterSymbolVariety |
                             PasswordsDoNotMatch |
                             InvalidBirthdayDate (Chain DateError) |
                             BirthdayDateIsInTheFuture Date |
                             InvalidPostalCode String
                             deriving (Show)

validateName :: String -> Validated RegistrationFormError String
validateName "" = Invalid (Singleton NameIsEmpty)
validateName name = Valid name

validateEmail :: String -> Validated RegistrationFormError Email
validateEmail email = case helper email of
    Just (user, domain) -> Valid Email { user = user, domain = domain }
    Nothing -> Invalid (Singleton (InvalidEmail email))
    where
        helper :: String -> Maybe (String, String)
        helper s = if countAt == 1
            then Just (takeWhile (/= '@') s, tail $ dropWhile (/= '@') s)
            else Nothing
            where
                countAt = length $ filter (== '@') s

validatePasswordLength :: String -> Validated RegistrationFormError String
validatePasswordLength password = if (length password < 8) 
    then Invalid (Singleton PasswordTooShort)
    else Valid password

validatePasswordVariety :: String -> Validated RegistrationFormError String
validatePasswordVariety password = if (any isDigit password && any isLower password && any isUpper password)
    then Valid password
    else Invalid (Singleton PasswordRequiresGreaterSymbolVariety)

validatePasswordEquality :: String -> String -> Validated RegistrationFormError String
validatePasswordEquality password passwordConfirmation = if (password == passwordConfirmation)
    then Valid password
    else Invalid (Singleton PasswordsDoNotMatch)

validatePassword :: String -> String -> Validated RegistrationFormError String
validatePassword password passwordConfirmation = do
    _ <- validatePasswordLength password
    _ <- validatePasswordVariety password
    result <- validatePasswordEquality password passwordConfirmation
    return result

myIsNumber :: String -> Bool
myIsNumber str = all isDigit str

toNumber :: String -> Maybe Int
toNumber str = if (myIsNumber str) then Just (read str) else Nothing

validateYear :: String -> Validated DateError Int
validateYear str = case toNumber str of
    Just month -> Valid month
    Nothing -> Invalid (Singleton (YearIsNotAnInteger str))

validateMonthInteger :: String -> Validated DateError Int
validateMonthInteger str = case toNumber str of
    Just year -> Valid year
    Nothing -> Invalid (Singleton (MonthIsNotAnInteger str))

validateMonthRange :: Int -> Validated DateError Int
validateMonthRange month = if (month >= 1 && month <= 12) 
    then Valid month 
    else Invalid (Singleton (MonthOutOfRange month))

validateMonth :: String -> Validated DateError Int
validateMonth str = do
    month <- validateMonthInteger str
    _ <- validateMonthRange month
    return month

validateDayInteger :: String -> Validated DateError Int
validateDayInteger str = case toNumber str of
    Just day -> Valid day
    Nothing -> Invalid (Singleton (DayIsNotAnInteger str))

validateDayRange :: Int -> Validated DateError Int
validateDayRange day = if (day >= 1 && day <= 31) 
    then Valid day 
    else Invalid (Singleton (DayOutOfRange day))

validateDay :: String -> Validated DateError Int
validateDay str = do
    day <- validateDayInteger str
    _ <- validateDayRange day
    return day

validateDateHelper :: String -> String -> String -> Validated DateError Date
validateDateHelper yearStr monthStr dayStr = do
    year <- validateYear yearStr
    month <- validateMonth monthStr
    day <- validateDay dayStr
    return Date { day = day, month = month, year = year }

validateDateExists :: Date -> Validated DateError Date
validateDateExists = undefined

-- validateDate :: String -> String -> String -> Validated RegistrationFormError Date
-- validateDate yearStr monthStr dayStr = do
--     date <- validateDateHelper yearStr monthStr dayStr
--     _ <- validateDateExists date
--     return date
--     -- ...

validatePostalCode :: String -> Validated RegistrationFormError (Maybe String)
validatePostalCode = undefined

validate :: RegistrationForm -> Validated RegistrationFormError User
validate = undefined

myZip :: Validated e a -> Validated e b -> Validated e (a, b)
myZip (Valid x) (Valid y) = Valid (x, y)
myZip (Valid _) (Invalid e) = Invalid e
myZip (Invalid e) (Valid _) = Invalid e
myZip (Invalid e1) (Invalid e2) = Invalid (Append e1 e2)

myZipWith :: Validated e a -> Validated e b -> (a -> b -> c) -> Validated e c
myZipWith (Valid x) (Valid y) f = Valid (f x y)
myZipWith (Valid _) (Invalid e) _ = Invalid e
myZipWith (Invalid e) (Valid _) _ = Invalid e
myZipWith (Invalid e1) (Invalid e2) _ = Invalid (Append e1 e2)