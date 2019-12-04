#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [ containers ])"

module SecureContainer where

import Text.ParserCombinators.ReadP

type Digit = Int
type Length = Int
type Nr = Int
type Password = [Digit]
type Bound = Password
type Bounds = (Bound, Bound)

interval :: String -> Bounds
interval = fst . head . readP_to_S (do
  low <- digits
  char '-'
  high <- digits
  (char '\n' >> return ()) +++ eof
  return (low, high)
  )
  where
    digits = many (read . (:[]) <$> satisfy (`elem` "0123456789"))

tightenUp :: Bound -> Bound
tightenUp bs = foldr (\d more s -> case s of
                       Left highDigit
                         | d < highDigit -> highDigit:more (Right highDigit)
                         | otherwise     -> d:more (Left d)
                       Right x -> x:more s
                     )
                     (const [])
                     bs
                     (Left 0)

tightenDown :: Bound -> Bound
tightenDown bs = either id id $
  foldr (\d more s -> case s of
          Left highDigit
            | d < highDigit -> case more (Right 9) of
              Left  _ -> error "Everything should be Right"
              Right ds -> Right (9:ds)
            | otherwise     -> case more (Left d) of
              Left ds -> Left (d:ds)
              Right ds -> Left ((d-1):ds)
          Right 9 -> case more s of
            Left []  -> Right [9]
            Right ds -> Right (9:ds)
        )
        (const (Left []))
        bs
        (Left 0)

tighten :: Bounds -> Bounds
tighten (low, high) = (tightenUp low, tightenDown high)

nrMonotone :: Digit -> Length -> Digit -> Nr
nrMonotone nine 1 lowerBound = (nine + 1) - lowerBound
nrMonotone nine l lowerBound =
  sum (map (nrMonotone nine (l - 1)) [lowerBound..nine])

nrPasswords :: Digit -> Length -> Digit -> Nr
nrPasswords _ l _ | l < 2 = error "length < 2 -> impossible to repeat a digit"
nrPasswords _ 2 firstDigit = 1
nrPasswords nine length firstDigit = nrMonotone nine (length - 2) firstDigit
  + sum (map (nrPasswords nine (length - 1)) [firstDigit + 1..nine])

boundedPasswords :: Bounds -> Nr
boundedPasswords (low, high) = lowNr + midNrs + highNr
  where
    l = length low
    lowFst = head low
    lowSnd = head (tail low)
    lowMono | lowFst == lowSnd = nrMonotone 9 (l - 2) lowFst
            | otherwise = 0
    lowNr = lowMono + sum (map (nrPasswords 9 (l - 1)) [lowSnd..9])
    midNrs = sum (map (nrPasswords 9 l) [head low + 1..head high - 1])
    highFst = head high
    highSnd = head (tail high)
    highMono | highFst == highSnd = nrMonotone highFst (l - 2) highFst
             | otherwise = 0
    highNr = highMono + sum (map (nrPasswords 9 (l - 1)) [highFst..highSnd - 1])
      + 1

monoIncrement :: Password -> Password
monoIncrement p = either (error "Can't increment past all 9's") id $
  foldr (\d more s -> case more (if d < 9 then Just d else s) of
          Right ds -> Right (d:ds)
          Left ds | d == 9 -> case s of
                    Just d' -> Left (d' + 1:ds)
                    Nothing -> error "Can't increment past all 9's"
                  | otherwise -> Right (d + 1:ds)
        )
        (const (Left []))
        p
        Nothing

monotone :: Bounds -> [Password]
monotone (low, high) | low <= high = low:monotone (monoIncrement low, high)
                     | otherwise  = []

incFirst :: [Int] -> [Int]
incFirst (x:xs) = x + 1:xs

repetitions :: Password -> [Length]
repetitions p = foldr (\d more s -> case s of
                        Just d' | d == d' -> incFirst (more s)
                                | otherwise -> 1:more (Just d)
                        Nothing -> more (Just d)
                      )
                      (\s -> case s of
                        Just _ -> [1]
                        Nothing -> []
                      )
                      p
                      Nothing

aRepetition :: Password -> Bool
aRepetition = any (>= 2) . repetitions

lonePair :: Password -> Bool
lonePair = any (== 2) . repetitions

boundedPasswords' :: Bounds -> Nr
boundedPasswords' = length . filter lonePair . monotone

boundedPasswordsExhaustive :: Bounds -> [Password]
boundedPasswordsExhaustive = filter aRepetition . monotone

main :: IO ()
main = do
  bounds <- interval <$> readFile "input.txt"
  -- let passwords = boundedPasswordsExhaustive (tighten bounds)
  putStrLn . ("Nr of different passwords: " <>) . show $
    boundedPasswords (tighten bounds)
    -- length passwords
  putStrLn . ("Nr of passwords given one more important detail: " <>) . show $
    boundedPasswords' (tighten bounds)
    -- length (filter lonePair passwords)
