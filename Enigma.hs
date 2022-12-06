{-- Stub for the grading assignment. Fill it in, making sure you use good
 -- functional style, and add comments (including replacing those that are
 -- already here).
--}

module Enigma where
  import Data.Char  -- to use functions on characters
  import Data.Maybe -- breakEnigma uses Maybe type
  -- add extra imports if needed, but only standard library functions!
  import Debug.Trace

{- Part 1: Simulation of the Enigma -}

  type Rotor = ([Char], Int) 
  type Reflector = [(Char, Char)]
  type Offsets = (Int, Int, Int)
  type Stecker = [(Char, Char)]
  abc = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  
  
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
                | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Stecker

  {- Encodes the message
  1) Checks for empty input
  2) Cleans the current input character to check for invalid characters
  3) Performs rotor shifts
  4) Encodes character in first pass
  5) Reflects the character
  6) Passes character back through rotors
  7) Encodes other characters
  8) Returns total result
  -}
  encodeMessage :: String -> Enigma -> String
  encodeMessage [] _ = []
  encodeMessage (x:xs) (SimpleEnigma r1 r2 r3 reflec (off1, off2, off3)) = 
      if isLetter x then
       [encodedLetter] ++ encodeMessage xs (SimpleEnigma r1 r2 r3 reflec (newOff1, newOff2, newOff3))
      else
       encodeMessage xs (SimpleEnigma r1 r2 r3 reflec (off1, off2, off3))
      where [newOff1, newOff2, newOff3] = rotateRotors r1 r2 r3 [off1, off2, off3]
            firstPass = (passLeft r1 (passLeft r2 (passLeft r3 (toUpper x) newOff3) newOff2) newOff1)
            reflected = reflectorFunction firstPass reflec
            encodedLetter = (passRight r3 (passRight r2 (passRight r1 reflected newOff1) newOff2) newOff3)
  encodeMessage (x:xs) (SteckeredEnigma r1 r2 r3 reflec (off1, off2, off3) steck) = 
      if isLetter x then
       [steckeredOutput] ++ encodeMessage xs (SteckeredEnigma r1 r2 r3 reflec (newOff1, newOff2, newOff3) steck)
      else
       encodeMessage xs (SteckeredEnigma r1 r2 r3 reflec (off1, off2, off3) steck)
      where [newOff1, newOff2, newOff3] = rotateRotors r1 r2 r3 [off1, off2, off3]
            steckeredInput = steckerPass (toUpper x) steck
            firstPass = (passLeft r1 (passLeft r2 (passLeft r3 steckeredInput newOff3) newOff2) newOff1)
            reflected = reflectorFunction firstPass reflec
            encodedLetter = (passRight r3 (passRight r2 (passRight r1 reflected newOff1) newOff2) newOff3)
            steckeredOutput = steckerPass encodedLetter steck


  --Reflects the input given based upon the reflector
  reflectorFunction :: Char -> Reflector -> Char
  reflectorFunction x ((y, z):ys)
    | x == y = z
    | x == z = y
    | otherwise = reflectorFunction x ys

  --Steckers the input
  steckerPass :: Char -> Stecker -> Char
  steckerPass x ((y,z): [])
    | x == y = z
    | x == z = y
    | otherwise = x
  steckerPass x ((y,z): ys)
    | x == y = z
    | x == z = y
    | otherwise = steckerPass x ys

  --Converts a position of a character to its character in the alphabet
  int2let :: Int -> Char
  int2let n = chr (ord 'A' + n)

  --Moves the letter input forwards in the alphabet acting as the rotor shift
  shiftInput :: Int -> Char -> Char
  shiftInput 0 x = x
  shiftInput n c  = int2let ((alphaPos c + n) `mod` 26)

  --Moves the letter input back in the alphabet reversing the shift ready for the next rotor
  unShiftInput :: Int -> Char -> Char
  unShiftInput 0 x = x
  unShiftInput n c = int2let ((alphaPos c - n) `mod` 26)

  --Passes through rotor.
  --Starts by shifting the input along the alphabet till it reaches the correct offset
  --Letter is then converted to an int in order to search for that index in the rotor
  --Index of the rotor element is found and Char is returned
  --Char is unshifted ready for output into the next rotor
  passLeft :: Rotor -> Char -> Int -> Char
  passLeft (roto,_) ch shift = unShiftInput shift (roto!!(alphaPos (shiftInput shift ch)))

  passRight :: Rotor -> Char -> Int -> Char
  passRight (roto,_) ch shift = unShiftInput shift (abc!!(findLetterPosition roto (shiftInput shift ch) 0))

  findLetterPosition :: [Char] -> Char -> Int -> Int
  findLetterPosition (x:xs) letter count = if x == letter then count else findLetterPosition xs letter (count+1)

  --Rotates the provided rotors by 1
  --Compares each rotors pin position with the next value
  --If value is met it turns the next rotor
  --Returns the new offsets
  rotateRotors :: Rotor -> Rotor -> Rotor -> [Int] -> [Int]
  rotateRotors _ _ _ [] = []
  rotateRotors (_,_) (_,r2KnockOn) (_,r3KnockOn) [r1Off, r2Off, r3Off] = 
         if r3KnockOn == (r3Off + 1) then
          if r2KnockOn == (r2Off + 1) then
            [(r1Off + 1) `mod` 26, (r2Off + 1) `mod` 26, (r3Off + 1) `mod` 26]
          else
            [r1Off, (r2Off + 1) `mod` 26, (r3Off + 1) `mod` 26]
         else
          [r1Off, r2Off, (r3Off + 1) `mod` 26]


{- Part 2: Finding the Longest Menu -}

  type Menu = [Int] -- the supplied type is not correct; fix it!
  type Crib = [(Char, Char)] -- the supplied type is not correct; fix it!

  
  longestMenu :: Crib -> Menu
  longestMenu [] = []
  longestMenu startingCrib = findLongestMenu startingCrib 0

  findLongestMenu :: Crib -> Int -> Menu
  findLongestMenu chosenCrib count = 
    if (count == ((length chosenCrib) - 1)) then
        generateNextStep chosenCrib count
    else
      if ((length currentMenu) > (length nextLongestMenu)) then
        currentMenu
      else
        nextLongestMenu
    where nextLongestMenu = findLongestMenu chosenCrib (count+1)
          currentMenu = (generateNextStep chosenCrib count)


  generateNextStep :: Crib -> Int -> Menu
  generateNextStep currentCrib (-1) = []
  generateNextStep currentCrib currentPosition = 
    if ((moreCharacters currentCrib) || ((length nextPositions) == 0)) then 
      if ((length nextPositions) > 1) then
        [currentPosition] ++ findBestPath newCrib nextPositions
      else
        [currentPosition] ++ (generateNextStep newCrib (extractData nextPositions))
    else
      []
    where newCrib = removeCurrentCharacter currentCrib currentPosition 0
          nextPositions = findNextLetter (findCipheredValue currentPosition currentCrib 0) newCrib 0

  extractData :: [Int] -> Int
  extractData [] = -1
  extractData listOfInts = head listOfInts

  findBestPath :: Crib -> [Int] -> Menu
  findBestPath currentCrib (x:[]) = generateNextStep currentCrib x
  findBestPath currentCrib (x:xs) =
    if (length calculatedPath) > (length previousBestCalculatedPath) then
      calculatedPath
    else
      previousBestCalculatedPath
    where calculatedPath =  generateNextStep currentCrib x
          previousBestCalculatedPath = findBestPath currentCrib xs

  findCipheredValue :: Int -> Crib -> Int -> Char
  findCipheredValue pos ((_,x): xs) count
    | pos == count = x 
    | otherwise = findCipheredValue pos xs (count+1)

  moreCharacters :: Crib -> Bool
  moreCharacters ((currentCrib, _):[]) = if (isLetter currentCrib) == True then True else False
  moreCharacters ((currentCrib, _):xs) = if (isLetter currentCrib) == True then True else moreCharacters xs

  findNextLetter :: Char -> Crib -> Int -> [Int]
  findNextLetter x ((y,_): []) count
    | x == y = [count]
    | otherwise = []
  findNextLetter x ((y,_): xs) count
    | x == y = [count] ++ findNextLetter x xs (count+1)
    | otherwise = findNextLetter x xs (count+1)

  removeCurrentCharacter :: Crib -> Int -> Int -> Crib
  removeCurrentCharacter ((y,z): xs) position count
    | count == position = (('#', '#'): xs)
    | otherwise = [(y,z)] ++ removeCurrentCharacter xs position (count+1)


{- Part 3: Simulating the Bombe -}
  
  breakEnigma :: Crib -> Maybe (Offsets, Stecker)
  breakEnigma _ = Nothing

{- Useful definitions and functions -}

   -- substitution cyphers for the Enigma rotors
   -- as pairs of (wirings, knock-on position)
   -- knock-on position is where it will cause the next left wheel to
   -- advance when it moves past this position
 
        --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  rotor1=("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)
  rotor2=("AJDKSIRUXBLHWTMCQGZNPYFVOE",5::Int)
  rotor3=("BDFHJLCPRTXVZNYEIWGAKMUSQO",22::Int)
  rotor4=("ESOVPZJAYQUIRHXLNFTGKDCMWB",10::Int)
  rotor5=("VZBRGITYUPSDNHLXAWMJQOFECK",0::Int)

  {- the standard Enigma reflector (Reflector B)
    swapped A<->Y, B<->R, C<->U,D<->H, E<->Q, F<->S, G<->L, 
            I<->P, J<->X, K<->N, M<->O, T<->Z,V<->W
  -}
  reflectorB= [('A','Y'),
              ('B','R'),
              ('C','U'),
              ('D','H'),
              ('E','Q'),
              ('F','S'),
              ('G','L'),
              ('I','P'),
              ('J','X'),
              ('K','N'),
              ('M','O'),
              ('T','Z'),
              ('V','W')]

  {- alphaPos: given an uppercase letter, returns its index in the alphabet
     ('A' = position 0; 'Z' = position 25)
   -}
  alphaPos :: Char -> Int
  alphaPos c = (ord c) - ord 'A'

  debug = flip trace