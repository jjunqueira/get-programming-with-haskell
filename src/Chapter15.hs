--- Captstone Project
module Chapter15 where

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving(Show, Enum, Bounded)

data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving(Show, Enum, Bounded)

type Bits = [Bool]

data Rot = Rot

data OneTimePad = OTP String

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where halfAlphabet = alphabetSize `div` 2
        offset = fromEnum c + halfAlphabet
        rotation = offset `mod` alphabetSize

rotNDecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNDecoder n c = toEnum rotation
  where halfN = n `div` 2
        offset = if even n
                 then fromEnum c + halfN
                 else 1 + fromEnum c + halfN
        rotation = offset `mod` n

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

smallestCharNumber :: Int
smallestCharNumber = fromEnum (minBound :: Char)

rotChar :: Char -> Char
rotChar = rotN sizeOfAlphabet
  where sizeOfAlphabet = 1 + largestCharNumber

message :: [FourLetterAlphabet]
message = [L1, L3, L4, L1, L1, L2]

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta,Alpha, Kappa]

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder = map rot41
  where alphaSize = 1 + fromEnum(maxBound :: FourLetterAlphabet)
        rot41 = rotN alphaSize

threeLetterAlphabetEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterAlphabetEncoder = map rot31
  where alphaSize = 1 + fromEnum(maxBound :: ThreeLetterAlphabet)
        rot31 = rotN alphaSize

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder = map rot31decoder
  where alphaSize = 1 + fromEnum(maxBound :: ThreeLetterAlphabet)
        rot31decoder = rotNDecoder alphaSize

rotEncoder :: String -> String
rotEncoder = map rotChar
  where alphaSize = 1 + fromEnum (maxBound :: Char)
        rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder = map rotCharDecoder
  where alphaSize = 1 + fromEnum (maxBound :: Char)
        rotCharDecoder = rotNDecoder alphaSize

xorBool :: Bool -> Bool -> Bool
xorBool b1 b2 = (b1 || b2) && not (b1 && b2)

xorPair :: (Bool, Bool) -> Bool
xorPair (b1, b2) = xorBool b1 b2

xor :: [Bool] -> [Bool] -> [Bool]
xor bs1 bs2 = zipWith (curry xorPair) bs2 bs2

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if remainder == 0
               then False : intToBits' nextVal
               else True : intToBits' nextVal
  where remainder = n `mod` 2
        nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where reversedBits = reverse (intToBits' n)
        missingBits = maxBits - length reversedBits
        leadingFalses = replicate missingBits False

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2 ^ snd x) trueLocations)
  where size = length bits
        indices = [size-1, size-2 .. 0]
        trueLocations = filter fst
                        (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext =  map (\pair ->
                                 (fst pair) `xor` (snd pair))
                           (zip padBits plaintextBits)
 where padBits =  map charToBits pad
       plaintextBits =  map charToBits plaintext

applyOTP :: String -> String -> String
applyOTP pad plainText = map bitsToChar bitList
  where bitList = applyOTP' pad plainText

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text

instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])
