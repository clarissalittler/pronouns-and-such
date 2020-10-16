import PronounGenerator
import ByteBeat
import Control.Monad.Random
import Control.Monad.Reader
import Data.Char
import Data.List

she :: PronounSet
she = PSet "she" "her" "her" "hers" "herself"

he :: PronounSet
he = PSet "he" "him" "his" "his" "himself"

they :: PronounSet
they = PSet "they" "them" "their" "theirs" "themself"

fae :: PronounSet
fae = PSet "fae" "faer" "faer" "faers" "faerself"

zie :: PronounSet
zie = PSet "zie" "hir" "hir" "hirs" "hirself"

xie :: PronounSet
xie = PSet "xie" "xer" "xer" "xers" "xerself"

-- first try is that we take a set of pronouns
-- and then generate a bytebeat program entirely from iterating over the strings smashed together
-- so how would that work? For every character we'd choose either a operator or a constant
-- and let "empty" holes then be the Time variable
genbeats :: PronounSet -> ByteExp
genbeats (PSet n a p1 p2 r) = genbeatString (n ++ a ++ p1 ++ p2 ++ r)

takeHalf l = take (length l `div` 2) l
dropHalf l = drop (length l `div` 2) l

gbsBinOp c s = c (genbeatString (takeHalf s)) (genbeatString (dropHalf s))


totalLength (PSet n a p1 p2 r) = length $ n ++ a ++ p1 ++ p2 ++ r

vowelString = "aeiouy"

consonantString = "bcdfghjklmnpqrstvwxyz"

numvowels :: String -> Int
numvowels = length . filter (\c -> c `elem` vowelString) 

numconsonants :: String -> Int
numconsonants = length . filter (\c -> c `elem` consonantString)

sumAsNumbers :: String -> Int
sumAsNumbers = foldr (\c sm -> (ord c) + sm) 0

-- so what we're going to do is choose between a few possibilities from basic sonic elements
-- that are interesting
-- the first are sierpinski rhythms
-- to generate sierpinski rhythms is to do a polynomial of
-- \| and ^ s based on the properties of the pronouns
{-
test idea:
if the nominative starts with a vowel then have a percussive -1 at the end
then for the total polynomial we can do something like

(exp1 op1 exp2 op2 exp3 op3 exp4 op4 exp5)&(total length if total length odd) - 1 [if nominative starts with vowel]

op1 -> op4 are either | or ^, they're | if the number of vowels in exp1 + exp2 is even otherwise ^

expn = t*[2 for each vowel, 3 for each consonant]&t>>[sum of letters as numbers % 9 + 2]   


-}

primes = [2,3,5,7,11,13,17,19,23,29,31]


genbeat4 :: PronounSet -> ByteExp
genbeat4 p@(PSet n a p1 p2 r) = Minus (Mult multFact (BitAnd ((((exp1 `op1` exp2) `op2` exp3) `op3` exp4) `op4` exp5) andFact)) minusFact
  where minusFact = if head n `elem` vowelString then Const 1 else Const 0
        {- andFact   | totalLength p `mod` 2 == 0 = Const 255
                  | numvowels (n ++ a ++ p1 ++ p2 ++ r) > 5 = Const (totalLength p)
                  | otherwise = Const $ sumAsNumbers (n ++ a ++ p1 ++ p2 ++ r) `mod` 255 -}
        andFact = Const $ sumAsNumbers (n ++ a ++ p1 ++ p2 ++ r) `mod` 255
        opMaker s1 s2 | numvowels (s1++s2) `mod` 2 == 0 = BitIOr
                      | s1 /= s2 = BitXOr
                      | otherwise = \e1 e2 -> Div e1 (Plus (ShiftRight e2 (Const (length s2))) (Const 2))
        op1 = opMaker n a
        op2 = opMaker a p1
        op3 = opMaker p1 p2
        op4 = opMaker p2 r
        multFact = if numvowels (n ++ a ++ p1 ++ p2 ++ r) > 10 then Time else Const 1
        aux c sm = (if c `elem` vowelString then 1 else 2) + sm
        expMaker s = BitAnd (Mult Time (Const $ primes !! ((foldr aux 0 s) `mod` 7))) (ShiftRight Time (Const $ (((sumAsNumbers s) `mod` 10) + 3)))
        exp1 = expMaker n
        exp2 = expMaker (n++a)
        exp3 = expMaker (n++a++p1)
        exp4 = expMaker (n++a++p1++p2)
        exp5 = expMaker r
        

genbeat3 :: PronounSet -> ByteExp
genbeat3 p@(PSet n a p1 p2 r) = if (length n `mod` 2 == 0)
                                then Minus genbeat3' (Const $ (totalLength p) `mod` 4)
                                else genbeat3'
  where genbeat3Nom e = if take 1 n `elem` vowels
                        then Mult Time e
                        else Mult (genbeatString n) e 
        genbeat3Acc e = if n == a
                        then BitAnd Time (ShiftRight Time e)
                        else BitAnd (genbeatString a) e
        genbeat3Pro e = if (length p1) < 4 then BitXOr (Const (totalLength p)) e else BitIOr (Const (totalLength p)) e 
        genbeat3Pred e = Div (Mult e (genbeatString p2)) (Const $ length p2)
        genbeat3Refl e = BitXOr Time (Const $ length r)
        genbeat3' = genbeat3Nom $ genbeat3Acc $ genbeat3Pro $ genbeat3Pred $ genbeat3Refl $ Time


genbeatString2 :: String -> Reader PronounSet ByteExp
genbeatString2 [] = return Time
genbeatString2 ('a':s) = return $ gbsBinOp Mult s
genbeatString2 ('b':s) = return $ gbsBinOp Mod s
genbeatString2 ('c':s) = return $ gbsBinOp Plus s
genbeatString2 ('d':s) | length s < 2 = do
                         n <-  asks nominative
                         e <- genbeatString2 s
                         return $ Minus e (Const $ length n)
                       | otherwise = return $ gbsBinOp Minus s
genbeatString2 ('e':s) = return undefined
genbeatString2 ('f':s) = return undefined
genbeatString2 ('g':s) = return undefined
genbeatString2 ('h':s) = return undefined
genbeatString2 ('i':s) = return undefined
genbeatString2 ('j':s) = return undefined
genbeatString2 ('k':s) = return undefined
genbeatString2 ('l':s) = return undefined
genbeatString2 ('m':s) = return undefined
genbeatString2 ('n':s) = return undefined
genbeatString2 ('o':s) = return undefined
genbeatString2 ('p':s) = return undefined
genbeatString2 ('q':s) = return undefined
genbeatString2 ('r':s) = return undefined
genbeatString2 ('s':s) = return undefined
genbeatString2 ('t':s) = return undefined
genbeatString2 ('u':s) = return undefined
genbeatString2 ('v':s) = return undefined
genbeatString2 ('w':s) = return undefined
genbeatString2 ('x':s) = return undefined
genbeatString2 ('y':s) = return undefined
genbeatString2 ('z':s) = return undefined

genbeatString :: String -> ByteExp
genbeatString [] = Time
genbeatString ('a':s) = gbsBinOp Mult s 
genbeatString ('b':s) = gbsBinOp Mod s
genbeatString ('c':s) = gbsBinOp Plus s
genbeatString ('d':s) = gbsBinOp Minus s
genbeatString ('e':s) = ShiftRight (genbeatString s) (Const $ (length s `mod` 7) +2)
genbeatString ('f':s) = gbsBinOp BitAnd s
genbeatString ('g':s) = gbsBinOp BitXOr s
genbeatString ('h':s) = gbsBinOp BitIOr s
genbeatString ('i':s) = Tern (genbeatString $ take 3 s) (genbeatString (takeHalf $ drop 3 s)) (genbeatString (dropHalf $ drop 3 s))
genbeatString ('j':s) = Complement (genbeatString s)
genbeatString ('k':s) = Mult (Const $ (length s) + 2) (genbeatString s)
genbeatString ('l':s) = Mod (genbeatString s) (Const $ (length s) + 5)
genbeatString ('m':s) = Plus (genbeatString s) (Const $ (length s) + 2)
genbeatString ('n':s) = Minus (genbeatString s) (Const $ (length s) +2)
genbeatString ('o':s) = ShiftRight (genbeatString s) (Const $ (length s `mod` 7) + 2)
genbeatString ('p':s) = Tern (gbsBinOp LessThanEq $ take 6 s)
                           (genbeatString $ takeHalf $ drop 6 s)
                           (genbeatString $ dropHalf $ drop 6 s)
genbeatString ('q':s) = Tern (gbsBinOp Eq $ take 6 s)
                           (genbeatString $ takeHalf $ drop 6 s)
                           (genbeatString $ dropHalf $ drop 6 s)
genbeatString ('r':s) = BitAnd (Const (length s)) (genbeatString s)
genbeatString ('s':s) = BitXOr (Const (length s)) (genbeatString s)
genbeatString ('t':s) = BitIOr (Const (length s)) (genbeatString s)
genbeatString ('u':s) = Mult (genbeatString $ take (length s `div` 3) s)
                           (genbeatString $ drop (length s `div` 3) s)
genbeatString ('v':s) = Plus (genbeatString $ take (length s `div` 3) s)
                           (genbeatString $ drop (length s `div` 3) s)
genbeatString ('w':s) = Plus (genbeatString $ take (length s `div` 3) s)
                           (genbeatString $ drop (length s `div` 3) s)
genbeatString ('x':s) = Minus (genbeatString $ take (length s `div` 3) s)
                            (genbeatString $ drop (length s `div` 3) s)
genbeatString ('y':s) = BitAnd (genbeatString $ take (length s `div` 3) s)
                             (genbeatString $ drop (length s `div` 3) s)
genbeatString ('z':s) = BitXOr (genbeatString $ take (length s `div` 3) s)
                             (genbeatString $ drop (length s `div` 3) s)

pronounCompileAndPrint p = do
  putStrLn "//Pronouns:"
  putStrLn $ "//Nominative: " ++ (nominative p)
  putStrLn $ "//Accusative: " ++ (accusative p)
  putStrLn $ "//Pronominal: " ++ (pronom p)
  putStrLn $ "//Predicative: " ++ (predic p)
  putStrLn $ "//Reflexive: " ++ (reflex p)
  putStrLn "\n//Code:"
  putStrLn $ compileProg $ genbeat4 p

main = do
  --p <- evalRandIO pronounSetGenerator
  --pronounCompileAndPrint p
  pronounCompileAndPrint xie
