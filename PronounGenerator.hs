module PronounGenerator where

import Control.Monad.Random
import Data.Char
import Data.List

-- import ByteBeat

{- we need to generate a starter to the pronoun
   the starter can either be Consonant-Vowel or needs to be

   vowel-consonant
-}


genVowel = equalList vowels

genSConsonant = equalList startConsonants

genEConsonant = equalList endConsonants

genEConsonantNoS = equalList $ filter (/= "s") endConsonants

{- generating bytebeat program from pronoun generation  -}

startConsonants = ["pl","pr","bl","br","tr","dr","kl",
               "kr","gl","gr","fl","fr","thr","fr",
               "sk","skr","sl","sm","sn","sp","spl","spr",
               "st","str","sw","tw","dw","kw","skw","gw","h",
               "s","d","x","g","y","xh","b","ps","cr","c","cs","z","zr","zh","j"]

vowels = ["a","e","i","o","u","ee","ei","ie","eh","ah","ou","ae","y"]

startingVowels = filter (/= "i") vowels

endConsonants = ["k","r","s","m","x","n","rn","mn"]

equalList = fromList . map (\x -> (x,1))


{-
Bad cases:
PSet {nominative = "e", accusative = "klen", pronom = "ek", predic = "klens", reflex = "klenselves"}
PSet {nominative = "o", accusative = "thrin", pronom = "or", predic = "thrins", reflex = "thrinself"}

Good cases:
PSet {nominative = "eer", accusative = "yax", pronom = "youn", predic = "yaxs", reflex = "yaxself"}



let's think for a bit about pronoun sets

there are five cases

nominative (she)
acccusative (her)
pronominal possessive (her)
predicative possessive (hers)
reflexive (herself)

reflexive is a modifier of either the accusative/pronominal/predicative
never the nominal

the reflexive is always one of those cases followed by self *or* selves

nominative is always either
(a) vowel
(b) vowel/consonant not ending in s
(c) consonant cluster/vowel

accusative is always either
(a) the nominative [but only if the nominative ends in a consonant]
(b) the nominative + consonant [if the nominative ends in a vowel, not ending in s]
(c) the nominative starting consonant + random vowel + consonant
(d) consonant + vowel + consonant [not ending in s]


pronominal is always either
(a) accusative
(b) the nominative + consonant [if the nominative ends in a vowell, CAN end in s]
(c) starting consonant of accusative + vowel + consonant [CAN end in s]

predicative is either
(a) the pronominal [+ s if the pronominal does not end in s]
(b) the accusative + s

reflexive
(a) accusative + self or selves
(b) predicative possessive + self or selves [removing intermmediate s]


-}

randCases :: MonadRandom m => [(m a, Rational)] -> m a
randCases ms = do
  m <- fromList ms
  m

randEqCases ms = randCases $ zip ms (repeat 1)

genNomVowel :: Rand StdGen [String]
genNomVowel = do
  v <- equalList startingVowels
  return $ [v]

genNomVowelCons = do
  v <- genNomVowel
  c <- genEConsonantNoS
  return $ v ++ [c]

genNomConsVowel = do
  c <- genSConsonant
  v <- genVowel
  return $ [c,  v]

genNominative = randEqCases [genNomVowel,genNomVowelCons, genNomConsVowel]

startVowel s = (head s) `elem` vowels
endConsonant s = (head $ reverse s) `elem` (startConsonants ++ endConsonants)
endVowel s = (head $ reverse s) `elem` vowels
startConsonant s = (head s) `elem` startConsonants

genAccNomVEnd n = do
  c <- genEConsonantNoS
  return $ n ++ [c]

genAccConVowlCon = do
  c1 <- genSConsonant
  v <- genVowel
  c2 <- genEConsonantNoS
  return $ [c1, v, c2]

genAccNomSC n = do
  v <- genVowel
  c <- genEConsonantNoS
  return $ [head n, v, c]

genAccusative nom = randEqCases (genAccNomCase
                                 ++ genAccNomVEndCase
                                 ++ genAccNomSCCase
                                 ++ [genAccConVowlCon])
  where genAccNomCase = if endConsonant nom then [return nom] else []
        genAccNomVEndCase = if endVowel nom then [genAccNomVEnd nom] else []
        genAccNomSCCase = if startConsonant nom then [genAccNomSC nom] else []

genProNom n = do
  c <- genEConsonant
  return $ n ++ [c]

genProAccSC a = do
  c <- genEConsonant
  v <- genVowel
  return $ [(head a),v,c]
    
genPronominal nom acc = randEqCases ([return acc]
                                     ++ genProNomCase
                                     ++ genProAccSCCase)
  where genProNomCase = if endVowel nom then [genProNom nom] else []
        genProAccSCCase = if startConsonant acc then [genProAccSC acc] else []

genPredPro p = if last p == "s" then return p else return (p ++ ["s"])

genPredAcc a = return (a ++ ["s"])

genPredicative pro acc = randEqCases [genPredPro pro,genPredAcc acc]

genReflAcc a = do
  suf <- equalList ["self","selves"]
  return $ a ++ [suf]

genReflPred p = do
  suf <- equalList ["self","selves"]
  if last p == "s"
    then return $ take (length p - 1) p ++ [suf]
    else return $ p ++ [suf]

genRefl acc pred = randEqCases [genReflAcc acc ,genReflPred pred]


data PronounSet = PSet {nominative :: String,
                        accusative :: String,
                        pronom :: String,
                        predic :: String,
                        reflex :: String} 
  deriving (Eq,Show)


pronounSetGenerator = do
  n <- genNominative
  a <- genAccusative n
  p1 <- genPronominal n a
  p2 <- genPredicative p1 a
  r <- genRefl a p2
  return $ PSet {nominative = concat n,
                 accusative = concat a,
                 pronom = concat p1,
                 predic = concat p2,
                 reflex = concat r}
{-
main = do
  p <- evalRandIO pronounSetGenerator
  print p
-}


{-
pronounStarter = do
  r <- getRandom
  if r < (0.5 :: Float)  then do
    s <- genSConsonant
    v <- genVowel
    return $ (s,s ++ v)
   else do
    v <- genVowel
    s <- genEConsonant
    return $ (v ++ s, v ++ s)


pronounSetGenerator = do
  (objstart,subj) <- pronounStarter
  objandPosVowel <- genVowel
  objEnd <- genEConsonant
  posEnd <- genEConsonant
  if objEnd == posEnd
    then pronounSetGenerator
    else return (subj,
                 objstart ++ objandPosVowel ++ objEnd,
                 objstart ++ objandPosVowel ++ posEnd)

caps s = (toUpper $ head s) : (tail s) 
  
sentenceTemplate s o p r = (caps s) ++ " walked to the store. " ++
                           "I saw " ++ o ++ " licking the produce. " ++
                           "It was not " ++ p ++ " produce to lick. " ++
                           (caps s) ++ " should be ashamed of " ++ r ++ "!"

sentenceTemplate2 s o p r = do
  v1 <- equalList verb1past
  v2 <- equalList verb2ing
  n1 <- equalList noun1
  p1 <- equalList place1
  v3 <- equalList verb3ed
  v4 <- equalList verb4ed

  return $ unwords ["I",v1,o++".",caps s,"was",v2,p,n1,"at the",p1++".","We",v3,"and",s,v4,r++"."]

verb1past = ["saw","spotted", "spied on", "ran into", "summoned"]

verb2ing = ["smoking","poking","eating","shopping for"]

noun1 = ["pet skunk","bag of quarters","favorite YouTuber","bar of soap"]
           
place1 = ["grocery", "chicken house", "dog cafe", "catsup emporium"]

verb3ed = ["talked", "danced", "drew occultic symbols", "snorted chalk dust"]

verb4ed = ["overshared about", "praised", "got really down on", "felt better about"]

main = do
  (sub,obj,pos) <- evalRandIO pronounSetGenerator
  putStrLn $ "Subjective: " ++ sub
  putStrLn $ "Objective: " ++ obj
  putStrLn $ "Possessive: " ++ pos
  let refl = if last obj == 's' then obj ++ "elf" else obj ++ "self"
  putStrLn $ "Reflexive: " ++ refl
  putStrLn $ "Example:\n"
  s <- evalRandIO $ sentenceTemplate2 sub obj pos refl
  putStrLn s
-}
