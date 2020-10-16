module ByteBeat where

import Control.Monad.Random


data ByteExp = Complement ByteExp
             | Mult ByteExp ByteExp
             | Div ByteExp ByteExp
             | Mod ByteExp ByteExp
             | Plus ByteExp ByteExp
             | Minus ByteExp ByteExp
             | ShiftLeft ByteExp ByteExp
             | ShiftRight ByteExp ByteExp
             | LessThanEq ByteExp ByteExp
             | Eq ByteExp ByteExp
             | NotEq ByteExp ByteExp
             | BitAnd ByteExp ByteExp
             | BitXOr ByteExp ByteExp
             | BitIOr ByteExp ByteExp
             | Tern ByteExp ByteExp ByteExp
             | Const Int
             | Time
             deriving (Eq,Show)

parens :: String -> String
parens s = "(" ++ s ++ ")"

binOpCompile :: String -> ByteExp -> ByteExp -> String
binOpCompile o b1 b2 = parens $ (compile b1) ++ o ++ (compile b2)

compile :: ByteExp -> String
compile (Complement b) = "~" ++ (compile b)
compile (Div b1 b2) = binOpCompile "/" b1 b2
compile (Mult b1 b2) = binOpCompile "*" b1 b2
compile (Mod b1 b2) = binOpCompile "%" b1 b2
compile (Plus b1 b2) = binOpCompile "+" b1 b2
compile (Minus b1 b2) = binOpCompile "-" b1 b2
compile (ShiftLeft b1 b2) = binOpCompile "<<" b1 b2
compile (ShiftRight b1 b2) = binOpCompile ">>" b1 b2
compile (LessThanEq b1 b2) = binOpCompile "<=" b1 b2
compile (Eq b1 b2) = binOpCompile "==" b1 b2
compile (NotEq b1 b2) = binOpCompile "!=" b1 b2
compile (BitAnd b1 b2) = binOpCompile "&" b1 b2
compile (BitIOr b1 b2) = binOpCompile "|" b1 b2
compile (BitXOr b1 b2) = binOpCompile "^" b1 b2
compile (Tern b1 b2 b3) = parens $ (compile b1) ++ " ? " ++ (compile b2) ++ " : " ++ compile b3
compile (Const i) = show i
compile Time = "t"

compileProg :: ByteExp -> String
compileProg b = "main()\n{\n int t = 0;\nfor(;;t++) putchar(" ++ (compile b) ++ ");\n}"

{-
main = do
  let testExp = BitAnd Time (ShiftRight Time (Const 8))
  putStrLn $ compileProg testExp
-}
