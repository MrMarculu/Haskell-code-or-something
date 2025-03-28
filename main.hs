import System.IO (hFlush, stdout, getChar, readIO)

data Mem = Mem [Int] Int [Int]
data Sign = Plus | Minus 
instance Show Sign where 
    show Plus = "+"
    show Minus = "-"

data AST = Move Sign | Sum Sign | While [AST] | Input | Output 

process :: String -> [AST]
process [] = []
process (c : str)
    | c == '>' = Move Plus : process str 
    | c == '<' = Move Minus : process str 
    | c == '+' = Sum Plus : process str 
    | c == '-' = Sum Minus : process str 
    | c == '[' = 
        let (loopstr, rest) = processLoop 1 str []
        in While (process loopstr) : process rest
    | c == ',' = Input : process str 
    | c == '.' = Output : process str 
    | otherwise = process str 
    where 
        processLoop :: Int -> String -> String -> (String,String) 
        processLoop 0 str acc = (reverse acc, str)
        processLoop n ('[': str) acc = processLoop (n+1) str acc 
        processLoop n (']':str) acc = processLoop (n-1) str acc 
        processLoop n (c:str) acc = processLoop n str (c:acc)
        processLoop _ [] _ = error "Unbalanced brackets"

newTape :: Mem 
newTape = Mem (repeat 0) 0 (repeat 0)

mvLeft :: Mem -> Mem 
mvLeft (Mem (l:ls) x rs ) = Mem ls l (x:rs)

mvRight :: Mem -> Mem 
mvRight (Mem ls x (r:rs)) = Mem (x:ls) r rs

add :: Mem -> Mem 
add (Mem ls x rs) = Mem ls (succ x) rs 

sub :: Mem -> Mem 
sub (Mem ls x rs) = Mem ls (pred x) rs 

change :: Mem -> Int -> Mem 
change (Mem ls x rs) nx = Mem ls nx rs 

pint :: Mem -> Int 
pint (Mem ls x rs) = x 

eval :: [AST] -> IO Mem
eval = eval' newTape
  where 
    eval' :: Mem -> [AST] -> IO Mem 
    eval' tape [] = return tape
    eval' tape (action:arr) =
      case action of
        Move Plus    -> eval' (mvRight tape) arr
        Move Minus   -> eval' (mvLeft tape) arr
        Sum Plus     -> eval' (add tape) arr
        Sum Minus    -> eval' (sub tape) arr
        Input        -> do
                          inp <- getChar
                          let val = fromEnum inp
                          if val > 255
                            then error "Only one byte is allowed!"
                            else eval' (change tape val) arr
        Output       -> do
                          putChar (toEnum (pint tape) :: Char)
                          eval' tape arr
        While body -> do
            if pint tape == 0
                then eval' tape arr 
                else do 
                    newtape <- eval' tape body
                    eval' newtape (While body : arr)
main :: IO ()
main = do 
    putStr "BF>  "
    hFlush stdout
    content <- getLine
    if content == ".quit"
        then putStrLn "exiting..."
        else do
            eval (process content)
            putStrLn ""
            return ()
