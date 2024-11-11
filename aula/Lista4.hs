import Data.List
import System.IO

numLines' n [] = []
numLines' n (x:xs) = (n, x) : numLines' (n+1) xs
numLines ls = numLines' 1 ls

allNumWords []= []
allNumWords ((n,x):xs)= zip (repeat n ) (words x) ++ allNumWords xs

inverte [] =[]
inverte ((n,x): xs) = (x,n) : inverte xs

sortLs xs = inverte (sort(inverte xs))

almalgamate [(a,b)] = [(a,b)]
almalgamate ((n,x):(a,b):xs)=if x == b then almalgamate ((n++a, x):xs) else (n,x): almalgamate ((a,b):xs)

aux []=[]
aux((n,x):xs)= ([n],x): aux xs

shorten' [x]=[]
shorten'(x:y:xs)= if x ==y then shorten' (x:xs) else x:shorten'(y:xs)

shorten []=[]
shorten ((n,x):xs)= (shorten' n, x): shorten xs

imprimir []= return ()
imprimir ((i,p): xs)= do putStr (p++" ")
                         putStrLn(show i)
                         imprimir xs

main = do putStr " Nome do arquivo:"
          name<- getLine
          arq <- readFile name
          let resposta = shorten(almalgamate(aux(sortLs(allNumWords(numLines(lines arq))))))
          imprimir resposta