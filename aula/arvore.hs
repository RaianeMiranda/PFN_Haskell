import Data.List
import System.IO

type Word' = String
-- Criando tipo

data Tree = Node Word' [Int] Tree Tree | Leaf deriving Show
-- Construtor Node (String, Inteiro, left, right) | Vazio

--b
numLines' n [] = []
numLines' n (x:xs) = (n, x) : numLines' (n+1) xs -- recebe n=1 e a lista de todo o texto
--separa em uma dupla em número e primeira linha de palavras, recursiva para o resto somando n+1
numLines ls = numLines' 1 ls -- chama a aux e passa 1=n e todo o texto

--c
allNumWords [] = []
allNumWords ((n, x) : xs) = zip (repeat n) (words x) ++ allNumWords xs
-- repeat n = repetindo n até words x acabar
-- words quebra a linha a cada espaço
-- Recebe uma lista com um número e uma palavra
-- Utiliza a função zip para receber duas listas e agrupar em duplas (x:xs) (y:ys) = (x:y)

--d
insOrd x []= [x]
insOrd x (y:ys)= if x < y then x:(y:ys) else if x == y then (y:ys) else y : insOrd x ys
-- inserir de forma ordenada, se x< y então vira cabeça, se forem iguais ele não adiciona
-- se x for > y então o y assume cabeça e chamamos recursão para x e a cauda

--e
ins p l Leaf = Node p [l] Leaf Leaf
-- Se for vazio cria um nó com p l
ins p l (Node x ls left right)  | p == x = Node x (insOrd l ls) left right
                                | p < x = Node x ls (ins p l left) right
                                | otherwise = Node x ls left (ins p l right)

--f
mIndexTree' [] arv = arv
mIndexTree' ((l, p) : xs) arv = mIndexTree' xs (ins p l arv)
-- (ins p l arv) = nova arvore para chamada recursiva

mIndexTree x = mIndexTree' x Leaf 

-- Imprimir
imprimir Leaf = return ()
imprimir (Node p ls left right) = do imprimir left
                                     putStrLn (p ++ " " ++ show ls)
                                     -- imprime o nó atual
                                     imprimir right

-- Executando Main para imprimir todas as operações
main = do putStr " Nome do arquivo:"
          name<- getLine
          arq <- readFile name
          let resposta = mIndexTree(allNumWords(numLines(lines arq)))
          imprimir resposta