import Data.List
import System.IO

numLines' n [] = []
numLines' n (x:xs) = (n, x) : numLines' (n+1) xs
numLines ls = numLines' 1 ls

allNumWords [] = []
allNumWords ((n, x) : xs) = zip (repeat n) (words x) ++ allNumWords xs

inverte [] = []
inverte ((n, x) : xs) = (x, n) : inverte xs

sortLs xs = inverte (sort (inverte xs))

almalgamate [(a, b)] = [(a, b)]
almalgamate ((n, x) : (a, b) : xs) =
  if x == b
    then almalgamate ((n ++ a, x) : xs)
    else (n, x) : almalgamate ((a, b) : xs)

aux [] = []
aux ((n, x) : xs) = ([n], x) : aux xs

shorten' [x] = [x] -- Retorna o último elemento da lista
shorten' (x : y : xs) =
  if x == y
    then shorten' (x : xs)
    else x : shorten' (y : xs)

shorten [] = []
shorten ((n, x) : xs) = (shorten' n, x) : shorten xs

imprimir [] = return ()
imprimir ((i, p) : xs) = do
  putStr (p ++ " ")
  putStrLn (show i)
  imprimir xs

main = do
  let texto = "a maca caiu da arvore e a maca foi comida\n e assim a maca desistiu da arvore e de ser bailarina"
  let linhas = numLines (lines texto)
  print "Linhas"
  print linhas -- Verifique o resultado de numLines
  let palavras = allNumWords linhas
  print "Palavras:"
  print palavras -- Verifique o resultado de allNumWords
  let ordenado = sortLs palavras
  print "Ordenado:"
  print ordenado -- Verifique o resultado de sortLs
  let amalgamado = almalgamate (aux ordenado)
  print "Ordenado:"
  print amalgamado -- Verifique o resultado de almalgamate
  let resposta = shorten amalgamado
  print "AMAAMAA:"
  imprimir resposta
