-- Questão 1
pertence n []= False
pertence n (x:xs) = if n == x then True else pertence n xs

-- Questão 2
intercessao [] _ = []  -- Se a primeira lista estiver vazia, não há interseção
intercessao (x:xs) ys = if verifica x ys then x : intercessao xs ys else intercessao xs ys

verifica _ [] = False  -- Se a lista estiver vazia, não pode conter o elemento
verifica e (y:ys) = if e == y then True else verifica e ys

-- Questão 3
inverso [] = []
inverso (x:xs)= (inverso xs) ++ [x]

-- Questão 4
nprimeiros n []=[] 
nprimeiros 0 xs = [] 
nprimeiros n (x:xs) = x: nprimeiros (n-1) xs

nUltimos n []=[] 
nUltimos 0 xs = [] 
nUltimos n (x:xs) = inverso (nprimeiros (n) (inverso (x:xs)))

-- Questão 5
soma2 [] _ = []  
soma2 _ [] = []  
soma2 (x:xs) (y:ys) = (x + y) : soma2 xs ys 

--Questão 6
pot2 n = auxPot2 1 n

auxPot2 e n = if e > n then [] else pot e : auxPot2 (e + 1) n
pot 0 = 1
pot e = 2 * pot (e - 1)

-- Qustão 7
intercalacao x [] = x
intercalacao [] x = x
intercalacao (x:xs) (y:ys) = if x < y then x : (intercalacao xs (y:ys)) else y : (intercalacao ys (x:xs))

--Questão 8
menor [a] = a
menor (x:xs) = if x < (menor xs) then x else menor xs

--Questão 9 
removerElem x [] = []
removerElem x (y:ys) = if x == y then ys else y:removerElem x ys -- Comparar x com y, se y == x então retorna o corpo, caso contrário mantem o y e testa x com ys

--Questão 10
ordenar [x]= [x]
ordenar (x:xs) = menor (x:xs) : (ordenar(removerElem (menor (x:xs)) (x:xs))) 

--Questão 11
ins x []= [x]
ins x (y:ys)= if x < y then x:(y:ys) else if x == y then (y:ys) else y : ins x ys

-- Questão 12
enesimo 1 (y:ys)= y
enesimo x (y:ys) = enesimo (x-1) ys

-- Questão 13
repetir 0 e = []
repetir n e = e : repetir (n-1) e

--Questão 14
d2c 0 = '0'
d2c 1 = '1'
d2c 2 = '2'
d2c 3 = '3'
d2c 4 = '4'
d2c 5 = '5'
d2c 6 = '6'
d2c 7 = '7'
d2c 8 = '8'
d2c 9 = '9'

numString 0 = "0"
numString n = inverso (numString' n)

numString' 0 = []
numString' n = d2c (mod n 10) : numString' (div n 10)

--Questão 15
c2d '0' = 0
c2d '1' = 1
c2d '2' = 2
c2d '3' = 3
c2d '4' = 4
c2d '5' = 5
c2d '6' = 6
c2d '7' = 7
c2d '8' = 8
c2d '9' = 9

stringNum s = stringNum' 0 s
stringNum' n [] = n  
stringNum' n (c:cs) = stringNum' (n * 10 + c2d c) cs

--Questão 16
c4d "0" = 0
c4d "1" = 1
c4d "10" = 2
c4d "11" = 3
c4d "100" = 4
c4d "101" = 5
c4d "110" = 6
c4d "111" = 7
c4d "1000" = 8
c4d "1001" = 9
c4d "1010" = 10
c4d "1011" = 11
c4d "1100" = 12
c4d "1101" = 13
c4d "1110" = 14
c4d "1111" = 15

bin2int [] = 0 
bin2int s = bin2int' s 0 

bin2int' [] _ = 0  
bin2int' cs exp = bin2int' (init cs) (exp + 1) + c4d [last cs] * (potencia 2 exp)

potencia _ 0 = 1  
potencia b e = b * potencia b (e - 1)  

-- Questão 17
c5d 0 = "0"
c5d 1 = "1"
c5d 2 = "10"
c5d 3 = "11"
c5d 4 = "100"
c5d 5 = "101"
c5d 6 = "110"
c5d 7 = "111"
c5d 8 = "1000"
c5d 9 = "1001"

int2bin 0 = "0"
int2bin n = int2bin' n

int2bin' 0 = ""
int2bin' n = int2bin' (div n 2) ++ c5d (mod n 2)

--Questão 18

c6d 'A' = "a"
c6d 'B' = "b"
c6d 'C' = "c"
c6d 'D' = "d"
c6d 'E' = "e"
c6d 'F' = "f"
c6d 'G' = "g"
c6d 'H' = "h"
c6d 'I' = "i"
c6d 'J' = "j"
c6d 'K' = "k"
c6d 'L' = "l"
c6d 'M' = "m"
c6d 'N' = "n"
c6d 'O' = "o"
c6d 'P' = "p"
c6d 'Q' = "q"
c6d 'R' = "r"
c6d 'S' = "s"
c6d 'T' = "t"
c6d 'U' = "u"
c6d 'V' = "v"
c6d 'W' = "w"
c6d 'X' = "x"
c6d 'Y' = "y"
c6d 'Z' = "z"
c6d x = [x] 

minusculas [] = [] 
minusculas (x:xs) = c6d x ++ minusculas xs