-- 1.4) SomaPares
somaPares n | n<= 0  = 0
            |ehPar n = n + somaPares(n-2)
            |otherwise= somaPares (n-1)
             where ehPar n | n == 0 = True
                      | n == 1 = False
                      |otherwise = ehPar (n-2)

primo n | tamanho [x | x <-[1..n], mod n x == 0]==2 = True
        |otherwise = False
        where 
            tamanho []= 0
            tamanho (_:xs)= 1+ tamanho xs

foo [] ys = ys
foo (x:xs) ys = x: foo ys xs


ocorrencias n[]=0
ocorrencias n (x:xs) |n == x = 1 + ocorrencias n xs
                     |otherwise = ocorrencias n xs

todos [a]=a
todos (x:xs)|x==True=todos xs
            |otherwise= False

restante _ []=[]
restante n (x:xs)| n ==x =xs
                 | otherwise= restante n xs


inverteDupla [] =[]
inverteDupla ((x,y):xs) = (y,x): inverteDupla xs