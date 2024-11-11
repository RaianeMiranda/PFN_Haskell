--Funções de Ordem superior

inc [] = []
inc (x:xs) = x+1:inc xs

dobra []=[]
dobra (x:xs)=x+2:dobra xs

map' n[]=[]
map' n(x:xs)= n x: map' n xs

