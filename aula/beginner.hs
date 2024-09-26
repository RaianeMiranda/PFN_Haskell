main = do --function main
    let name = "Dudlay" -- "Dudlay in storaged in "name" variable
    print ("Hello dear " ++ name) -- appear as ""Hello"
    putStrLn ("Hello "++ name ++" my deareast") -- appears as Hello... 
    -- ++ is concatanation
    list -- calling the function setted bellow "list"
    putStrLn ""
    print (cube 3)

scores = [97,83,100] -- this is a list of integers
--     [index 0, index 1, index 2]
list = do
    print (scores !! 0) --Will take any item that is in index 0 of the list
    -- print (head scores) only the first item
    -- print (last scores) only the last one
    -- print (init scores) print all of the list, expect the last one
    -- print (tail scores) print all of the list, expect the first one

cube num = num *num *num -- can be called alone

appendr x [] = [x] -- if list is null then the x value is append right inside it
appendr x (y:ys) = y : (appendr x ys) -- if list has already a value the x is append right in the end of the list
-- calling first the y of the list (head) then ys turns into y
-- keeps takin y until theres no item left, then puts x value into the list and closes it

appendl x [] = [x]
appendl x (y:ys) = x : (y:ys) 
-- starting list, calling first value x then the y:ys lis and closing it


attach [] x = x -- if there's only one list then just return the list, there's nothing to attach
attach (y:ys) x = y : (attach ys x) -- Call the whole first list, when it finished is going to bring the second list value then close
-- call y until y:ys ends then calls x list value

invert [] = [] -- if the list is null then nothing is called
invert (x:xs) = (invert xs) ++ [x] 
-- calling x out of the list until there's no body left
-- every time a head it taked out of the list it's placed on the end, creating a new order

-- [1,2,3]
-- x= 1 => (invert [2,3]) ++ 1
-- x= 2=> (invert [3]) ++ 2,1
-- x= 3=> (invert []) ++ 3,2,1
-- [3,2,1]

nprimeiros n []=[] -- se lista vazia, retorna vazia
nprimeiros 0 xs = [] -- se pedir 0 primeiros numeros de uma lista, retorna vazia
nprimeiros n (x:xs) = x: nprimeiros (n-1) xs
-- chama x e atribui na nova lista de n
-- ao ser chamado x recursivamente n tem que ser diminiudo por 1 para ser regressivo (4,3,2,1)
-- 3 [1,3,5,7,9]
-- 2 [3,5,7,9]
-- 1 [5,7,9]
-- 0 [7,9] => []
-- nova lista: [1,3,5]

--transformar string em inteiro
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

numString  s = numString' 1 (invert s)
numString' p[] = 0
numString' p (c:cs) = p*c2d c + numString' (p*10) cs



