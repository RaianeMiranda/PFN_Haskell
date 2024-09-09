-- 1) FORMAR TRIÂNGULO 
-- Soma de dois lados de um triângulo deve sempre ser maior que o terceiro lado

ehTriangulo a b c = a+b>c && b+c>a && c+a>b

-- 2) TIPO DE TRIÂNGULO

tipoTriangulo a b c = if a == b && b == c then "Equilatero" else if a ==b || b==c || c== a then "Isoceles" else "Escaleno"

-- 3) CONFIMAR TRIÂNGULO E RESPONDER TIPO (USAR FUNÇÕES JÁ DEFINIDAS)

nao = "nao eh triangulo"

triangulo a b c = if ehTriangulo a b c then tipoTriangulo a b c else nao

-- 4) NÚMEROS PARES

ehPar 0 = True  
ehPar 1 = False 
ehPar n = ehPar (n - 2)  


somaPares n = 
    if n < 0 then 0
    else if ehPar n then n + somaPares (n - 2) 
    else somaPares (n - 1) 

-- 5) SOMA POTÊNCIAS

somaPot2m m n = auxSoma 0 m n 

auxSoma e m n =  if e > n then 0 else m * pot2 e + auxSoma (e + 1) m n 

pot2 0 = 1  
pot2 e = 2 * pot2 (e - 1) 

-- 6) VERIFICAR SE É NÚMERO PRIMO

primo n = if n <= 1 then False else divisor n (n - 1)  

divisor n d = 
    if d == 1 
    then True  
    else if divisivelPor n d 
         then False  
         else divisor n (d - 1)  

divisivelPor n d = divisao n d 

divisao n d = 
    if n == 0 
    then True     
    else if n < 0 
         then False
         else divisao (n - d) d  


-- 7) APROXIMAÇÃO DE PI

seriePI n = auxSeriePI n 1 1 0.0

auxSeriePI n denom sign acc =
    let termo = (4 / fromIntegral denom) * fromIntegral sign 
    in if abs termo < 4 / fromIntegral n 
       then acc 
       else auxSeriePI n (denom + 2) (-sign) (acc + termo) 

-- Exemplos de testes:
-- abs (pi - seriePI 100) < 0.1
-- abs (pi - seriePI 10000) < 0.001
