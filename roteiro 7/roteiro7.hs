--Questao 1
paridade::[Int]->[Bool]
paridade lst = map even lst

--Questao 2
prefixos::[String]->[String]
prefixos lst = map (take 3) lst

--Questao 3
saudacao::[String]->[String]
saudacao lst = map ("Oi " ++) lst

--Questao 4
filtrar:: (a->Bool)->[a]->[a]
filtrar f [] = []
filtrar f (h:t) 
    |f h = h:(filtrar f t)
    |otherwise = filtrar f t
    
filtrar2:: (a->Bool)->[a]->[a]
filtrar2 f lst = [x|x<-lst, f x]
    
--Questao 5
pares::[Int]->[Int]
pares lst = filter even lst

--Questao 6
solucoes::[Int]->[Int]
solucoes lst = filter (\x-> (5*x+6)<(x*x)) lst

--Questao 7
maior::[Int]->Int
maior lst = foldr1 max lst

--Questao 8
menor_min10::[Int]->Int
menor_min10 lst = foldr min 10 lst

--Questao 9
junta_silabas_plural:: [String]->String
junta_silabas_plural lst = foldr (++) "s" lst

--Questao 10   
menores10Aux::[Int]->[Int]
menores10Aux [] = []
menores10Aux (h:t)
    |h<10 = h:menores10Aux t
    |otherwise = menores10Aux t

conta10::[Int]->Int->Int
conta10 [] x = x;
conta10 (h:t) x
    |h<10 = conta10 t x+1
    |otherwise = conta10 t x;

menores10::[Int]->([Int],Int)
menores10 lst = (menores10Aux lst, conta10 lst 0)

--Questao 11
busca::Int->[Int]->Bool
busca x [] = False
busca x (h:t) 
    |x==h = True
    |x/=h = busca x t

numcomp::Int->[Int]->Int->Int
numcomp x [] cont = cont
numcomp x (h:t) cont
    |x==h = cont + 1
    |x/=h = numcomp x t cont+1

busca_elem::Int->[Int]->(Bool,Int)
busca_elem x lst = (busca x lst, numcomp x lst 0)