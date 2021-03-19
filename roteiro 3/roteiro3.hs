--Questao 1
ouP::(Bool,Bool)->Bool
ouP(_,True)=True
ouP(True,_)=True
ouP(_,_)=False

ouG::(Bool,Bool)->Bool
ouG (x,y)
    |x==True = True 
    |y==True = True
    |otherwise = False

--Questao 2
distancia::(Float,Float)->(Float,Float)->Float
distancia (x1,y1) (x2,y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)

--Questao 3
fatorialp::Int->Int
fatorialp 0 = 1
fatorialp n = n*fatorialp(n-1)

fatorialg::Int->Int
fatorialg n
    |n==0 = 1
    |n>0 = n*fatorialg(n-1)

--Questao 4        
fibo::Int->Int
fibo 1 = 1
fibo 2 = 1
fibo n = fibo(n-1) + fibo(n-2)

--QUestao 5
n_tri::Int->Int
n_tri 0 = 0
n_tri n = n_tri (n-1) + n

--Questao 6
potencia2::Int->Int
potencia2 0 = 1
potencia2 n = potencia2(n-1) * 2

--Questao 7
prodIntervalo::(Int,Int)->Int
prodIntervalo(m,n) = if (m==n-1) then m*n
    else prodIntervalo(m,n-1)*n

fatProd::Int->Int
fatProd 0 = 1
fatProd 1 = 1
fatProd x = prodIntervalo(1,x)                     

--Questao 8
resto_div::(Int,Int)->Int
resto_div(m,n)
    |m==n = 0
    |m>n = resto_div(m-n , n) 
    |otherwise = m
    
div_inteira:: (Int,Int)->Int
div_inteira(m,n)
    |m-n<n = 1
    |otherwise = 1 + div_inteira(m-n,n)
    
--Questao 9
mdcg::(Int,Int)->Int
mdcg (x,y)
    |y==0 = x
    |otherwise= mdcg(y, mod x y)
    
mdcp::(Int,Int)->Int
mdcp (x,0) = x
mdcp (x,y) = mdcp(y, mod x y)

--Questao 10 
binog::(Int,Int)->Int
binog(n,k)
    |k==0 = 1
    |k==n = 1
    |otherwise = binog(n-1,k) + binog(n-1,k-1)
   
binop::(Int,Int)->Int
binop (n,0) = 1
binop (n,k) = if (k==n) then 1
    else binop (n-1,k) + binop (n-1,k-1)          

--Questao 11
passo::Int->(Int,Int) -> (Int,Int)
passo 1 (x,y) = (x,y)
passo n (x,y) = passo (n-1) (y,x+y)

fibo2::Int->Int
fibo2 n = fst(passo n (1,1))







 


