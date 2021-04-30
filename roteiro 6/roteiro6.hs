--Questao 1.a
valida::(Int,Int,Int)->Bool
valida (d,m,a)
    |(d>=1 && d<=28) && (m>=1 && m<=12) = True
    |(d==29 || d==30) && (m/=2) = True
    |(d==31) && (m==1 || m==3 || m==5 || m==7 || m==8 || m==10 || m==12) = True 
    |(d==29) && m==2 && (bissexto a) = True
    |otherwise = False
    where
    bissexto::Int->Bool
    bissexto a
        |mod a 100 == 0 && mod a 400 == 0 = True
        |mod a 100 /= 0 && mod a 4 == 0 = True
        |otherwise = False

--Questao 1.b
bissextos::[Int]->[Int]
bissextos lst = [x|x<-lst, bissexto x]
    where
    bissexto::Int->Bool
    bissexto a
        |mod a 100 == 0 && mod a 400 == 0 = True
        |mod a 100 /= 0 && mod a 4 == 0 = True
        |otherwise = False

--Questao 1.c
type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

atrasados::Emprestimos->Data->Emprestimos
atrasados emps hoje = [x| x<-emps, not(emDia x hoje)]
    where
    emDia::Emprestimo->Data->Bool
    emDia (codLivro, codAluno, dataEmprestimo, (de,me,ae), situacao) (d,m,a)
        |valida(de,me,ae) && valida(d,m,a) && (precede(d,m,a) (de,me,ae)) = True
        |valida(de,me,ae) && valida(d,m,a) && (d==de && m==me && a==ae) = True 
        |otherwise = False
        where
        valida::(Int,Int,Int)->Bool
        valida (d,m,a)
            |(d>=1 && d<=28) && (m>=1 && m<=12) = True
            |(d==29 || d==30) && (m/=2) = True
            |(d==31) && (m==1 || m==3 || m==5 || m==7 || m==8 || m==10 || m==12) = True 
            |(d==29) && m==2 && (bissexto a) = True
            |otherwise = False
            where
            bissexto::Int->Bool
            bissexto a
                |mod a 100 == 0 && mod a 400 == 0 = True
                |mod a 100 /= 0 && mod a 4 == 0 = True
                |otherwise = False
        precede::Data->Data->Bool
        precede(d1,m1,a1) (d2,m2,a2)
            |a1<a2 = True
            |a1==a2 && m1<m2 = True
            |a1==a2 && m1==m2 && d1<d2 = True
            |otherwise = False

--Questao 1.d
fibo2::Int->Int
fibo2 n = fst(passo n (1,1))
    where
    passo::Int->(Int,Int) -> (Int,Int)
    passo 1 (x,y) = (x,y)
    passo n (x,y) = passo (n-1) (y,x+y)
    
--Questao 1.e
fatProd::Int->Int
fatProd 0 = 1
fatProd 1 = 1
fatProd x = prodIntervalo(1,x)
    where
    prodIntervalo::(Int,Int)->Int
    prodIntervalo(m,n) = if (m==n-1) then m*n
                         else prodIntervalo(m,n-1)*n

--Questao 2.a
valida2::(Int,Int,Int)->Bool
valida2 (d,m,a)
    |(d>=1 && d<=28) && (m>=1 && m<=12) = True
    |(d==29 || d==30) && (m/=2) = True
    |(d==31) && (m==1 || m==3 || m==5 || m==7 || m==8 || m==10 || m==12) = True 
    |(d==29) && m==2 && let {
    bissexto2 a
        |mod a 100 == 0 && mod a 400 == 0 = True
        |mod a 100 /= 0 && mod a 4 == 0 = True
        |otherwise = False
    }in bissexto2 a = True
    |otherwise = False
    
--Questao 2.b
bissextos2::[Int]->[Int]
bissextos2 lst = let{
    bissexto2 a
        |mod a 100 == 0 && mod a 400 == 0 = True
        |mod a 100 /= 0 && mod a 4 == 0 = True
        |otherwise = False
    }in [x|x<-lst, bissexto2 x]
    
--Questao 2.c
atrasados2::Emprestimos->Data->Emprestimos
atrasados2 emps hoje = let{
    emDia2 (codLivro, codAluno, dataEmprestimo, (de,me,ae), situacao) (d,m,a) let{
        valida2 (d,m,a)
            |(d>=1 && d<=28) && (m>=1 && m<=12) = True
            |(d==29 || d==30) && (m/=2) = True
            |(d==31) && (m==1 || m==3 || m==5 || m==7 || m==8 || m==10 || m==12) = True 
            |(d==29) && m==2 && let{
                bissexto2 a
                    |mod a 100 == 0 && mod a 400 == 0 = True
                    |mod a 100 /= 0 && mod a 4 == 0 = True
                    |otherwise = False
            }in bissexto2 a = True
            |otherwise = False
        precede2(d1,m1,a1) (d2,m2,a2)
            |a1<a2 = True
            |a1==a2 && m1<m2 = True
            |a1==a2 && m1==m2 && d1<d2 = True
            |otherwise = False
        }in |valida2(de,me,ae) && valida2(d,m,a) && (precede2(d,m,a) (de,me,ae)) = True
            |valida2(de,me,ae) && valida2(d,m,a) && (d==de && m==me && a==ae) = True 
            |otherwise = False
    }in [x| x<-emps, not(emDia x hoje)]

--Questao 2.d 
fibo3::Int->Int
fibo3 n = let{
    passo2 1 (x,y) = (x,y)
    passo2 n (x,y) = passo2 (n-1) (y,x+y)
    }in fst(passo2 n (1,1))

--Questao 2.e  
fatProd::Int->Int
fatProd 0 = 1
fatProd 1 = 1
fatProd x = let{ 
    prodIntervalo(m,n) = if (m==n-1) then m*n
                         else prodIntervalo(m,n-1)*n
    }in prodIntervalo(1,x)
    
--Questao 5
--a  (\x y->y)((\z->z)(\z->z))(\w->w)5        == 5
--b  ((\f->(\x->f(f x)))(\y->(y*y)))3         == 81
--c  ((\f->(\x->f(f x)))(\y->(y+y)))5         == 20
--d  ((\x->(\y->x+y)5)((\y->y-3)7))           == 9
--e  (((\f->(\x->f(f(f x))))(\y->(y*y)))2)    == 256
--f  (\x y-> x+((\x-> x-3)y))5 6              == 8


    
    
    
    
