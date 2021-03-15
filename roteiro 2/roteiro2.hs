dobro::Float->Float
dobro x = x * 2

quadruplo::Float->Float
quadruplo x = dobro x + dobro x

hipotenusa::Float->Float->Float
hipotenusa x y = sqrt((x^2) + (y^2))

distancia::(Float,Float)->(Float,Float)->Float
distancia (x1,y1) (x2,y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)

conversao::Float->(Float,Float,Float)
conversao x = (x,x*3.96,x*4.45)

bissexto::Int->Bool
bissexto x
    |mod x 100 == 0 && mod x 400 == 0 = True
    |mod x 100 /= 0 && mod x 4 == 0 = True
    |otherwise = False

type Data = (Int, Int, Int)

bissexto2::Data->Bool
bissexto2 (d,m,a)
    |mod a 100 == 0 && mod a 400 == 0 = True
    |mod a 100 /= 0 && mod a 4 == 0 = True
    |otherwise = False

valida::Data->Bool
valida (d,m,a)
    |(d>=1 && d<=28) && (m>=1 && m<=12) = True
    |(d==29 || d==30) && (m/=2) = True
    |(d==31) && (m==1 || m==3 || m==5 || m==7 || m==8 || m==10 || m==12) = True 
    |(d==29) && m==2 && (bissexto2 (d,m,a)) = True
    |otherwise = False
    
precede::Data->Data->Bool
precede(d1,m1,a1) (d2,m2,a2)
    |a1<a2 = True
    |a1==a2 && m1<m2 = True
    |a1==a2 && m1==m2 && d1<d2 = True
    |otherwise = False
    
type Livro = (String, String, String, String, Int)
type Aluno = (String, String, String, String)
type Emprestimo = (String, String, Data, Data, String)

e1::Emprestimo
e1 = ("H123C9","BSI200945",(12,9,2009),(20,9,2009),"aberto")

emDia::Emprestimo->Data->Bool
emDia (codLivro, codAluno, dataEmprestimo, (de,me,ae), situacao) (d,m,a)
    |valida(de,me,ae) && valida(d,m,a) && (precede(d,m,a) (de,me,ae)) = True
    |valida(de,me,ae) && valida(d,m,a) && (d==de && m==me && a==ae) = True 
    |otherwise = False
