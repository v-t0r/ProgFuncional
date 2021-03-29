--Questao 2
listaA = [5,4..1]
listaB = ['a','c'..'e']
listaC = [1,4..16]
listaD = zip [1,(-2)..(-11)] [1,5..17]

--Qeustao 3a
intervalo:: (Int,Int)->[Int]
intervalo (a,b) = [a..b]

--Questao 3b
interPares::(Int,Int)->[Int]
interPares (a,b)
 |even a = [(a+2),(a+4)..(b-1)]
 |otherwise = [(a+1),(a+3)..(b-1)]
 
--Questao 5
quadrados:: (Int,Int)->[Int]
quadrados (a,b) = [x^2|x<-[a..b]] 

--Questao 6
seleciona_impares::[Int]->[Int]
seleciona_impares lst = [x|x<-lst, odd x]

--Questao 7
tabuada:: Int->[Int]
tabuada a = [x*a|x<-[1..10]]

--Questao 8
bissexto::Int->Bool
bissexto x
    |mod x 100 == 0 && mod x 400 == 0 = True
    |mod x 100 /= 0 && mod x 4 == 0 = True
    |otherwise = False
    
bissextos::[Int]->[Int]
bissextos lst = [x|x<-lst, bissexto x]

--Questao 9
sublistas::[[Int]]->[Int]
sublistas lst = [x|y<-lst, x<-y]

--Questao 10
precede::Data->Data->Bool
precede(d1,m1,a1) (d2,m2,a2)
    |a1<a2 = True
    |a1==a2 && m1<m2 = True
    |a1==a2 && m1==m2 && d1<d2 = True
    |otherwise = False

valida::Data->Bool
valida (d,m,a)
    |(d>=1 && d<=28) && (m>=1 && m<=12) = True
    |(d==29 || d==30) && (m/=2) = True
    |(d==31) && (m==1 || m==3 || m==5 || m==7 || m==8 || m==10 || m==12) = True 
    |(d==29) && m==2 && (bissexto a) = True
    |otherwise = False

emDia::Emprestimo->Data->Bool
emDia (codLivro, codAluno, dataEmprestimo, (de,me,ae), situacao) (d,m,a)
    |valida(de,me,ae) && valida(d,m,a) && (precede(d,m,a) (de,me,ae)) = True
    |valida(de,me,ae) && valida(d,m,a) && (d==de && m==me && a==ae) = True 
    |otherwise = False

type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]
bdEmprestimo::Emprestimos
bdEmprestimo =
 [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
 ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
 ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]
 
atrasados::Emprestimos->Data->Emprestimos
atrasados emps hoje = [x| x<-emps, not(emDia x hoje)]

--Questao 11
uniaoNRec::[Int]->[Int]->[Int]
uniaoNRec lst1 lst2 =lst1 ++ [x| x<-lst2, not(elem x lst1)] 
   

    
 