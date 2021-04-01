--Questao 1
analisa_raizes::Int->Int->Int->String
analisa_raizes a b c
    |a==0 = "Equacao degenerada"
    |b^2>4*a*c = "Possui duas raizes reais"
    |b^2==4*a*c = "Possui uma raiz real"
    |otherwise = "Nao possui raizes reais"
    
--Questao 2
equacao::Float->Float->Float->(Float,Float)
equacao a b c 
    |a==0 = ((-c)/b,a)
    |otherwise = ((-b+sqrt((b^2)-4*a*c))/(a*2) , (-b-sqrt((b^2)-4*a*c))/(a*2))
    
--Questao 3
type Data = (Int,Int,Int)

precede::Data->Data->Bool
precede(d1,m1,a1) (d2,m2,a2)
    |a1<a2 = True
    |a1==a2 && m1<m2 = True
    |a1==a2 && m1==m2 && d1<d2 = True
    |otherwise = False

bissexto::Int->Bool
bissexto x
    |mod x 100 == 0 && mod x 400 == 0 = True
    |mod x 100 /= 0 && mod x 4 == 0 = True
    |otherwise = False

valida::Data->Bool
valida (d,m,a)
    |(d>=1 && d<=28) && (m>=1 && m<=12) = True
    |(d==29 || d==30) && (m/=2) = True
    |(d==31) && (m==1 || m==3 || m==5 || m==7 || m==8 || m==10 || m==12) = True 
    |(d==29) && m==2 && (bissexto a) = True
    |otherwise = False

aniversario::(Int,Int)->(Int,Int)->Bool
aniversario (d,m)(dn,mn)
    |mn<m = True
    |mn==m && dn<=d = True
    |otherwise = False

idade::Data->Data->Int
idade (d, m, a)(dn, mn, an)
    |valida(d,m,a) && valida(dn,mn,an) && precede(dn,mn,an)(d,m,a) && a==an = 0
    |valida(d,m,a) && valida(dn,mn,an) && precede(dn,mn,an)(d,m,a) && aniversario(d,m)(dn,mn) = (a-an)
    |valida(d,m,a) && valida(dn,mn,an) && precede(dn,mn,an)(d,m,a) && not(aniversario(d,m)(dn,mn)) = (a-an)-1
    |otherwise = -1
    
passagem::Float->Data->Data->Float
passagem valor hoje nasc
    |(idade hoje nasc)<= 2 && (idade hoje nasc)>=0 = valor*0.15
    |(idade hoje nasc)<= 10 && (idade hoje nasc)>=3 = valor*0.40
    |(idade hoje nasc)<= 69 && (idade hoje nasc)>=11 = valor
    |(idade hoje nasc)>=70 = valor*0.50
    |otherwise = -1
    
--Questao 4
inteiros = [1..20]
l1=[15,16]
gera1 = [x^3|x<-inteiros, even x, x>2, x<12]
gera2 = [(x,y)|x<-inteiros, x<6, y<-[x..x*3]]
gera3 = [x|y<-l1, x<-inteiros, x<=y]
gera4 = [(x,y)| x<-inteiros, x<10, even x, y<-[x+1]]
gera5 = [x+y| dupla<-gera4, x<-[fst(dupla)], y<-[snd(dupla)]] 

--Questao 5a
contaPosM3::[Int]->Int
contaPosM3 lst = length[x|x<-lst, x>0, (mod x 3)==0]

--Questao 5b
listaPosM3::[Int]->[Int]
listaPosM3 lst = [x|x<-lst, x>0, (mod x 3)==0]

--Questao 6
fatores::Int->[Int]
fatores x = [y|y<-[1..x], (mod x y)==0]

primos::Int->Int->[Int]
primos a b = [x| x<-[a..b], (fatores x)== [1,x]]

--Questao 7 
mmc::Int->Int->Int->Int
mmc a b c = [x|x<-fatores (a*b*c), (mod x a)==0, (mod x b)==0, (mod x c)==0] !! 0

--Questao 8
serie::Float->Int->Float
serie x n
    |n==1 = 1/x
    |odd n = (fromIntegral n)/x + (serie x (n-1))
    |even n = x/(fromIntegral n) + (serie x (n-1))
  
--Questao 9
fb::Int->[String]->[String]
fb n lst
    |n<1 = lst
    |n>=1 && (mod n 2)==0 && (mod n 3) ==0 = fb (n-1) ("FizzBuzz":lst)
    |n>=1 && (mod n 2)==0 = fb (n-1) ("Fizz":lst) 
    |n>=1 && (mod n 3) ==0 = fb (n-1) ("Buzz":lst)
    |otherwise = fb (n-1) ("No":lst)

fizzbuzz::Int->[String]
fizzbuzz n = fb n []

--Questao 10
sel_multiplos::Int->[Int]->[Int]
sel_multiplos n lst = [x|x<-lst, (mod x n)==0]

--Questao 11
ocorrencias::(Eq a)=>a->[a]->Int->Int
ocorrencias n [] cont = cont
ocorrencias n (h:t) cont
    |n == h = ocorrencias n t (cont+1)
    |n /= h = ocorrencias n t cont

unica_ocorrencia::Int->[Int]->Bool
unica_ocorrencia n lst 
    |(ocorrencias n lst 0) == 1 = True
    |otherwise = False

--Questao 12
inter::[Int]->[Int]->[Int]->[Int]
inter lst1 lst2 result
    |lst1==[] && lst2==[] = result
    |lst1==[] = inter lst1 (tail lst2) ((head lst2):result)
    |lst2==[] = inter (tail lst1) lst2 ((head lst1):result)
    |otherwise = inter (tail lst1) (tail lst2) ((head lst2):(head lst1):result)

intercala::[Int]->[Int]->[Int]
intercala lst1 lst2 = reverse(inter lst1 lst2 [])

--Questao 13
zipar:: [a]->[a]-> [[a]]
zipar lst1 [] = []
zipar [] lst2 = []
zipar (h1:t1) (h2:t2) = [h1,h2]:(zipar t1 t2)

--Questao 14
type Contato = (String, String, String, String) --(Nome, Endereco, Telefone, Email)
type Agenda = [Contato]

agenda::Agenda
agenda = [("Maria", "Rua 1", "999487512", "maria@gmail.com"),
          ("Joao", "Rua 2", "998457125", "joao@outlook.com"),
          ("Jose", "Rua 3", "991574632", "jose@gmail.com")]

recupera_nome::String->Agenda->String
recupera_nome em [] = "Email desconhecido"
recupera_nome em ((nome, _, _, email):t)
    |em == email = nome
    |em /= email = recupera_nome em t
    
--Questao 15
type Pessoa = (String, Float, Int, Char)

pessoas :: [Pessoa]
pessoas = [ ("Rosa", 1.66, 27,'F'),
            ("João", 1.85, 26, 'C'),
            ("Maria", 1.55, 62, 'S'),
            ("Jose", 1.78, 42, 'C'),
            ("Paulo", 1.93, 25, 'S'),
            ("Clara", 1.70, 33, 'C'),
            ("Bob", 1.45, 21, 'C'),
            ("Rosana", 1.58,39, 'S'),
            ("Daniel", 1.74, 72, 'S'),
            ("Jocileide", 1.69, 18, 'S') ]

soma_altura::[Pessoa]->Float
soma_altura [(_, altura, _, _)] = altura
soma_altura ((_, altura, _, _):t) = altura + (soma_altura t)


altura_media::[Pessoa]->Float
altura_media pe = (soma_altura pe)/(fromIntegral(length pe))

mais_nova::[Pessoa]->Int
mais_nova [(_,_,idade,_)] = idade
mais_nova ((nome1,altura1,idade1,ec1):(nome2,altura2,idade2,ec2):t)
    |idade1<idade2 = mais_nova ((nome1,altura1,idade1,ec1):t)
    |otherwise = mais_nova ((nome2,altura2,idade2,ec2):t)
    
mais_velha::[Pessoa]->(String,Char)
mais_velha [(nome,_,_,ec)] = (nome, ec)
mais_velha ((nome1,altura1,idade1,ec1):(nome2,altura2,idade2,ec2):t)
    |idade1>idade2 = mais_velha ((nome1,altura1,idade1,ec1):t)
    |otherwise = mais_velha ((nome2,altura2,idade2,ec2):t)
    
ciquentaOuMais::[Pessoa]->[Pessoa]
ciquentaOuMais pe = [(nome,altura,idade,ec)|(nome,altura,idade,ec)<-pe, idade>=50] 

casadas::[Pessoa]->Int->Int
casadas [] _ = 0
casadas ((nome,altura,idade,ec):t) i
    |ec == 'C' && idade >= i = 1 + (casadas t i)
    |otherwise = (casadas t i)
    
--Questao 16
insere_ord::(Ord a)=>a->[a]->[a]
insere_ord x [] = [x]
insere_ord x (h:t)
    |x<=h = (x:h:t)
    |otherwise = h:(insere_ord x t)
    
--Questao 17
rv::[a]->[a]->[a]
rv [x] lst2 = x:lst2
rv (h:t) lst2 = rv t (h:lst2)

reverte::[a]->[a]
reverte lst = rv lst []

--Questao 18
elimina::Eq a=>a->[a]->[a]
elimina x [] = []
elimina x (h:t)
    |x/=h = h:(elimina x t)
    |otherwise = elimina x t
    
elimina_repet::Eq a=>[a]->[a]
elimina_repet [] = []
elimina_repet(h:t) = h:elimina_repet(elimina h t)

--Questao 19
disponiveis = [1,2,5,10,20,50,100]

notasTroco::Int->[[Int]]
notasTroco 0 = [[]]
notasTroco troco = [x:xs|x<-disponiveis, troco>=x, xs<-notasTroco(troco-x)]


--Questao 20
modulo::Int->Int
modulo x
    |x>0 = x
    |otherwise = -x
    
ataca::(Int,Int)->(Int,Int)->Bool
ataca (x1,y1) (x2,y2)
    |modulo(x1-x2)==modulo(y1-y2) || (x1==x2) || (y1==y2) = True
    |otherwise = False
    
ataca_lista::(Int,Int)->[(Int,Int)]->Bool
ataca_lista (_,_) [] = False
ataca_lista (x1,y1) ((x2,y2):t)
    |ataca (x1,y1) (x2,y2) = True
    |otherwise = ataca_lista (x1,y1) t
    
alguemSeAtaca::[(Int,Int)]->Bool
alguemSeAtaca [(_,_)] = False
alguemSeAtaca ((x1,y1):t)
    |ataca_lista (x1,y1) t = True
    |otherwise = alguemSeAtaca t

tc_sublista::[(Int,Int)]->[Int]
tc_sublista [] = []
tc_sublista ((x,y):t) = y:(tc_sublista t)

tira_coluna::[[(Int,Int)]]->[[Int]]
tira_coluna [] = []
tira_coluna (h:t) = (tc_sublista h):(tira_coluna t) 

geraRainhas::Int->Int->[[(Int,Int)]]
geraRainhas 1 m = [[(1,y)]| y<-[1..m]]
geraRainhas n m = [(x,y):t|x<-[n,n-1..1], y<-[1..m], t<-(geraRainhas (n-1) m), not(ataca_lista (x,y) t)]

nRainhas::Int->[[Int]]
nRainhas n = tira_coluna (geraRainhas n n)

    

