--Listas para testes
l1=[1..2000]
l2=[2000,1999..1]
l3=l1++[0]
l4=[0]++l2
l5=l1++[0]++l2
l6=l2++[0]++l1
l7=l2++[0]++l2
l8=[2001]++l1
x1=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
x2=[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
x3=[11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
x4=[10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
x5=[11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
x6=[1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
x7=[20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6]


--Questao 1
bolha::Ord a=>[a]->([a],Int)
bolha [] = ([],0) 
bolha lst = bolhaOrd (lst, 0) (length lst)

bolhaOrd::Ord a=>([a],Int)->Int->([a],Int)
bolhaOrd (lst, cont) 0 = (lst, cont)
bolhaOrd (lst, cont) n = bolhaOrd (troca lst cont) (n-1) 

troca::Ord a=>[a]->Int->([a],Int)
troca [x] cont = ([x],cont)
troca (x:y:zs) cont 
    |x>y = (y:lst1, contN1)    
    |otherwise = (x:lst2, contN2)
    where (lst1, contN1) = troca (x:zs) (cont+1)
          (lst2, contN2) = troca (y:zs) (cont)

--Questao 1 Variacao 1
bolha1::Ord a=>[a]->([a],Int)
bolha1 [] = ([],0) 
bolha1 lst = bolhaOrd1 (lst, 0)

bolhaOrd1::Ord a=>([a],Int)->([a],Int)
bolhaOrd1 (lst, cont)
    |contN /= cont = bolhaOrd1 (lstN, contN) 
    |otherwise = (lstN, contN)
    where (lstN, contN) = troca1 lst cont

troca1::Ord a=>[a]->Int->([a],Int)
troca1 [x] cont = ([x],cont)
troca1 (x:y:zs) cont 
    |x>y = (y:lst1, contN1)    
    |otherwise = (x:lst2, contN2)
    where (lst1, contN1) = troca1 (x:zs) (cont+1)
          (lst2, contN2) = troca1 (y:zs) (cont)

--Questao 1 Variacao 2
bolha2::Ord a=>[a]->([a],Int)
bolha2 [] = ([],0) 
bolha2 lst = bolhaOrd2 (lst, 0) (length lst)

bolhaOrd2::Ord a=>([a],Int)->Int->([a],Int)
bolhaOrd2 (lst, cont) n
    |contN /= cont = bolhaOrd2 (lstN, contN) (n-1)
    |otherwise = (lstN, contN)
    where (lstN, contN) = troca2 lst cont n

troca2::Ord a=>[a]->Int->Int->([a],Int)
troca2 lst cont 1 = (lst, cont)
troca2 (x:y:zs) cont n
    |x>y = (y:lst1, contN1)    
    |otherwise = (x:lst2, contN2)
    where (lst1, contN1) = troca2 (x:zs) (cont+1) (n-1)
          (lst2, contN2) = troca2 (y:zs) (cont) (n-1)
    
--Questao 2
selecao::Ord a=>[a]->([a],Int)
selecao [x] = ([x], 0)
selecao (h:t)
    |x == h = ((x:lstN), contN)
    |otherwise = ((x:lstN), (contN+1))
    where (lstN, contN) = selecao (remove x (h:t))
          x = minimo (h:t) 
    
remove::Ord a=>a->[a]->[a]
remove a [] = []
remove a (x:xs)
    |a==x = xs
    |otherwise = x:(remove a xs)
    
minimo::Ord a=>[a]->a
minimo [] = undefined
minimo [x] = x
minimo (x:xs)
    |x <= (minimo xs) = x
    |otherwise = minimo xs
    
--Questao 2 Variacao 1
selecao1::Ord a=>[a]->([a],Int)
selecao1 [x] = ([x], 0)
selecao1 (h:t)
    |min == h = ((min:lstN2), cont)
    |otherwise = ((min:lstN2), (cont+1))
    where (lstN2, cont) = selecao1 lstN 
          (lstN, min) = r_m (h:t)

r_m::Ord a=>[a]->([a],a)
r_m (h:t)
    |h == min = (lst, min)
    |h > min = (h:lst, min)
    where(lst, min) = remove_minimo (t) h

remove_minimo::Ord a=>[a]->a->([a],a)
remove_minimo [] x = ([],x)
remove_minimo (h:t) min 
    |h < min && h/=min1 = (h:lst1, min1)
    |h < min && h==min1 = (lst1, min1)
    |h >= min = (h:lst2, min2)
    where (lst1, min1) = remove_minimo (t) h
          (lst2, min2) = remove_minimo (t) min

--Questao 2 Variacao 2
selecao2::Ord a=>[a]->([a],Int)
selecao2 [x] = ([x], 0)
selecao2 (h:t)
    |x == h = (x:lstN, cont)
    |otherwise = (x:lstN, (cont+1))
    where (lstN, cont) = selecao2 (remove x (h:t)) 
          x = foldr1 min (h:t)

remove2::Ord a=>a->[a]->[a]
remove2 a [] = []
remove2 a (x:xs)
    |a==x = xs
    |otherwise = x:(remove2 a xs)

--Questao 3
insercao::Ord a=>[a]->([a],Int)
insercao [] = ([], 0)
insercao (x:xs) = insereOrd x (lst) (cont)
    where (lst, cont) = insercao xs 

insereOrd::Ord a=> a->[a]->Int->([a],Int)
insereOrd x [] cont = ([x], cont)
insereOrd x (y:zs) cont
    |x <= y = ((x:y:zs), cont+1)
    |otherwise = ((y:lst), (contN+1))
    where (lst, contN) = insereOrd x zs cont
    
--Questao 3 Variacao 1
insercao1::Ord a=>[a]->[a]
insercao1 lst = foldr insereOrd1 [] lst
 
insereOrd1::Ord a=> a->[a]->[a]
insereOrd1 x [] = [x]
insereOrd1 x (y:zs)
    |x <= y = (x:y:zs)
    |otherwise = (y:insereOrd1 x zs)

--Qeustao 4
quicksort::Ord a=>[a]->([a],Int)
quicksort [] = ([], 0)
quicksort [x] =([x],0)
quicksort (h:t) = ((quick1)++[h]++(quick2), (cont1)+(cont2)+length(t)*2)
    where (quick1, cont1) = quicksort[x|x<-t, x<h]
          (quick2, cont2) = quicksort[x|x<-t, x>=h]


--Questao 4 Variacao 1
quicksort1::Ord a=>[a]->([a],Int)
quicksort1 [] = ([], 0)
quicksort1 [x] =([x],0)
quicksort1 (h:t) = ((quick1)++[h]++(quick2), (cont1)+(cont2)+length(t))
    where (quick1, cont1) = quicksort1 (metade1)
          (quick2, cont2) = quicksort1 (metade2)
          (metade1, metade2) = divide h t
 
divide::Ord a=>a->[a]->([a],[a])
divide pivo [] = ([], [])
divide pivo (h:t)
    |h>=pivo = (menor, h:maior)
    |otherwise = (h:menor, maior)
    where (menor, maior) = divide pivo t

--Questao 4 Variacao 2
quicksort2::Ord a=>[a]->([a],Int)
quicksort2 [] = ([], 0)
quicksort2 [x] = ([x],0)
quicksort2 lst = ((quick1)++[pivo]++(quick2), (cont1)+(cont2)+length(lstN))
    where (quick1, cont1) = quicksort2 (metade1)
          (quick2, cont2) = quicksort2 (metade2)
          (metade1, metade2) = divide2 (pivo) (lstN)
          (lstN, pivo) = pivo_lista lst

pivo_lista::Ord a=>[a]->([a],a)
pivo_lista [x,y] = ([y], x)
pivo_lista (x:y:z:tail)
    |mediana [x,y,z] == x = ((y:z:tail),x)
    |mediana [x,y,z] == y = ((x:z:tail),y)
    |mediana [x,y,z] == z = ((x:y:tail),z)

mediana::Ord a=>[a]->a
mediana [x,y] = foldr1 max[x,y]
mediana [x,y,z]
    |foldr1 max[x,y,z]== x = mediana[y,z]
    |foldr1 max[x,y,z]== y = mediana[x,z] 
    |foldr1 max[x,y,z]== z = mediana[x,y]
    
divide2::Ord a=>a->[a]->([a],[a])
divide2 pivo [] = ([], [])
divide2 pivo (h:t)
    |h>=pivo = (menor, h:maior)
    |otherwise = (h:menor, maior)
    where (menor, maior) = divide2 pivo t

--Questao 5
mergesort :: Ord a=>[a]->([a],Int)
mergesort [] = ([],0)
mergesort [a] = ([a],0)
mergesort lst = (lstN, (cont1)+(cont2)+contN)
    where (metade1, cont1) = mergesort (take (div (length lst) 2) lst)
          (metade2, cont2) = mergesort (drop (div (length lst) 2) lst)
          (lstN, contN) = merge (metade1) (metade2)

merge :: Ord a =>[a]->[a]->([a],Int)
merge xs [] = (xs,0)
merge [] ys = (ys,0)
merge (x:xs) (y:ys) | x <= y    = (x:lst1, (cont1+1))
                    | otherwise = (y:lst2, (cont2+1))
    where (lst1, cont1) = merge xs (y:ys)
          (lst2, cont2) = merge (x:xs) ys

--Questao 6a
data Exp = Val Float -- um numero
         | Add Exp Exp -- soma de duas expressoes
         | Sub Exp Exp -- subtração de duas expressoes
         | Mul Exp Exp -- multiplicacao de duas expressoes
         | Div Exp Exp -- divisao de suas expressoes

avalia::Exp->Float
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Mul exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Div exp1 exp2) = (avalia exp1) / (avalia exp2)

--Questao 6b

-- (3+12)*(15-5)/(1*3) = 50
-- (Div (Mul (Add (Val 3) (Val 12)) (Sub (Val 15) (Val 5))) (Mul (Val 1) (Val 3))) = 50

-- -((6+8-5+1)*(2+6/2)) = -50
-- (Mul (Val (-1))(Mul (Add (Val 6) (Add (Val 8) (Add (Val (-5)) (Val 1)))) (Add (Val 2) (Div (Val 6) (Val 2))))) = -50

--Questao 7a
data Jogada = Papel | Tesoura | Pedra
              deriving(Show, Enum, Eq)

--Questao 7b
vence::Jogada->Jogada->Bool
Papel `vence` Pedra = True
j1 `vence` j2 
    |pred j1 == j2 = True
    |otherwise = False

--Questao 7c
vencedor ::(Jogada,Jogada)->Jogada
vencedor (Papel, Pedra) = Papel
vencedor (j1, j2)
    |pred j1 == j2 = j1
    |otherwise = j2

vencedoras::[(Jogada,Jogada)]->[Jogada]
vencedoras lst = map vencedor lst

--Questao 8a
data Nebuloso = Verdadeiro
              | Talvez Float
              | Falso
              deriving(Show)

--Questao 8b
fuzzifica::Float->Nebuloso
fuzzifica valor
    |valor <=0 = Falso
    |valor >=1 = Verdadeiro
    |otherwise = Talvez valor

--Questao 8c
verifica_alto::Float->Nebuloso
verifica_alto altura = fuzzifica ((altura-1.70)/0.20)

--Questao 8d
verifica_barato::Float->Nebuloso
verifica_barato custo = fuzzifica ((50000-custo)/20000)

--Questao 9a
data Estudante = Colegial Ano Instituicao Matricula Altura Peso
               | Universitario Instituicao Curso Matricula Altura Idade
               deriving(Show)
               
type Ano = Int
type Instituicao = String
type Matricula = String
type Altura = Float
type Peso = Float
type Curso = String
type Idade = Int

type Estudantes = [Estudante]

base::Estudantes
base=[(Colegial 1 "Gabarito" "AAAAA" 1.68 85),
      (Colegial 1 "Nacional" "AAAAB" 1.65 60),
      (Colegial 1 "Olimpo" "AAAAC" 1.67 72),
      (Colegial 2 "Nacional" "AAAAD" 1.70 67),
      (Colegial 2 "Olimpo" "AAAAE" 1.89 78),
      (Colegial 2 "Gabarito""AAAAF" 1.84 89),
      (Colegial 2 "Nacional" "AAAAG" 1.73 64),
      (Colegial 3 "Olimpo" "AAAAH" 1.72 72),
      (Colegial 3 "Nacional" "AAAAI" 1.92 89),
      (Colegial 3 "Gabarito" "AAAAJ" 1.95 95),
      (Universitario "UFU" "Computacao" "11921035" 1.76 20),
      (Universitario "UNITRI" "Medicina" "12345678" 1.61 19),
      (Universitario "UNA" "Direito" "11111111" 1.69 24),
      (Universitario "UFU" "Musica" "52134687" 1.65 17),
      (Universitario "UNITRI" "Direito" "12435879" 1.72 29),
      (Universitario "UNA" "Computacao" "13215472" 1.78 35),
      (Universitario "UFU" "Medicina" "42187258" 1.85 49),
      (Universitario "UNITRI" "Musica" "87512346" 1.82 27),
      (Universitario "UNA" "Computacao" "99999999" 1.92 21),
      (Universitario "UFU" "Medicina" "87654321" 1.91 25)]

--Questao 9b
descobre_altos::Estudantes->[(Matricula, Nebuloso)]
descobre_altos alunos = map d_a alunos

d_a::Estudante->(Matricula, Nebuloso)
d_a (Colegial _ _ matricula altura _) = (matricula, verifica_alto altura)
d_a (Universitario _ _ matricula altura _) = (matricula, verifica_alto altura)

--Questao 10
data ArvBinInt = Nulo
               | No Int ArvBinInt ArvBinInt
               deriving (Show)

--Lista das Folhas
folhas::ArvBinInt->[Int]
folhas (Nulo) = []
folhas (No x Nulo Nulo) = [x]
folhas (No x esq dir) = (folhas esq) ++ (folhas dir)

--Soma dos Nos Internos
posOrdem::ArvBinInt->[Int]
posOrdem Nulo = []
posOrdem (No x Nulo Nulo) = []
posOrdem (No x esq dir) = (posOrdem esq) ++ (posOrdem dir) ++ [x]

somaNosInternos::ArvBinInt->Int
somaNosInternos arv = foldr1 (+) (posOrdem arv)

--Verifica se elemento pertence a lista
pertence::ArvBinInt->Int->Bool
pertence (Nulo) elem = False
pertence (No x esq dir) elem = (pertence esq elem) || x==elem || (pertence dir elem)

arvEx::ArvBinInt
arvEx = (No 2 (No 7 (No 2 Nulo Nulo)
                    (No 6 (No 5 Nulo Nulo)
                          (No 11 Nulo Nulo)))
              (No 5 Nulo (No 9 (No 4 Nulo Nulo) Nulo)))


