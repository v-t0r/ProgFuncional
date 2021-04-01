--Questao 1
conta_ch::[Char]->Int
conta_ch [] = 0
conta_ch (x:resto) = 1 + conta_ch resto

conta::[t]->Int
conta [] = 0
conta (x:r) = 1 + conta r

maior::[Int]->Int
maior [x] = x
maior (x:y:resto)
    |x > y = maior (x:resto)
    |otherwise = maior (y:resto)

primeiros::Int->[t]->[t]
primeiros 0 _ = []
primeiros _ [] = []
primeiros n (x:xs) = x: primeiros (n-1) xs

pertence::Eq t=> t->[t]->Bool
pertence a [] = False
pertence a (x:z) = if (a==x) then True  
                             else pertence a z

uniaoR::Eq t=> [t]->[t]->[t]
uniaoR [] l = l
uniaoR (x:xs) l = if pertence x l then uniao xs l
                                else x: uniaoR xs l

uniao:: Eq t => [t]->[t]->[t]
uniao a b = a ++ [x|x<-b, not(pertence x a)]

--Questao 2
npares::[Int]->Int
npares [] = 0
npares (h:t)
    |even h = 1 + npares t
    |otherwise = npares t

--Questao 3
produtorio::Num a=> [a]->a
produtorio [] = 0
produtorio [x] = x
produtorio (h:t) = h * (produtorio t)

--Questao 4
comprime_sub::[a]->[a]->[a]
comprime_sub [] lst = lst
comprime_sub (h:t) lst = h:(comprime_sub t lst)

comprime::[[a]]->[a]
comprime [] = []
comprime (h:t) = comprime_sub h (comprime t)

--Questao 5
tamanho::[t]->Int
tamanho [] = 0
tamanho (x:r) = 1 + tamanho r

--Questao 6
tira_repetido::Eq t=>[t]->[t]->[t]
tira_repetido _ [] = []
tira_repetido lst (h:t)
    |not(pertence h lst) = h:(tira_repetido lst t)
    |otherwise = tira_repetido lst t

uniaoRec2::Eq t=> [t]->[t]->[t]
uniaoRec2 [] lst = lst
uniaoRec2 (h:t) lst = h:(uniaoRec2 t (tira_repetido (h:t) lst))