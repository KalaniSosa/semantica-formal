-- Listas 


somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs


multDois :: [Int] -> [Int]
multDois [] = []
multDois (x:xs) = 2*x : multDois xs


multLista :: Int -> [Int] -> [Int]
multLista c [] =  []
multLista c (x:xs) = c*x : multLista c xs 

elemento :: Int -> [Int] -> Bool
elemento c [] = False
elemento c (x:xs) = x == c || elemento c xs

conta :: Int -> [Int] -> Int
conta c [] = 0
conta c (x:xs)
	| c == x = 1 + conta c xs
	| otherwise = 0 + conta c xs
	
	
contaMaiores :: Int -> [Int] -> Int
contaMaiores c [] = 0
contaMaiores c (x:xs)
	| c < x = 1 + contaMaiores c xs
	| otherwise = 0 + contaMaiores c xs

	
maiores :: Int -> [Int] -> [Int]
maiores c [] = []
maiores c (x:xs)
	| c < x = x : maiores c xs
	| otherwise = maiores c xs
	

geraLista :: Int -> Int -> [Int]
geraLista 0 n = []
geraLista m n = n : geraLista (m-1) n


addFim :: Int -> [Int] -> [Int]
addFim c [] = [c]
addFim c (x:xs) = x : addFim c xs 


join :: [Int] -> [Int] -> [Int] 
join [] y = y 
join (x:xs) y = x : join xs y
	
inverte :: [Int] -> [Int]	
inverte x = auxInverte x [] 


auxInverte :: [Int] -> [Int] -> [Int]
auxInverte [] acc = acc 
auxInverte (x:xs) acc = auxInverte xs (x : acc)


membro :: Int -> [Int] -> Bool
membro c [] = False
membro c x = conta c x /= 0


unico :: [Int] -> [Int]
unico [] = []
unico x = auxUnico x x
	

auxUnico :: [Int] -> [Int] -> [Int]
auxUnico [] y = []
auxUnico (x:xs) y
	| conta x y > 1 = auxUnico xs y
	| otherwise = x : auxUnico xs y  
	

























