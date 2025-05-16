-- exercicio.hs
-- comentario


idade :: Int -- Um valor inteiro constante
idade = 17

testeIdade :: Bool 			-- Usa a definicao de
testeIdade = idade>=18	-- idade

quadrado :: Int -> Int	-- funcao que eleva numero
quadrado x = x * x 			-- ao quadrado

mini :: Int -> Int -> Int -- funcao que mostra
mini a b 									-- o menor entre
	| a <= b = a 						-- dois valores
	| otherwise = b
	
tresIguais :: Int -> Int -> Int -> Bool		-- funcao que verifica 
tresIguais x y z = (x == y) && (y == z)		-- se tres numeros sao iguais

palindromo :: String -> Bool	-- verifica se a string é um palindromo
palindromo s = reverse s == s

verificaTriangulo :: Int -> Int -> Int -> Bool
verificaTriangulo x y z = (x + y > z) && (x + z > y) && (y + z > x)

sinal :: Int -> Int
sinal x
	| x < 0 = -1
	| x > 0 = 1
	| otherwise = 0
	
menorTres :: Int -> Int -> Int -> Int
menorTres a b c
	| a <= b && a <= c = a
	| b <= a && b <= c = b
	| otherwise = c

fatorial :: Int -> Int
fatorial 0 = 1
fatorial 1 = 1
fatorial n = n * fatorial (n-1)

potencia :: Int -> Int -> Int  -- Ordem: Base Expoente
potencia x 0 = 1
potencia x 1 = x
potencia x y = x * potencia x (y-1)

quatroIguais :: Int -> Int -> Int -> Int -> Bool
quatroIguais a b c d = (a == b) && (c == d) && (a == c)

quantosIguais :: Int -> Int -> Int -> Int
quantosIguais a b c
	| tresIguais a b c = 3
	| todosDiferentes a b c = 0
	| otherwise = 2

todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes a b c = (a /= b) && (b /= c) && (a /= c)

elevadoDois :: Int -> Int
elevadoDois x = x * x

elevadoQuatro :: Int -> Int
elevadoQuatro x = elevadoDois x * elevadoDois x

vendas :: Int -> Int
vendas x = x * 2


vendaTotal :: Int -> Int
vendaTotal 0 = vendas 0
vendaTotal x = vendas x + vendaTotal (x-1)
















