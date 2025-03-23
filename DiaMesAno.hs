module DiaMesAno where

type Dia = Int

data Mes = Janeiro | Fevereiro | Marco | Abril | Maio | Junho | Julho |
    Agosto | Setembro | Outubro | Novembro | Dezembro 
    deriving (Eq, Ord, Show, Enum, Read)

type Ano = Int

data Data = Data Dia Mes Ano 

instance Eq Data where
    (==) :: Data -> Data -> Bool
    (Data d1 m1 a1) == (Data d2 m2 a2) = (d1 == d2) && (m1 == m2) && (a1 == a2)

instance Ord Data where
    compare :: Data -> Data -> Ordering
    compare (Data d1 m1 a1) (Data d2 m2 a2)
        | a1 > a2   = GT
        | a1 < a2   = LT 
        | m1 > m2   = GT
        | m1 < m2   = LT
        | d1 > d2   = GT
        | d1 < d2   = LT
        | otherwise = EQ

instance Show Data where
    show :: Data -> String
    show (Data d m a) = show d ++ "/" ++ show (mesParaNum m) ++ "/" ++ show a

mesParaNum :: Mes -> Int
mesParaNum m = 1 + fromEnum m

diasNoMes :: Mes -> Ano -> Dia
diasNoMes Janeiro _   = 31
diasNoMes Fevereiro a = if anoBissexto a then 29 else 28
diasNoMes Marco _     = 31
diasNoMes Abril _     = 30
diasNoMes Maio _      = 31
diasNoMes Junho _     = 30
diasNoMes Julho _     = 31
diasNoMes Agosto _    = 31
diasNoMes Setembro _  = 30
diasNoMes Outubro _   = 31
diasNoMes Novembro _  = 30
diasNoMes Dezembro _  = 31

anoBissexto :: Ano -> Bool
anoBissexto ano = ano `mod` 4 == 0

mesSeguinte :: Mes -> Mes
mesSeguinte Dezembro = Janeiro
mesSeguinte m = succ m

mesAnterior :: Mes -> Mes 
mesAnterior Janeiro = Dezembro
mesAnterior m = pred m

(+!) :: Data -> Dia -> Data
(Data d m a) +! s

    | diasRestantes >= s = Data (d+s) m a
    | otherwise          = Data 1 (mesSeguinte m) (somaAno a) +! (s - 1 - diasRestantes)

    where   diasRestantes = diasNoMes m a - d
            somaAno a     = if m == Dezembro then a + 1 else a

(-!) :: Data -> Dia -> Data
(Data d m a) -! s 
    | d - s > 0    = Data (d-s) m a
    | m == Janeiro = Data 31 Dezembro (a-1) -! (s - d)
    | otherwise    = Data (diasNoMes (mesAnterior m) a) (mesAnterior m) a -! (s - d)