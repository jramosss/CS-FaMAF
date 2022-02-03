module Song where
import Euterpea


data Song = 
    Fragment [Primitive Pitch] -- Crea una canción dada la lista de notas 
    | Transpose_by Int Song    -- Transpone todas las notas de s en i sem-tonos.
    | Repeat Int Song          -- Repite i veces la canción s.
    | Concat Song Song         -- Concatena las canciones s y s´.
    | Parallel Song Song       -- Representa la canción compuesta
    deriving (Show, Eq)        -- por la ejecución de s y s' en conjunto.


--FUNCIONES AUXILIARES
-- traspone: traspone n tonos la nota dada

transpose_aux :: Int -> Primitive Pitch -> Primitive Pitch

transpose_aux n (Rest q) = (Rest q)

transpose_aux n (Note x (a,b)) = (Note x (trans n (a,b)))
-------


-- transpp: se empieza a transponer cada nota del song

transpp :: Int -> Song -> Song

transpp n (Fragment xs) = (Fragment (map (\x -> (transpose_aux n x)) xs))

transpp n (Concat s s') = (Concat (transpp n s) (transpp n s'))

transpp n (Parallel s s') = (Parallel (transpp n s) (transpp n s'))


-- FUNCIONES PRINCIPALES --


unfold :: Song -> Maybe Song

unfold (Fragment xs) = Just(Fragment(xs))

unfold (Concat s s') = (unfold(s)) >>= \s1 ->
    (unfold(s')) >>= \s1' ->
        Just(Concat s1 s1')

unfold (Parallel s s') = (unfold(s)) >>= \s1 ->
    (unfold(s')) >>= \s1' ->
        Just(Parallel s1 s1')

unfold (Repeat n s) 
    | n <= 0 = Nothing
    | n == 1 = Just s
    | s == (Fragment []) = Nothing
    | otherwise = (unfold s) >>= \x -> 
       (unfold (Repeat (n-1) x)) >>= \p ->
            (Just (Concat x p))

unfold (Transpose_by i t) = 
    case t of 
        (Repeat k s) -> (unfold(Transpose_by i s)) >>= \m -> 
            (unfold(Repeat k m))

        (Concat s z) -> (unfold (Transpose_by i z)) >>= \m -> 
            ((unfold(Transpose_by i s)) >>= \p -> 
                (Just (Concat p m)))

        (Parallel s z) -> (unfold(Transpose_by i s)) >>= \m -> 
            ((unfold(Transpose_by i z)) >>= \p -> 
                (Just (Parallel m p)))

        (Transpose_by j x) -> unfold(Transpose_by 0 x) >>= \m ->
            (unfold(Transpose_by i (transpp j m)))

        t -> Just((transpp i t))

-------

compute :: Song -> Maybe (Music Pitch)

compute (Fragment []) = Just (Prim (Rest 0))

compute (Fragment (x:xs)) = (compute (Fragment xs)) >>= \p -> 
        Just (Prim x :+: p)

compute (Concat s1 s2) = (compute s2) >>= \ms2 -> 
        ((compute s1) >>= \ms1 -> 
            Just(ms1 :+: ms2))

compute (Parallel s1 s2) = (compute s2) >>= \ms2 -> 
        ((compute s1) >>= \ms1 -> 
            Just(ms1 :=: ms2))

compute s = (unfold s) >>= \ms -> (compute ms)

-------

time :: Song -> Maybe Rational

time (Fragment []) = Just 0

time (Fragment((Rest q) : xs)) = (time (Fragment xs)) >>= \mxs -> 
        Just(q + mxs)

time (Fragment ((Note x (p,o) :xs))) = (time (Fragment xs)) >>= \mxs -> 
        Just(x + mxs)

time (Concat s1 s2) = (time s1) >>= \ms1 -> 
        (time s2) >>= \ms2 -> 
            Just(ms1 + ms2)

time (Parallel s1 s2) = max (time s1) (time s2)

time s = (unfold s) >>= \ms -> (time ms)


-- Puntos estrellas 

-- Esta funcion devuelve la cantidad de notas (primitive pitch) que tiene un song
songLength :: Song -> Maybe Int

songLength (Fragment xs) = Just(length(xs))

songLength (Parallel s1 s2) = (songLength s1) >>= \ls1 ->
            ((songLength s2) >>= \ls2 -> Just(max ls1 ls2))

songLength (Concat s1 s2) = (songLength s1) >>= \ls1 ->
            ((songLength s2) >>= \ls2 -> Just(ls1 + ls2))

songLength s1 = (unfold s1) >>= \s2 -> (songLength s2)


-- Dados dos songs devuelve la diferencia de tamaño entre ambos
diffLength :: Song -> Song -> Maybe Int

diffLength s1 s2 = songLength(s1) >>= \ls1 -> 
                    (songLength(s2) >>= \ls2 -> 
                    Just(ls1 - ls2))


-- Esta funcion rellena el song que se pasa con i-silencios (Rest 0)
addRest :: Song -> Int -> Song

addRest (Fragment xs) 0 = (Fragment xs)

addRest (Fragment xs) i = (addRest (Fragment ((Rest 0) : xs)) (i-1))


-- Dados 2 songs calcula la diferencia de tamaño y completa el mas pequeño
parallelMax :: Song -> Song -> Maybe Song

parallelMax s1 s2 = (diffLength s1 s2) >>= \lng -> (
    (unfold s1) >>= \s1' -> (
        (unfold s2) >>= \s2' -> (
            
            --Si el largo de los dos es igual, devuelvo las canciones en paralelo
            if (lng == 0) then Just(Parallel s1' s2')
            --Si s1 < s2, relleno la cancion con los silencios que le faltan
            --a s1 para ser igual de larga que s2
            else if (lng < 0) then Just(Parallel (addRest s1' 
                                                    (negate(lng))) s2')
            else Just(Parallel s1' (addRest s2' lng))
            
            )
        )
    )

-- Dado un song y un entero n corta las ultimas n notas.
cutSong :: Song -> Int -> Maybe Song


cutSong (Fragment xs) n | (n < 0) = Nothing
                         | (n >= (length xs)) = Just (Fragment [])
                         | otherwise = Just(Fragment (take (length(xs)-n) xs))

cutSong (Concat s1 s2) n = (songLength s2) >>= \lng -> 
    if (n < lng) then (cutSong s2 n) >>= \cs2 -> Just(Concat s1 cs2)

    else if (n > lng) then cutSong s1 (n - lng)

    else Just(s1)

cutSong (Parallel s1 s2) n = (diffLength s1 s2) >>= \dif -> 

    if dif > 0 then (cutSong s1 n) >>= \cs1 -> 
        (cutSong s2 (n-dif)) >>= \cs2 -> Just(Parallel cs1 cs2)

    else if dif < 0 then (cutSong s1 (n - (negate dif))) >>= \cs1 ->
        (cutSong s2 n) >>= \cs2 -> Just(Parallel cs1 cs2)

    else (cutSong s1 n) >>= \cs1 -> (cutSong s2 n) >>= \cs2 -> 
        Just(Parallel cs1 cs2)



-- Dados 2 songs calcula la diferencia de tamaño y corta el mas grande
parallelMin:: Song -> Song -> Maybe Song

parallelMin s1 s2 = (diffLength s1 s2) >>= \lng -> 
    (unfold s1) >>= \s1' -> 
        (unfold s2) >>= \s2' ->
            --Si son igual de largas ambas canciones, retornamos el paralelo
            if (lng == 0) then Just (Parallel s1' s2')
            --Si s1 > s2, retorno el paralelo de s1 recortado len(s1-s2) tiempos
            else if (lng > 0) then (cutSong s1' lng) >>= \cs1 ->
                Just(Parallel cs1 s2')
            --si s1 < s2, corto -len(s1-s2) a s2 y retorno el paralelo con s1
            else (cutSong s2' (negate lng)) >>= \cs2 ->
                Just(Parallel s1' cs2)

