module Command where
import Song
import Euterpea --Solo para los ejemplos

data Command a = 
    Add a                           -- agrega un elemento a la cabeza del stack
    | Use1 Int (a -> a)             -- operan y vuelven añadir a la 
    | Use2 Int Int (a -> a -> a)    -- cabeza del stack
    

-- AUXILIARES PARA REPRODUCCION --

-- bind: paso de (maybe song) a (song).

bind :: Maybe Song -> Song

bind s = case s of
                Nothing -> (Fragment [Rest 0])
                Just s -> s
-------

--main: reproduccion del Song mediante playDev

main :: Song -> IO()

main s = case compute s of
                Nothing -> return()
                Just s -> playDev 2 s
-------

run :: [Command a] -> [a] -> Maybe [a]

run [] [] = Nothing

run [] xs = Just xs

run ((Add x) : xs) ys = run xs (x:ys)

run ((Use1 n f) : xs) ys    
        | n >= (length ys) || n < 0 = Nothing
        | otherwise = (run xs ((f (ys!!n)):ys))

run ((Use2 n i f) : xs) ys  
        | (n >= (length ys) || n < 0) || (i >= (length ys) || i < 0) = Nothing
        | otherwise = run xs ((f (ys!!n) (ys!!i)) : ys)


--Ejemplos para testear

song1 :: Song
song1 = Fragment [Note qn (D, 2), Note qn (D, 2), 
                  Note qn (Ds, 2), Note qn (G, 2)]

song3 :: Song
song3 = Transpose_by 1 (Repeat 2 (song1))

song4 :: Song
song4 = Transpose_by (-2) (Repeat 2 (song1))

song5 :: Song
song5 = Fragment[Rest 0]

song6 :: Maybe Song
song6 = cutSong song4 2

sonata :: Music Pitch
sonata = line [c 5 wn, e 5 hn, g 5 hn, b 4 dhn, c 5 en, d 5 en, c 5 hn, rest hn]