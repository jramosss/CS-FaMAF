# Requisitos

#### [Haskell](https://www.haskell.org/) 8.6.5:
* `$ sudo apt-get install haskell-platform`
* `$ sudo apt-get install ghc`

### [Cabal](https://www.haskell.org/cabal/) 2.4
* `$ cabal update`
* `$ cabal install Cabal cabal-install`

### Core Library [Euterpea](http://euterpea.com/) 2.0.4
*  `cabal install Euterpea`

#### Timidity y dependencias :
* `$ sudo apt install timidity`
* `$ sudo apt-get install libasound2-dev`


# Modo de compilación/ejecución

Si quiere escuchar su canción, antes de compilar ejecute el servicio midi Timidity:
* Si no lo tiene instalado:  ``` $ sudo apt install timidity -yes```

Luego ejecute

* `$ timidity -iA -Os`

El programa se compila en la carpeta src, con el siguiente comando:
* `$ make`

Compile como explicamos anteriormente, y genere en terminal su `Song` a escuchar.
* `$ misonido = [Mis notas]`

Luego escuche su sonido con:
* `$ main misonido`

Ahora si por algun motivo su sonido es de tipo `Maybe Song`, escuche su cancion usando:
* `$ main (bindeo misonido)`
