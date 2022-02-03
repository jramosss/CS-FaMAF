module HGUI.Config where

import Graphics.UI.Gtk


-- Para definir el lenguaje del resaltado

languageSpecFolder, languageSpecFunFile, anotherLanguage :: String
textStylesFolder, textStyleFileFun, funMimeType :: String
languageSpecHalFile, textStyleFileHal, halMimeType :: String

languageSpecFolder = "HGUI/sourceview/language-specs"

languageSpecFunFile = languageSpecFolder ++ "/fun.lang"

anotherLanguage = languageSpecFolder ++ "/haskell.lang"

textStylesFolder = "HGUI/sourceview/styles"

textStyleFileFun = textStylesFolder ++ "/fun.xml"

funMimeType = "text/fun"

languageSpecHalFile = languageSpecFolder ++ "/lisa.lang"

textStyleFileHal = textStylesFolder ++ "/lisa.xml"

halMimeType = "text/lisa"

executableFun :: String
executableFun = "fun-gui"

data LangInfo = LangInfo {
                    specFile :: String
                  , styleFile :: String
                  , mimeType :: String
                  , langName :: String
}

funLangInfo :: LangInfo
funLangInfo = LangInfo languageSpecFunFile textStyleFileFun funMimeType "fun"

halLangInfo :: LangInfo
halLangInfo = LangInfo languageSpecHalFile textStyleFileHal halMimeType "lisa"

breakMark :: String
breakMark = "break"

defToInputMsg :: String
defToInputMsg = "Esta vardef tal vez podría ser varinput."

guardFalseMsg :: String
guardFalseMsg = "La guarda no es verdadera."

guardTypeErrorMsg :: String
guardTypeErrorMsg = "La guarda no tiene sentido."

abortMsg :: String
abortMsg = "Ha ocurrido un abort."

evalContDelay :: Int
evalContDelay = 750000 --microsegundos

formatErrorMsg :: String -> String
formatErrorMsg msg = "<span foreground=\"red\">"++msg++"</span>"

formatOutputMsg :: String -> String
formatOutputMsg msg = "<span foreground=\"blue\">"++msg++"</span>"

evalLineColour :: String
evalLineColour = "#ee351de96116"

evalCommColour :: String
evalCommColour = "lightgray"

-- Para el identado:

funIdentWidth :: Int
funIdentWidth = 4

spacesInsteadTab, setIndentOnTab, autoIdent :: Bool
spacesInsteadTab = True
setIndentOnTab = True
autoIdent = True


-- Colores

backColorCommTV :: Color
backColorCommTV = Color 8000 8000 8000

textColorCommTV :: Color
textColorCommTV = Color 60000 60000 60000

textErrColorCommTV :: Color
textErrColorCommTV = Color 60000 20000 20000

scrollInc :: Double
scrollInc = 10.0

scrollDec :: Double
scrollDec = - scrollInc
