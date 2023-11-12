module PrintJson where

    import Json
    import Pretty
    import Numeric
    import Data.Bits (shiftR, (.&.))
    import Data.Char (ord)

    renderJvalue :: Jvalue -> Doc

    renderJvalue (Jstring s) = string s
    renderJvalue (Jnumber n) = double n
    renderJvalue (Jbool True) = text "true"
    renderJvalue (Jbool False)  = text "false"
    renderJvalue Jnull = text "null"

    renderJvalue (Jobject o) = series '{' '}' field o
        where field (name, val) = string name
                                Pretty.<> text ": "
                                Pretty.<> renderJvalue val
    
    renderJvalue (Jarray a) = series '[' ']' renderJvalue a

    example = Jarray [Jobject [("foo", Jnumber 24), ("bar", Jbool True)]]

    string :: String -> Doc
    string = enclose '"' '"' . hcat . map oneChar

    enclose :: Char -> Char -> Doc -> Doc
    enclose left right x = char left Pretty.<> x Pretty.<> char right

    oneChar :: Char -> Doc
    oneChar c = case lookup c simpleEscapes of
            Just r -> text r
            Nothing | mustEscape c -> hexEscape c
                    | otherwise    -> char c
        where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'
    
    simpleEscapes :: [(Char, String)]
    simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
                    where ch a b = (a, ['\\',b])

    smallHex :: Int -> Doc
    smallHex x  = text "\\u"
               Pretty.<> text (replicate (4 - length h) '0')
               Pretty.<> text h
            where h = showHex x ""
    
    astral :: Int -> Doc
    astral n = smallHex (a + 0xd800) Pretty.<> smallHex (b + 0xdc00)
            where a = (n `shiftR` 10) .&. 0x3ff
                  b = n .&. 0x3ff

    hexEscape :: Char -> Doc
    hexEscape c | d < 0x10000 = smallHex d
                | otherwise   = astral (d - 0x10000)
            where d = ord c
    
    series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
    series open close item = enclose open close . fsep . punctuate (char ',') . map item