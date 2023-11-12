module Pretty where
    import Json
    import Prelude hiding ((<>))

    data Doc = Empty
             | Char Char
             | Text String
             | Line
             | Concat Doc Doc
             | Union Doc Doc
             deriving (Show, Eq)
    
    empty :: Doc
    empty = Empty 

    text :: String -> Doc
    text "" = Empty
    text t = Text t

    double :: Double -> Doc
    double n = text (show n)

    char :: Char -> Doc
    char c = Char c

    line :: Doc
    line = Line

    (<>) :: Doc -> Doc -> Doc
    Empty <> y = y
    x <> Empty = x
    a <> b = a `Concat` b

    hcat :: [Doc] -> Doc
    hcat = fold (<>)

    fsep :: [Doc] -> Doc
    fsep = fold (</>)

    fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
    fold f = foldr f empty

    (</>) :: Doc -> Doc -> Doc
    x </> y = x <> softline <> y

    softline :: Doc
    softline = group line

    group :: Doc -> Doc
    group x = flatten x `Union` x

    flatten :: Doc -> Doc
    flatten (x `Concat` y) = flatten x `Concat` flatten y
    flatten Line           = Char ' '
    flatten (x `Union` _)  = flatten x
    flatten other          = other

    compact :: Doc -> String
    compact x = transform [x]
        where transform [] = ""
              transform (d:ds) =
                  case d of
                      Empty        -> transform ds
                      Char c       -> c : transform ds
                      Text s       -> s ++ transform ds
                      Line         -> '\n' : transform ds
                      a `Concat` b -> transform (a:b:ds)
                      _ `Union` b  -> transform (b:ds)

    punctuate :: Doc -> [Doc] -> [Doc]
    punctuate p [] = []
    punctuate p [d] = [d]
    punctuate p (d:ds) = (d <> p) : punctuate p ds