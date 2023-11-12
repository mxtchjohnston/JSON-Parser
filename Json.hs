module Json where

    data Jvalue = Jstring String
                | Jnumber Double
                | Jbool Bool
                | Jnull
                | Jobject [(String, Jvalue)]
                | Jarray [Jvalue]
                deriving (Eq, Ord, Show)

    getString :: Jvalue -> Maybe String
    getString (Jstring s) = Just s
    getString _ = Nothing

    getInt (Jnumber n) = Just $ truncate n
    getInt _ = Nothing

    getDouble (Jnumber d) = Just d
    getDouble _ = Nothing

    getBool (Jbool b) = Just b
    getBool _ = Nothing

    getObject (Jobject o) = Just o
    getObject _ = Nothing

    getArray (Jarray a) = Just a
    getArray _ = Nothing

    isNull v = v == Jnull