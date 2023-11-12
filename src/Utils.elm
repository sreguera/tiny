module Utils exposing 
    ( span
    , combine
    , flip
    )

span : (Char -> Bool) -> String -> (String, String)
span pred s =
    case String.uncons s of
        Just (h, t) ->
            if pred h then
                let 
                    (take, drop) = span pred t
                in
                (String.cons h take, drop)
            else
                ("", s)
        Nothing ->
            ("", s)


-- also in Result.Extra
combine : List (Result x a) -> Result x (List a)
combine l =
    List.foldr (Result.map2 (::)) (Ok []) l


-- also in Basics.Extra
flip : (a -> b -> c) -> b -> a -> c
flip f a b = f b a
