module MultipleListFilters exposing (boolUnzip, filterWithFilters, reverseIfSort)


filterWithFilters : List (a -> Bool) -> List a -> List a
filterWithFilters lfs ls =
    List.filter (andElements lfs) ls


andElements : List (a -> Bool) -> a -> Bool
andElements lfs x =
    case lfs of
        [] ->
            True

        f :: rest ->
            f x && andElements rest x


boolUnzip : List ( Bool, a ) -> List a
boolUnzip bls =
    case bls of
        [] ->
            []

        ( b, val ) :: rest ->
            case b of
                True ->
                    val :: boolUnzip rest

                False ->
                    boolUnzip rest


reverseIfSort : Bool -> List a -> List a
reverseIfSort b ls =
    case b of
        False ->
            List.reverse ls

        True ->
            ls



-- DATA
--l1 : List String
--l1 =
--    [ "foo", "bar1", "forte", "b1ar", "b17" ]
--f1 : String -> Bool
--f1 =
--    String.contains "fo"
--f2 : String -> Bool
--f2 =
--    String.contains "b"
--f3 : String -> Bool
--f3 =
--    String.contains "b1"
