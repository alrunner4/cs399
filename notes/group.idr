
-- interface Eq a where
--	(==) : ( a -> a -> Bool )
--	(/=) : ( a -> a -> Bool )
--	(/=) l r = not ( l == r )

groupBy : Eq a => List a -> List( List a )
groupBy Nil = Nil
groupBy ( x :: Nil ) = [[x]]
groupBy ( head :: next :: tail ) = reverse( snd( foldl accumulate_groups ( head, [[head]] ) ( next :: tail ) ) ) where
    accumulate_groups : ( a, List( List a ) ) -> a -> ( a, List( List a ) )
    accumulate_groups ( current_value, headGroup :: tailGroups ) elem = 
        if current_value == elem
                then ( current_value, ( elem :: headGroup ) :: tailGroups )
                else ( elem, [elem] :: headGroup :: tailGroups )

