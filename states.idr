
Stateful : Type -> Type -> Type -> Type
Stateful a b c = ( a , b ) -> ( c -> ( a , b ) )

--         m a = ( a , b )
--         b = List( List a )
-- (>>=) : Monad m => m a -> ( a -> m b ) -> m b



chain : Stateful a b c -> Stateful a b c -> Stateful a b c
chain fn1 fn2 = \ (x,y), z =>
	let (x1,y1) = fn1 (x,y) z
	in fn2 (x1,y1) z

data State s a = StateOperation( s -> ( s, a ) )


Applicative( State s ) where { }

-- m = State s
-- m a = State s a
-- ( a -> m b ) = ( a -> State s b )
-- m b = State s b
Monad( State s ) where
	
	(>>=)( StateOperation fn1 ) fn2 = StateOperation(\ state =>
	--      VVVVVVVVV : ( s, a )
		let ( state1, out1 ) = fn1 state
	--          VVVVVVVV : State s b  aka s -> ( s,b )
		    StateOperation fn2_internal = fn2 out1
	--                         ^^^^^^^^^^^^ : s -> ( s,b )
		in fn2_internal state1
	)


-- map : Functor f => (a -> b) -> f a -> f b
-- f = State s
-- mapFn : a -> b
-- State s b
-- fn1 : s -> ( s , a )

Functor( State s ) where
	map mapFn ( StateOperation fn1 ) = StateOperation(
		\ state => let ( state1, out1 ) = fn1 state
		--                                ^^^^^^^^^ : ( s , a )
			in ( state1, mapFn out1 )
	)

increment : State Int Bool
increment = StateOperation( \ x => ( x+1 , mod(x+1) 2 == 0 ) )

runState : State s a -> s -> ( s , a )
runState( StateOperation op ) input = op input

chainState : State s a -> State s a -> State s a
chainState( StateOperation fn1 )( StateOperation fn2 ) = StateOperation(
	\ state => let (state1,out1) = fn1 state in fn2 state1
)


doStates : State Int Bool
doStates = do
	x <- increment
	y <- increment
	pure y

