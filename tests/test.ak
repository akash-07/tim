f y = 3 * y + 2/3 ;
g x y = let z = x in z;
h x = case (let y = x in y) of
		<1> -> g f 2 ;
		<2> -> 5