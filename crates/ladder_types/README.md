A wrapper-type for the phenomenon of subtracting two points resulting in a vector rather than a point.

This crate differs from other approaches (eg euclid) in that rather than the difference of two vectors being a vector, it instead reaches a third level, and so on.

Statically checked

`ThingWithRelativity<Contents, Level=0> ==> T<0>`

`T<0> - T<0> ==> T<1>`
`T<1> - T<1> ==> T<2>`
`T<N> - T<N> ==> T<N+1>`

`T<0> +/- T<1> ==> T<0>`
`T<N> +/- T<N+1> ==> T<0>`

`T<N> + T<M>, where N!=M ==> INVALID`
`T<N> - T<M>, where N!=M and N!=M+1 ==> INVALID`

May have another static parameter for allowing opt-in behavior.  eg allowing `ThingWithRelativity<i32, Level=0>::new(5) + 7`
- Assume no relativity implies level of 0, or whatever the other level is (or one higher?)?
- Maybe an enum for default relativity level?  Options: Same, PlusOne, Zero, Disallow
