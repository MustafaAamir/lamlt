# Simply typed LC

```
function: λ
type: τ
variable: [A-Za-z]+[0-9]*
c: constant
```
### grammar
expr ::= x | (λx:τ.e) | (e e) | c
base := {TInt, TVar}
Aggregate types := {TArrow(base, base)}

### Rules
```
1. If x has type τ in ctx, then x has type τ
2. Constants have appropriate base types
3. If the type context is provided, i.e. x:τ and e:σ, then x:τ.e can be infered to have the type (τ -> σ)
4. If e:(τ -> σ) and e':σ then (e e'):σ
```

In a polymorphic system, the general base type is 'a, which denotes a polymorphic of the lowest specificity. TInt has a greater specificity than 'a, since it has more constraints.
if e is defined as e:'a.e, and then later applied to e':int, then the type of e is inferred to be (int -> int) instead of ('a -> 'a). This is known as the instantiation rule.


## Algorithm W
```
W: (ctx, expr) -> (subst, type)
```


