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
in let x = $f.f the variable x has a value $f.f though it isn't specified, $f.f has a type of ('a -> 'a), which means it's a polymorphic function that takes 'a and returns 'a. 'a is a placeholder for a general type value that can be anything. when you apply this abstraction to something, like Int 42, then it looks for a type with more constraints ('a is the most general, Int is way way more specific). So it chooses Int as the type for $f.f and the type changes from ('a -> 'a) to (int -> int) and remains this way throughout. If you applied $f.f $e.e, it would get the type (('a -> 'a) -> ('a -> 'a))

