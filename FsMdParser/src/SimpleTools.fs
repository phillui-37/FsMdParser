namespace FsMdParser.SimpleTools

module Funs =
    let constant a _ = a

    let lazyConstant fn _ = fn()

    let id a = a

    let bimap fn1 fn2 (a, b) = (fn1 a, fn2 b)

    let eq predicate value = predicate = value

    let ne predicate value = predicate <> value

    let gt predicate value = value > predicate
    
    let ge predicate value = value >= predicate

    let le predicate value = value <= predicate

    let lt predicate value = value < predicate

    let hasFlag expected testee = testee &&& expected <> 0
    
    let not' fn = fn() |> not
    
    let flip fn a b = fn b a

    let curry fn a b = fn((a,b))

    let uncurry fn (a, b) = fn a b

    let and' predicates testee =
        predicates |> Seq.fold (fun acc predicate -> acc && predicate testee) true