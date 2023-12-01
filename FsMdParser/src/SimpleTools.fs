namespace FsMdParser.SimpleTools

module Funs =
    let constant a _ = a

    let lazyConstant fn _ = fn()

    let id a = a

    let bimap fn1 fn2 (a, b) = (fn1 a, fn2 b)

    let eq predicate value = predicate = value