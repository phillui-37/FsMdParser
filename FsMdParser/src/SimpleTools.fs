namespace FsMdParser

open System.Runtime.CompilerServices

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

[<Extension>]
type String =
    [<Extension>]
    static let (|Prefix|_|) (p: string) (s: string) =
        if s.StartsWith(p) then Some(s.Substring(p.Length)) else None
    
    [<Extension>]
    static let (|Suffix|_|) (p: string) (s: string) =
            if s.EndsWith p then Some(s.Substring (s.Length - p.Length)) else None
    
    [<Extension>]
    static let (|Surround|_|) (prefix: string) (suffix: string) (s: string) =
        if s.StartsWith(prefix) && s.EndsWith(suffix) then Some(s.Substring(prefix.Length, s.Length - prefix.Length - suffix.Length)) else None

[<RequireQualifiedAccess>]
module List =
    let inline Pos idx (ls: 'a list) = ls[idx]

[<RequireQualifiedAccess>]
module Map =
    let inline Pos key (m: Map<'a, 'b>) = m[key]