namespace FsMdParser

module Context =
    open FSharpx.Collections
    open FsMdParser

    type ContextComponentStatus =
        | Idle
        | Busy
        | Closing
        | Dead

    type IContextComponent =
        abstract member Get: unit -> obj
        abstract member Status: unit -> ContextComponentStatus
        abstract member Close: unit -> unit
    
    type ContextMap = PersistentHashMap<Tag, IContextComponent>

    let Of(): ContextMap = PersistentHashMap.empty

    let Register tag component' (contextMap: ContextMap) =
        contextMap.Add((tag, component'))

    let Unregister tag (contextMap: ContextMap) =
        contextMap.Remove(tag)
    
    let Close (contextMap: ContextMap) =
        PersistentHashMap.toSeq contextMap
        |> Seq.map (fun c -> (snd c).Close())
        |> Seq.toArray
        |> ignore

    let IsClosed (contextMap: ContextMap) =
        PersistentHashMap.toSeq contextMap
        |> Seq.fold
            (fun acc item -> if (snd item).Status() = Dead then acc else acc + 1)
            0
        |> (=) 0

    let GetComponent tag (contextMap: ContextMap) = contextMap[tag]

    let TryGetComponent tag (contextMap: ContextMap) =
        if contextMap.ContainsKey tag then
            Some contextMap[tag]
        else
            None