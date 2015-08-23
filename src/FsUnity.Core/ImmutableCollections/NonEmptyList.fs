﻿namespace FsUnity.Collections.Immutable

open System
open System.Collections
open System.Collections.Generic
open System.Runtime.CompilerServices
open FsUnity

type NonEmptyList<'T> = 
    { Head: 'T
      Tail: 'T list }

    member x.Length = x.Tail.Length + 1

    interface IEnumerable<'T> with
        member x.GetEnumerator() = 
            let e = seq {
                yield x.Head
                yield! x.Tail }
            e.GetEnumerator()
    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = (x :> _ seq).GetEnumerator() :> IEnumerator

[<Extension>]
module NonEmptyList =
    [<CompiledName("Create")>]
    let inline create head tail = { Head = head; Tail = tail }

    [<CompiledName("Create")>]
    let inline createParamsArray(head, [<ParamArray>] tail) = { Head = head; Tail = Array.toList tail }

    [<CompiledName("Singleton")>]
    let inline singleton value = create value []

    [<CompiledName("Head")>]
    let inline head (x: NonEmptyList<_>) = x.Head

    [<CompiledName("Tail")>]
    let inline tail (x: NonEmptyList<_>) = x.Tail

    [<CompiledName("ToFSharpList")>]
    [<Extension>]
    let inline toList (x: NonEmptyList<_>) = x.Head :: x.Tail

    [<CompiledName("Length")>]
    let inline length (x: NonEmptyList<_>) = x.Length

    [<CompiledName("ToArray")>]
    [<Extension>]
    let toArray list =
        let r = Array.zeroCreate (length list)
        r.[0] <- head list
        let rec loop i = 
            function
            | [] -> ()
            | h::t -> 
                r.[i] <- h
                loop (i+1) t
        loop 1 (tail list)
        r

    [<CompiledName("AsEnumerable")>]
    [<Extension>]
    let toSeq (list: NonEmptyList<_>) = list :> _ seq

    [<CompiledName("Select")>]
    let map f list = 
        let newHead = f (head list)
        let newTail = List.map f (tail list)
        create newHead newTail
    
    [<CompiledName("Cons")>]
    let cons head tail =        
        create head (toList tail)

    [<CompiledName("Concat")>]
    let appendList list1 list2 = 
        create (head list1) (tail list1 @ list2)
            
    [<CompiledName("Concat")>]
    let inline append list1 list2 = 
        appendList list1 (toList list2)

    [<CompiledName("Aggregate")>]
    let inline reduce reduction list =
        List.fold reduction (head list) (tail list)

    [<CompiledName("Last")>]
    let inline last list = 
        reduce ((fun x _ -> x) id) list

    [<CompiledName("Reverse")>]
    [<Extension>]
    let rev list =
        List.fold (fun a b -> cons b a) (singleton (head list)) (tail list)

    [<CompiledName("SelectMany")>]
    let collect mapping list =
        List.fold (fun s e -> mapping e |> append s) (mapping (head list)) (tail list)