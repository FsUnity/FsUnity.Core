﻿(*

Copyright 2013 Jack Pappas

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

*)

namespace FsUnity.Collections

open System.Collections.Generic
open System.Diagnostics
open LanguagePrimitives
open OptimizedClosures
open FsUnity


/// <summary>A bi-directional LongMap.</summary>
/// <typeparam name="Value">The type of the values.</typeparam>
[<Sealed>]
//[<StructuredFormatDisplay("")>]
[<DebuggerDisplay("Count = {Count}")>]
[<DebuggerTypeProxy(typedefof<LongBimapDebuggerProxy<int64>>)>]
type LongBimap<'Value when 'Value : comparison>
    private (map : LongMap<'Value>, inverseMap : Map<'Value, int64>) =
    //
    static let empty = LongBimap (LongMap.Empty, Map.empty)

    //
    static member Empty
        with get () = empty

    //
    member private __.ForwardMap
        with get () = map

    //
    member private __.InverseMap
        with get () = inverseMap

    //
    static member private Equals (left : LongBimap<'Value>, right : LongBimap<'Value>) =
        left.ForwardMap = right.ForwardMap
        && left.InverseMap = right.InverseMap

    //
    static member private Compare (left : LongBimap<'Value>, right : LongBimap<'Value>) =
        match Unchecked.compare left.ForwardMap right.ForwardMap with
        | 0 ->
            compare left.InverseMap right.InverseMap
        | x -> x

    /// <inherit />
    override this.Equals other =
        match other with
        | :? LongBimap<'Value> as other ->
            LongBimap<_>.Equals (this, other)
        | _ ->
            false

    /// <inherit />
    override __.GetHashCode () =
        map.GetHashCode ()

    //
    member __.Count
        with get () =
            LongMap.count map

    //
    member __.IsEmpty
        with get () =
            LongMap.isEmpty map

    //
    member __.ContainsKey key =
        LongMap.containsKey key map

    //
    member __.ContainsValue value =
        Map.containsKey value inverseMap

    //
    member __.Find key =
        LongMap.find key map

    //
    member __.FindValue key =
        Map.find key inverseMap

    //
    member __.Paired (key, value) =
        // NOTE : We only need to check one of the maps, because all
        // Bimap functions maintain the invariant.
        match LongMap.tryFind key map with
        | None ->
            false
        | Some v ->
            v = value

    //
    member this.Remove key =
        // Use the key to find its corresponding value.
        match LongMap.tryFind key map with
        | None ->
            // The key doesn't exist. No changes are needed, so return this Bimap.
            this
        | Some value ->
            // Remove the values from both maps.
            LongBimap (
                LongMap.remove key map,
                Map.remove value inverseMap)

    //
    member this.RemoveValue key =
        // Use the key to find its corresponding value.
        match Map.tryFind key inverseMap with
        | None ->
            // The key doesn't exist. No changes are needed, so return this Bimap.
            this
        | Some value ->
            // Remove the values from both maps.
            LongBimap (
                LongMap.remove value map,
                Map.remove key inverseMap)

    //
    member __.TryFind key =
        LongMap.tryFind key map

    //
    member __.TryFindValue key =
        Map.tryFind key inverseMap

    //
    static member Singleton (key, value) : LongBimap<'Value> =
        LongBimap (
            LongMap.singleton key value,
            Map.singleton value key)

    //
    member this.Add (key, value) =
        // Add the values to both maps.
        // As in Map, we overwrite any existing entry; however, we have to be
        // a bit more thorough here to ensure the invariant is maintained.
        // OPTIMIZE : This could be implemented more efficiently to remove
        // unnecessary or duplicated checks while still maintaining the invariant.
        // It'd also be nice if we could do this in a way that detects if the values
        // are already present and bound to each other, so we don't need to alter the Bimap at all...
        // TODO : Create a private "AddUnsafe" method to avoid the lookups in TryAdd
        this.Remove(key)
            .RemoveValue(value)
            .TryAdd (key, value)

    //
    member this.TryAdd (key, value) =
        // Check that neither value is already bound in the Bimap
        // before adding them; if either already belongs to the map
        // return the original Bimap.
        match LongMap.tryFind key map, Map.tryFind value inverseMap with
        | None, None ->
            LongBimap (
                LongMap.add key value map,
                Map.add value key inverseMap)

        | _, _ ->
            // NOTE : We also return the original map when *both* values already
            // belong to the Bimap and are bound to each other -- because adding
            // them again wouldn't have any effect!
            this

    //
    member __.Exists (predicate : int64 -> 'Value -> bool) : bool =
        LongMap.exists predicate map

    //
    member __.Forall (predicate : int64 -> 'Value -> bool) : bool =
        LongMap.forall predicate map

    //
    member __.Iterate (action : int64 -> 'Value -> unit) : unit =
        LongMap.iter action map

    //
    member __.IterateBack (action : int64 -> 'Value -> unit) : unit =
        LongMap.iterBack action map

    //
    member __.Fold (folder : 'State -> int64 -> 'Value -> 'State, state : 'State) : 'State =
        LongMap.fold folder state map

    //
    member __.FoldBack (folder : int64 -> 'Value -> 'State -> 'State, state : 'State) : 'State =
        LongMap.foldBack folder map state

    //
    member this.Filter (predicate : int64 -> 'Value -> bool) =
        let predicate = FSharpFunc<_,_,_>.Adapt predicate

        this.Fold ((fun (bimap : LongBimap<_>) key value ->
            if predicate.Invoke (key, value) then
                bimap
            else
                bimap.Remove key), this)

    //
    member this.Partition (predicate : int64 -> 'Value -> bool) =
        let predicate = FSharpFunc<_,_,_>.Adapt predicate

        // Partition efficiently by removing elements from the original map
        // and adding them to a new map when the predicate returns false
        // (instead of creating two new maps).
        this.Fold ((fun (trueBimap : LongBimap<_>, falseBimap : LongBimap<_>) key value ->
            if predicate.Invoke (key, value) then
                trueBimap, falseBimap
            else
                trueBimap.Remove key,
                falseBimap.Add (key, value)), (this, empty))

    //
    static member OfSeq (sequence : seq<int64 * 'Value>) : LongBimap<'Value> =
        // Preconditions
        checkNonNull "sequence" sequence

        (empty, sequence)
        ||> Seq.fold (fun bimap (key, value) ->
            bimap.Add (key, value))

    //
    static member OfList list : LongBimap<'Value> =
        // Preconditions
        checkNonNull "list" list

        (empty, list)
        ||> List.fold (fun bimap (key, value) ->
            bimap.Add (key, value))

    //
    static member OfArray array : LongBimap<'Value> =
        // Preconditions
        checkNonNull "array" array

        (empty, array)
        ||> Array.fold (fun bimap (key, value) ->
            bimap.Add (key, value))

    //
    member __.ToSeq () =
        map.ToSeq ()

    //
    member __.ToList () =
        map.ToList ()

    //
    member __.ToArray () =
        map.ToArray ()

    //
    member __.ToLongMap () =
        map

    //
    member internal this.LeftKvpArray () : KeyValuePair<int64, 'Value>[] =
        let elements = ResizeArray (1024)

        this.Iterate <| fun key value ->
            elements.Add (
                KeyValuePair (key, value))

        elements.ToArray ()

    //
    member internal this.RightKvpArray () : KeyValuePair<'Value, int64>[] =
        let elements = ResizeArray (1024)

        this.Iterate <| fun key value ->
            elements.Add (
                KeyValuePair (value, key))

        elements.ToArray ()

    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | :? LongBimap<'Value> as other ->
                LongBimap<_>.Compare (this, other)
            | _ ->
                invalidArg "other" "The object cannot be compared to LongBimap`1."

    interface System.IEquatable<LongBimap<'Value>> with
        member this.Equals other =
            LongBimap<_>.Equals (this, other)

    interface System.IComparable<LongBimap<'Value>> with
        member this.CompareTo other =
            LongBimap<_>.Compare (this, other)

//
and [<Sealed>]
    internal LongBimapDebuggerProxy<'Value when 'Value : comparison> (bimap : LongBimap<'Value>) =

    [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
    member __.Left
        with get () : KeyValuePair<int64, 'Value>[] =
            bimap.LeftKvpArray ()

    [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
    member __.Right
        with get () : KeyValuePair<'Value, int64>[] =
            bimap.RightKvpArray ()


/// Functional programming operators related to the LongBimap type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LongBimap =
    /// The empty LongBimap.
    [<CompiledName("Empty")>]
    let empty<'T when 'T : comparison> : LongBimap<'T> =
        LongBimap<'T>.Empty

    /// The map containing the given binding.
    [<CompiledName("Singleton")>]
    let inline singleton key value : LongBimap<'T> =
        LongBimap<'T>.Singleton (key, value)

    /// Is the map empty?
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (bimap : LongBimap<'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.IsEmpty

    /// Returns the number of bindings in the map.
    [<CompiledName("Count")>]
    let inline count (bimap : LongBimap<'T>) : int64 =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Count

    /// Tests if an element is in the domain of the map.
    [<CompiledName("ContainsKey")>]
    let inline containsKey key (bimap : LongBimap<'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ContainsKey key

    /// Tests if a value is in the range of the map.
    [<CompiledName("ContainsValue")>]
    let inline containsValue value (bimap : LongBimap<'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ContainsValue value

    /// Tests if an element and a value are bound to each other in the map.
    [<CompiledName("Paired")>]
    let inline paired key value (bimap : LongBimap<'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Paired (key, value)

    /// Lookup an element in the map, returning a Some value if the
    /// element is in the domain of the map and None if not.
    [<CompiledName("TryFind")>]
    let inline tryFind key (bimap : LongBimap<'T>) : 'T option =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryFind key

    /// Lookup a value in the map, returning a Some value if the
    /// element is in the range of the map and None if not.
    [<CompiledName("TryFindValue")>]
    let inline tryFindValue value (bimap : LongBimap<'T>) : int64 option =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryFindValue value

    /// Lookup an element in the map, raising KeyNotFoundException
    /// if no binding exists in the map.
    [<CompiledName("Find")>]
    let inline find key (bimap : LongBimap<'T>) : 'T =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Find key

    /// Lookup a value in the map, raising KeyNotFoundException
    /// if no binding exists in the map.
    [<CompiledName("FindValue")>]
    let inline findValue value (bimap : LongBimap<'T>) : int64 =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.FindValue value

    /// Removes an element from the domain of the map.
    /// No exception is raised if the element is not present.
    [<CompiledName("Remove")>]
    let inline remove key (bimap : LongBimap<'T>) : LongBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Remove key

    /// Removes a value from the range of the map.
    /// No exception is raised if the value is not present.
    [<CompiledName("RemoveValue")>]
    let inline removeValue key (bimap : LongBimap<'T>) : LongBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.RemoveValue key

    /// Returns a new map with the binding added to the given map.
    [<CompiledName("Add")>]
    let inline add key value (bimap : LongBimap<'T>) : LongBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Add (key, value)

    /// Returns a new map with the binding added to the given map, but only when
    /// neither the key nor the value are already bound. If the key and/or value
    /// are already bound, the map is returned unchanged.
    [<CompiledName("TryAdd")>]
    let inline tryAdd key value (bimap : LongBimap<'T>) : LongBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryAdd (key, value)

    /// Returns true if the given predicate returns true for one or more bindings in the map.
    [<CompiledName("Exists")>]
    let inline exists (predicate : int64 -> 'T -> bool) (bimap : LongBimap<'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Exists predicate

    /// Returns true if the given predicate returns true for every binding in the map.
    [<CompiledName("Forall")>]
    let inline forall (predicate : int64 -> 'T -> bool) (bimap : LongBimap<'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Forall predicate

    /// Applies the given function to each binding in the map.
    [<CompiledName("Iterate")>]
    let inline iter (action : int64 -> 'T -> unit) (bimap : LongBimap<'T>) : unit =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Iterate action

    /// Applies the given function to each binding in the map.
    [<CompiledName("IterateBack")>]
    let inline iterBack (action : int64 -> 'T -> unit) (bimap : LongBimap<'T>) : unit =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.IterateBack action

    /// Folds over the bindings in the map.
    [<CompiledName("Fold")>]
    let inline fold (folder : 'State -> int64 -> 'T -> 'State)
            (state : 'State) (bimap : LongBimap<'T>) : 'State =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Fold (folder, state)

    /// Folds over the bindings in the map.
    [<CompiledName("FoldBack")>]
    let inline foldBack (folder : int64 -> 'T -> 'State -> 'State)
            (bimap : LongBimap<'T>) (state : 'State) : 'State =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.FoldBack (folder, state)

    /// Builds a new map containing only the bindings for which
    /// the given predicate returns 'true'.
    [<CompiledName("Filter")>]
    let inline filter (predicate : int64 -> 'T -> bool) (bimap : LongBimap<'T>) : LongBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Filter predicate

    /// Builds two new maps, one containing the bindings for which the given predicate
    /// returns 'true', and the other the remaining bindings.
    [<CompiledName("Partition")>]
    let inline partition (predicate : int64 -> 'T -> bool) (bimap : LongBimap<'T>)
            : LongBimap<'T> * LongBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Partition predicate

    /// Returns a new map made from the given bindings.
    [<CompiledName("OfSeq")>]
    let inline ofSeq (sequence : seq<int64 * 'T>) : LongBimap<'T> =
        // Preconditions checked by the member.
        LongBimap<'T>.OfSeq sequence

    /// Returns a new map made from the given bindings.
    [<CompiledName("OfList")>]
    let inline ofList list : LongBimap<'T> =
        // Preconditions checked by the member.
        LongBimap<_>.OfList list

    /// Returns a new map made from the given bindings.
    [<CompiledName("OfArray")>]
    let inline ofArray array : LongBimap<'T> =
        // Preconditions checked by the member.
        LongBimap<_>.OfArray array

    /// Views the collection as an enumerable sequence of pairs.
    /// The sequence will be ordered by the keys of the map.
    [<CompiledName("ToSeq")>]
    let inline toSeq (bimap : LongBimap<'T>) : seq<int64 * 'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToSeq ()

    /// Returns a list of all key-value pairs in the mapping.
    /// The list will be ordered by the keys of the map.
    [<CompiledName("ToList")>]
    let inline toList (bimap : LongBimap<'T>) : (int64 * 'T) list =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToList ()

    /// Returns an array of all key-value pairs in the mapping.
    /// The array will be ordered by the keys of the map.
    [<CompiledName("ToArray")>]
    let inline toArray (bimap : LongBimap<'T>) : (int64 * 'T)[] =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToArray ()

    /// Returns an LongMap with the same key-value pairs as the LongBimap.
    [<CompiledName("ToLongMap")>]
    let inline toLongMap (bimap : LongBimap<'T>) : LongMap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToLongMap ()


#if PROTO_COMPILER

/// <summary>A bi-directional TagMap.</summary>
/// <typeparam name="Tag">The tag (measure) type of the keys.</typeparam>
/// <typeparam name="Value">The type of the values.</typeparam>
[<MeasureAnnotatedAbbreviation>]
type LongTagBimap< [<Measure>] 'Tag, 'Value when 'Value : comparison > = LongBimap<'Value>

/// Functional programming operators related to the LongTagBimap<_,_> type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LongTagBimap =
    /// Retypes a value without emitting any IL instructions.
    /// WARNING: This should be used with EXTREME CAUTION.
    [<NoDynamicInvocation>]
    [<CompiledName("RetypeInlined")>]
    let inline private retype<'T,'U> (x:'T) : 'U = (# "" x : 'U #)

    /// The empty LongTagBimap.
    [<CompiledName("Empty")>]
    let empty<[<Measure>]'Tag, 'T when 'T : comparison> : LongTagBimap<'Tag, 'T> =
        retype LongBimap<'T>.Empty

    /// The map containing the given binding.
    [<CompiledName("Singleton")>]
    let inline singleton (key : int64<'Tag>) value : LongTagBimap<'Tag, 'T> =
        LongBimap.Singleton (retype key, value)
        |> retype

    /// Is the map empty?
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (bimap : LongTagBimap<'Tag, 'T>) : bool =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.IsEmpty

    /// Returns the number of bindings in the map.
    [<CompiledName("Count")>]
    let inline count (bimap : LongTagBimap<'Tag, 'T>) : int64 =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Count

    /// Tests if an element is in the domain of the map.
    [<CompiledName("ContainsKey")>]
    let inline containsKey (key : int64<'Tag>) (bimap : LongTagBimap<'Tag, 'T>) : bool =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap
        
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ContainsKey (retype key)

    /// Tests if a value is in the range of the map.
    [<CompiledName("ContainsValue")>]
    let inline containsValue key (bimap : LongTagBimap<'Tag, 'T>) : bool =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ContainsValue key

    /// Tests if an element and a value are bound to each other in the map.
    [<CompiledName("Paired")>]
    let inline paired (key : int64<'Tag>) value (bimap : LongTagBimap<'Tag, 'T>) : bool =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Paired (retype key, value)

    /// Lookup an element in the map, returning a Some value if the
    /// element is in the domain of the map and None if not.
    [<CompiledName("TryFind")>]
    let inline tryFind (key : int64<'Tag>) (bimap : LongTagBimap<'Tag, 'T>) : 'T option =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryFind (retype key)

    /// Lookup a value in the map, returning a Some value if the
    /// element is in the range of the map and None if not.
    [<CompiledName("TryFindValue")>]
    let inline tryFindValue key (bimap : LongTagBimap<'Tag, 'T>) : int64<'Tag> option =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryFindValue key
        |> retype

    /// Lookup an element in the map, raising KeyNotFoundException
    /// if no binding exists in the map.
    [<CompiledName("Find")>]
    let inline find (key : int64<'Tag>) (bimap : LongTagBimap<'Tag, 'T>) : 'T =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap
        
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Find (retype key)

    /// Lookup a value in the map, raising KeyNotFoundException
    /// if no binding exists in the map.
    [<CompiledName("FindValue")>]
    let inline findValue key (bimap : LongTagBimap<'Tag, 'T>) : int64<'Tag> =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.FindValue key
        |> retype

    /// Removes an element from the domain of the map.
    /// No exception is raised if the element is not present.
    [<CompiledName("Remove")>]
    let inline remove (key : int64<'Tag>) (bimap : LongTagBimap<'Tag, 'T>) : LongTagBimap<'Tag, 'T> =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Remove (retype key)
        |> retype

    /// Removes a value from the range of the map.
    /// No exception is raised if the value is not present.
    [<CompiledName("RemoveValue")>]
    let inline removeValue key (bimap : LongTagBimap<'Tag, 'T>) : LongTagBimap<'Tag, 'T> =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.RemoveValue key
        |> retype

    /// Returns a new map with the binding added to the given map.
    [<CompiledName("Add")>]
    let inline add (key : int64<'Tag>) value (bimap : LongTagBimap<'Tag, 'T>) : LongTagBimap<'Tag, 'T> =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Add (retype key, value)
        |> retype

    /// Returns a new map with the binding added to the given map, but only when
    /// neither the key nor the value are already bound. If the key and/or value
    /// are already bound, the map is returned unchanged.
    [<CompiledName("TryAdd")>]
    let inline tryAdd (key : int64<'Tag>) value (bimap : LongTagBimap<'Tag, 'T>) : LongTagBimap<'Tag, 'T> =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryAdd (retype key, value)
        |> retype

    /// Returns true if the given predicate returns true for one or more bindings in the map.
    [<CompiledName("Exists")>]
    let inline exists (predicate : int64<'Tag> -> 'T -> bool) (bimap : LongTagBimap<'Tag, 'T>) : bool =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Exists (retype predicate)

    /// Returns true if the given predicate returns true for every binding in the map.
    [<CompiledName("Forall")>]
    let inline forall (predicate : int64<'Tag> -> 'T -> bool) (bimap : LongTagBimap<'Tag, 'T>) : bool =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Forall (retype predicate)

    /// Applies the given function to each binding in the map.
    [<CompiledName("Iterate")>]
    let inline iter (action : int64<'Tag> -> 'T -> unit) (bimap : LongTagBimap<'Tag, 'T>) : unit =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Iterate (retype action)

    /// Applies the given function to each binding in the map.
    [<CompiledName("IterateBack")>]
    let inline iterBack (action : int64<'Tag> -> 'T -> unit) (bimap : LongTagBimap<'Tag, 'T>) : unit =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap
        
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.IterateBack (retype action)

    /// Folds over the bindings in the map.
    [<CompiledName("Fold")>]
    let inline fold (folder : 'State -> int64<'Tag> -> 'T -> 'State)
            (state : 'State) (bimap : LongTagBimap<'Tag, 'T>) : 'State =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Fold (retype folder, state)

    /// Folds over the bindings in the map.
    [<CompiledName("FoldBack")>]
    let inline foldBack (folder : int64<'Tag> -> 'T -> 'State -> 'State)
            (bimap : LongTagBimap<'Tag, 'T>) (state : 'State) : 'State =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.FoldBack (retype folder, state)

    /// Builds a new map containing only the bindings for which
    /// the given predicate returns 'true'.
    [<CompiledName("Filter")>]
    let inline filter (predicate : int64<'Tag> -> 'T -> bool) (bimap : LongTagBimap<'Tag, 'T>) : LongTagBimap<'Tag, 'T> =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Filter (retype predicate)
        |> retype

    /// Builds two new maps, one containing the bindings for which the given predicate
    /// returns 'true', and the other the remaining bindings.
    [<CompiledName("Partition")>]
    let inline partition (predicate : int64<'Tag> -> 'T -> bool) (bimap : LongTagBimap<'Tag, 'T>)
            : LongTagBimap<'Tag, 'T> * LongTagBimap<'Tag, 'T> =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        let trueMap, falseMap = bimap.Partition (retype predicate)
        (retype trueMap), (retype falseMap)

    /// Returns a new map made from the given bindings.
    [<CompiledName("OfSeq")>]
    let inline ofSeq (sequence : seq<int64<'Tag> * 'T>) : LongTagBimap<'Tag, 'T> =
        // Preconditions
        checkNonNull "sequence" sequence

        LongBimap<_>.OfSeq (retype sequence)
        |> retype

    /// Returns a new map made from the given bindings.
    [<CompiledName("OfList")>]
    let inline ofList (list : (int64<'Tag> * 'T) list) : LongTagBimap<'Tag, 'T> =
        // Preconditions
        checkNonNull "list" list

        LongBimap<_>.OfList (retype list)
        |> retype

    /// Returns a new map made from the given bindings.
    [<CompiledName("OfArray")>]
    let inline ofArray (array : (int64<'Tag> * 'T)[]) : LongTagBimap<'Tag, 'T> =
        // Preconditions
        checkNonNull "array" array

        LongBimap<_>.OfArray (retype array)
        |> retype

    /// Views the collection as an enumerable sequence of pairs.
    /// The sequence will be ordered by the keys of the map.
    [<CompiledName("ToSeq")>]
    let inline toSeq (bimap : LongTagBimap<'Tag, 'T>) : seq<int64<'Tag> * 'T> =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToSeq ()
        |> retype

    /// Returns a list of all key-value pairs in the mapping.
    /// The list will be ordered by the keys of the map.
    [<CompiledName("ToList")>]
    let inline toList (bimap : LongTagBimap<'Tag, 'T>) : (int64<'Tag> * 'T) list =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToList ()
        |> retype

    /// Returns an array of all key-value pairs in the mapping.
    /// The array will be ordered by the keys of the map.
    [<CompiledName("ToArray")>]
    let inline toArray (bimap : LongTagBimap<'Tag, 'T>) : (int64<'Tag> * 'T)[] =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToArray ()
        |> retype

    /// Returns a TagMap with the same key-value pairs as the LongTagBimap.
    [<CompiledName("ToTagMap")>]
    let inline toTagMap (bimap : LongTagBimap<'Tag, 'T>) : TagMap<'Tag, 'T> =
        // Retype as LongBimap.
        let bimap : LongBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToLongMap ()
        |> retype

#endif
