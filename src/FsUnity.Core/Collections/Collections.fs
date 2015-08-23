(*

Copyright 2010-2012 TidePowerd Ltd.
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

open System
open System.Linq
open System.Collections
open System.Collections.Generic
open System.Runtime.CompilerServices
open LanguagePrimitives
open OptimizedClosures
open FsUnity


/// <summary></summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="Key"></typeparam>
/// <typeparam name="T"></typeparam>
type IMapFolder<'State, 'Key, 'T> =
    /// <summary></summary>
    /// <param name="key"></param>
    /// <returns></returns>
    abstract Map : key:'Key -> 'T

    /// <summary></summary>
    /// <param name="state"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    abstract Fold : state:'State -> value:'T -> 'State

//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MapFolder =
    /// <summary></summary>
    /// <param name="mapping"></param>
    /// <param name="folder"></param>
    /// <returns></returns>
    [<CompiledName("FromFunctions")>]
    let inline fromFunctions (mapping : 'Key -> 'T) (folder : 'State -> 'T -> 'State) =
        { new IMapFolder<'State, 'Key, 'T> with
            member __.Map (key : 'Key) : 'T =
                mapping key
            member __.Fold (state : 'State) (value : 'T) : 'State =
                folder state value }

    /// <summary></summary>
    /// <param name="mapping"></param>
    /// <param name="folder"></param>
    /// <returns></returns>
    [<CompiledName("FromFunctions")>]
    let inline fromFunctionsTupled (mapping : 'Key -> 'T) (folder : 'State * 'T -> 'State) =
        { new IMapFolder<'State, 'Key, 'T> with
            member __.Map (key : 'Key) : 'T =
                mapping key
            member __.Fold (state : 'State) (value : 'T) : 'State =
                folder (state, value) }


/// <summary></summary>
/// <typeparam name="Key"></typeparam>
/// <typeparam name="T"></typeparam>
type IMapReduction<'Key, 'T> =
    /// <summary></summary>
    /// <param name="key"></param>
    /// <returns></returns>
    abstract Map : key:'Key -> 'T

    /// <summary></summary>
    /// <param name="value1"></param>
    /// <param name="value2"></param>
    /// <returns></returns>
    abstract Reduce : value1:'T -> value2:'T -> 'T

//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MapReduction =
    /// <summary></summary>
    /// <param name="mapping"></param>
    /// <param name="reduction"></param>
    /// <returns></returns>
    [<CompiledName("FromFunctions")>]
    let inline fromFunctions (mapping : 'Key -> 'T) (reduction : 'T -> 'T -> 'T) =
        { new IMapReduction<'Key, 'T> with
            member __.Map (key : 'Key) : 'T =
                mapping key
            member __.Reduce (value1 : 'T) (value2 : 'T) : 'T =
                reduction value1 value2 }

    /// <summary></summary>
    /// <param name="mapping"></param>
    /// <param name="reduction"></param>
    /// <returns></returns>
    [<CompiledName("FromFunctions")>]
    let inline fromFunctionsTupled (mapping : 'Key -> 'T) (reduction : 'T * 'T -> 'T) =
        { new IMapReduction<'Key, 'T> with
            member __.Map (key : 'Key) : 'T =
                mapping key
            member __.Reduce (value1 : 'T) (value2 : 'T) : 'T =
                reduction (value1, value2) }


/// Functional operators over a range of values.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Range =
    open LanguagePrimitives

    (* NOTE :   In the functions below, 'start' and 'finish' are *inclusive*, just like the F# 'for' loop. *)

    /// <summary></summary>
    /// <param name="action"></param>
    /// <param name="start"></param>
    /// <param name="finish"></param>
    /// <remarks></remarks>
    [<CompiledName("Iterate")>]
    let inline iter (action : ^T -> unit) start finish : unit =
        let mutable index = start
        while index <= finish do
            action index
            index <- index + GenericOne

    /// <summary></summary>
    /// <param name="folder"></param>
    /// <param name="start"></param>
    /// <param name="finish"></param>
    /// <param name="state"></param>
    /// <remarks></remarks>
    [<CompiledName("Fold")>]
    let inline fold (folder : ^State -> ^T -> ^State) start finish state : ^State =
        let mutable state = state
        let mutable index = start
        while index <= finish do
            state <- folder state index
            index <- index + GenericOne
        state

    /// <summary></summary>
    /// <param name="folder"></param>
    /// <param name="start"></param>
    /// <param name="finish"></param>
    /// <param name="state"></param>
    /// <remarks></remarks>
    [<CompiledName("FoldBack")>]
    let inline foldBack (folder : ^T -> ^State -> ^State) start finish state : ^State =
        let mutable state = state
        let mutable index = finish
        while index >= start do
            state <- folder index state
            index <- index - GenericOne
        state

    /// <summary></summary>
    /// <param name="predicate"></param>
    /// <param name="start"></param>
    /// <param name="finish"></param>
    /// <remarks></remarks>
    [<CompiledName("Exists")>]
    let inline exists (predicate : ^T -> bool) start finish : bool =
        let mutable foundMatch = false
        let mutable index = start
        while index <= finish && not foundMatch do
            foundMatch <- predicate index
            index <- index + GenericOne
        foundMatch

    /// <summary></summary>
    /// <param name="predicate"></param>
    /// <param name="start"></param>
    /// <param name="finish"></param>
    /// <remarks></remarks>
    [<CompiledName("Forall")>]
    let inline forall (predicate : ^T -> bool) start finish : bool =
        let mutable allMatch = true
        let mutable index = start
        while index <= finish && allMatch do
            allMatch <- predicate index
            index <- index + GenericOne
        allMatch

    // TODO
    // mapReduce


/// <summary>
/// Functional programming operators related to the <see cref="System.Collections.Generic.KeyValuePair`2"/> type.
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module KeyValuePair =
    open System.Collections.Generic

    /// <summary>Gets the key from the key/value pair.
    [<CompiledName("Key")>]
    let inline key (kvp : KeyValuePair<'Key, 'T>) =
        kvp.Key

    /// <summary>Gets the value in the key/value pair.
    [<CompiledName("Value")>]
    let inline value (kvp : KeyValuePair<'Key, 'T>) =
        kvp.Value

    /// <summary>
    /// Transforms the value in a key/value pair by applying the specified function to it. The key passed to the function
    /// indicates the key of the value being transformed. This function is analogous to <see cref="Map.map"/>.
    /// </summary>
    [<CompiledName("Map")>]
    let map (mapping : 'Key -> 'T -> 'U) (kvp : KeyValuePair<'Key, 'T>) : KeyValuePair<'Key, 'U> =
        KeyValuePair (kvp.Key, mapping kvp.Key kvp.Value)

module NameValueCollection =
    open System.Collections.Specialized
    open System.Linq

    /// <summary>
    /// Returns a new <see cref="NameValueCollection"/> with the concatenation of two <see cref="NameValueCollection"/>s
    /// </summary>
    /// <param name="a"></param>
    /// <param name="b"></param>
    [<Extension>]
    [<CompiledName("Concat")>]
    let concat a b = 
        let x = NameValueCollection()
        x.Add a
        x.Add b
        x

    /// <summary>
    /// In-place add of a key-value pair to a <see cref="NameValueCollection"/>
    /// </summary>
    /// <param name="x"></param>
    /// <param name="a"></param>
    /// <param name="b"></param>
    let inline addInPlace (x: NameValueCollection) (a,b) = x.Add(a,b)

    /// Adds an element to a copy of an existing NameValueCollection
    let add name value (x: NameValueCollection) =
        let r = NameValueCollection x
        r.Add(name,value)
        r

    /// <summary>
    /// Returns a <see cref="NameValueCollection"/> as an array of key-value pairs.
    /// Note that keys may be duplicated.
    /// </summary>
    /// <param name="a"></param>
    [<Extension>]
    [<CompiledName("ToArray")>]
    let toArray (a: NameValueCollection) =
        a.AllKeys
        |> Array.collect (fun k -> a.GetValues k |> Array.map (fun v -> k,v))

    /// <summary>
    /// Returns a <see cref="NameValueCollection"/> as a sequence of key-value pairs.
    /// Note that keys may be duplicated.
    /// </summary>
    /// <param name="a"></param>
    [<Extension>]
    [<CompiledName("ToEnumerable")>]
    let toSeq (a: NameValueCollection) =
        a.AllKeys
        |> Seq.collect (fun k -> a.GetValues k |> Seq.map (fun v -> k,v))

    /// <summary>
    /// Returns a <see cref="NameValueCollection"/> as a list of key-value pairs.
    /// Note that keys may be duplicated.
    /// </summary>
    /// <param name="a"></param>
    let inline toList a = toSeq a |> Seq.toList

    /// <summary>
    /// Creates a <see cref="NameValueCollection"/> from a list of key-value pairs
    /// </summary>
    /// <param name="l"></param>
    let ofSeq l =
        let x = NameValueCollection()
        Seq.iter (addInPlace x) l
        x

    [<Extension>]
    [<CompiledName("ToLookup")>]
    let toLookup a =
        let s = toSeq a
        s.ToLookup(fst, snd)

    [<Extension>]
    [<CompiledName("AsDictionary")>]
    let asDictionary (x: NameValueCollection) =
        let notimpl() = raise <| NotImplementedException()
        let getEnumerator() =
            let enum = x.GetEnumerator()
            let wrapElem (o: obj) = 
                let key = o :?> string
                let values = x.GetValues key
                KeyValuePair(key,values)
            { new IEnumerator<KeyValuePair<string,string[]>> with
                member e.Current = wrapElem enum.Current
                member e.MoveNext() = enum.MoveNext()
                member e.Reset() = enum.Reset()
                member e.Dispose() = ()
                member e.Current = box (wrapElem enum.Current) }
        { new IDictionary<string,string[]> with
            member d.Count = x.Count
            member d.IsReadOnly = false 
            member d.Item 
                with get k = 
                    let v = x.GetValues k
                    if v = null
                        then raise <| KeyNotFoundException(sprintf "Key '%s' not found" k)
                        else v
                and set k v =
                    x.Remove k
                    for i in v do
                        x.Add(k,i)
            member d.Keys = upcast ResizeArray<string>(x.Keys |> Seq.cast)
            member d.Values = 
                let values = ResizeArray<string[]>()
                for i in 0..x.Count-1 do
                    values.Add(x.GetValues i)
                upcast values
            member d.Add v = d.Add(v.Key, v.Value)
            member d.Add(key,value) = 
                if key = null
                    then raise <| ArgumentNullException("key")
                if d.ContainsKey key
                    then raise <| ArgumentException(sprintf "Duplicate key '%s'" key, "key")
                d.[key] <- value
            member d.Clear() = x.Clear()
            member d.Contains item = x.GetValues(item.Key) = item.Value
            member d.ContainsKey key = x.[key] <> null
            member d.CopyTo(array,arrayIndex) = notimpl()
            member d.GetEnumerator() = getEnumerator()
            member d.GetEnumerator() = getEnumerator() :> IEnumerator
            member d.Remove (item: KeyValuePair<string,string[]>) = 
                if d.Contains item then
                    x.Remove item.Key
                    true
                else
                    false
            member d.Remove (key: string) = 
                let exists = d.ContainsKey key
                x.Remove key
                exists
            member d.TryGetValue(key: string, value: byref<string[]>) = 
                if d.ContainsKey key then
                    value <- d.[key]
                    true
                else false
            }

    [<Extension>]
    [<CompiledName("AsReadonlyDictionary")>]
    let asReadonlyDictionary x =
        let a = asDictionary x
        let notSupported() = raise <| NotSupportedException("Readonly dictionary")
        { new IDictionary<string,string[]> with
            member d.Count = a.Count
            member d.IsReadOnly = true
            member d.Item 
                with get k = a.[k]
                and set k v = notSupported()
            member d.Keys = a.Keys
            member d.Values = a.Values
            member d.Add v = notSupported()
            member d.Add(key,value) = notSupported()
            member d.Clear() = notSupported()
            member d.Contains item = a.Contains item
            member d.ContainsKey key = a.ContainsKey key
            member d.CopyTo(array,arrayIndex) = a.CopyTo(array,arrayIndex)
            member d.GetEnumerator() = a.GetEnumerator()
            member d.GetEnumerator() = a.GetEnumerator() :> IEnumerator
            member d.Remove (item: KeyValuePair<string,string[]>) = notSupported(); false
            member d.Remove (key: string) = notSupported(); false
            member d.TryGetValue(key: string, value: byref<string[]>) = a.TryGetValue(key, ref value)
        }                

    [<Extension>]
    [<CompiledName("AsLookup")>]
    let asLookup (this: NameValueCollection) =
        let getEnumerator() = 
            let e = this.GetEnumerator()
            let wrapElem (o: obj) = 
                let key = o :?> string
                let values = this.GetValues key :> seq<string>
                { new IGrouping<string,string> with
                    member x.Key = key
                    member x.GetEnumerator() = values.GetEnumerator()
                    member x.GetEnumerator() = values.GetEnumerator() :> IEnumerator }
  
            { new IEnumerator<IGrouping<string,string>> with
                member x.Current = wrapElem e.Current
                member x.MoveNext() = e.MoveNext()
                member x.Reset() = e.Reset()
                member x.Dispose() = ()
                member x.Current = box (wrapElem e.Current) }
                      
        { new ILookup<string,string> with
            member x.Count = this.Count
            member x.Item 
                with get key = 
                    match this.GetValues key with
                    | null -> Seq.empty
                    | a -> upcast a
            member x.Contains key = this.Get key <> null
            member x.GetEnumerator() = getEnumerator()
            member x.GetEnumerator() = getEnumerator() :> IEnumerator }
