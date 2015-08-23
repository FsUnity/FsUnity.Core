﻿(*

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
open LanguagePrimitives
open OptimizedClosures
open System
open System.Collections.Generic
open FsUnity


/// Additional functional operators on arrays.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array =




    #if PROTO_COMPILER
    // TODO : Re-implement this using inline IL; it should simply provide a way
    // to emit an 'ldlen' instruction given an array.
    /// <summary>Returns the length of the array as an unsigned, native-length integer.</summary>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("RawLength")>]
    let inline rawLength (array : 'T[]) : unativeint =
        unativeint array.Length
    #endif

    /// <summary>Given an element, creates an array containing just that element.</summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Singleton")>]
    let inline singleton (value : 'T) =
        [| value |]

    /// <summary>Returns the only element of the array.</summary>
    /// <param name="array">The input array.</param>
    /// <returns>The only element of the array.</returns>
    /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
    /// <exception cref="System.ArgumentException">Thrown when the input array does not have precisely one element.</exception>
    [<CompiledName("ExactlyOne")>]
    let exactlyOne (array : 'T[]) : 'T =
        // Preconditions
        checkNonNull "array" array

        match array with
        | [| |] ->
            invalidArg "array" "The array is empty."
        | [| x |] -> x
        | _ ->
            invalidArg "array" "The array contains more than one (1) element."
    
    /// <summary>Builds an array that contains the elements of the set in order.</summary>
    /// <param name="set"></param>
    /// <returns></returns>
    [<CompiledName("OfSet")>]
    let inline ofSet (set : Set<'T>) : 'T[] =
        Set.toArray set

    /// <summary>Builds a set that contains the same elements as the given array.</summary>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("ToSet")>]
    let inline toSet (array : 'T[]) : Set<'T> =
        Set.ofArray array

    /// <summary>Builds a vector from the given array.</summary>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("ToVector")>]
    let inline toVector (array : 'T[]) : vector<'T> =
        FsUnity.vector.Create array

    /// <summary>
    /// Applies a function to each element of the array, returning a new array whose elements are
    /// tuples of the original element and the function result for that element.
    /// </summary>
    /// <param name="projection"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("ProjectValues")>]
    let projectValues (projection : 'Key -> 'T) (array : 'Key[]) =
        // Preconditions
        checkNonNull "array" array

        array |> Array.map (fun x -> x, projection x)

    /// <summary>
    /// Applies a function to each element of the array, returning a new array whose elements are
    /// tuples of the original element and the function result for that element.
    /// </summary>
    /// <param name="projection"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("ProjectKeys")>]
    let projectKeys (projection : 'T -> 'Key) (array : 'T[]) =
        // Preconditions
        checkNonNull "array" array

        array |> Array.map (fun x -> projection x, x)

    /// <summary>Returns the first element in the array.</summary>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("First")>]
    let inline first (array : 'T[]) =
        if Array.isEmpty array then
            invalidOp "Cannot retrieve the first element of an empty array."
        else array.[0]

    /// <summary>Returns the index of the last element in the array.</summary>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("LastIndex")>]
    let inline lastIndex (array : 'T[]) =
        if Array.isEmpty array then
            invalidOp "The array is empty."
        else array.Length - 1

    /// <summary>Returns the last element in the array.</summary>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("Last")>]
    let inline last (array : 'T[]) =
        if Array.isEmpty array then
            invalidOp "Cannot retrieve the last element of an empty array."
        else array.[array.Length - 1]

    /// <summary>Sets all elements in the array to Unchecked.defaultof.</summary>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("Clear")>]
    let inline clear (array : 'T[]) : unit =
        System.Array.Clear (array, 0, array.Length)

    /// <summary>Determines if an array contains a specified value.</summary>
    /// <param name="value"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("Contains")>]
    let contains value (array : 'T[]) : bool =
        // Preconditions
        checkNonNull "array" array

        System.Array.IndexOf (array, value) <> -1

    /// <summary>Expands an array by creating a copy of it which has the specified number of empty elements appended to it.</summary>
    /// <param name="count"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("ExpandRight")>]
    let expandRight count (array : 'T[]) : 'T[] =
        // Preconditions
        checkNonNull "array" array
        if count < 0 then
            invalidArg "count" "The number of elements to expand the array by is negative."

        // OPTIMIZATION : If the count is zero (0), return the original array.
        if count = 0 then array
        else
            // Create the new "expanded" array. Copy the elements from the original array
            // into the "left" side of the new array, then return the expanded array.
            let expandedArr = Array.zeroCreate (array.Length + count)
            Array.blit array 0 expandedArr 0 array.Length
            expandedArr

    /// <summary>Expands an array by creating a copy of it which has the specified number of empty elements prepended to it.</summary>
    /// <param name="count"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("ExpandLeft")>]
    let expandLeft count (array : 'T[]) : 'T[] =
        // Preconditions
        checkNonNull "array" array
        if count < 0 then
            invalidArg "count" "The number of elements to expand the array by is negative."

        // OPTIMIZATION : If the count is zero (0), return the original array.
        if count = 0 then array
        else
            // Create the new "expanded" array. Copy the elements from the original array
            // into the "right" side of the new array, then return the expanded array.
            let expandedArr = Array.zeroCreate (array.Length + count)
            Array.blit array 0 expandedArr count array.Length
            expandedArr

    /// <summary></summary>
    /// <param name="predicate"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("TryFindBack")>]
    let tryFindBack (predicate : 'T -> bool) (array : 'T[]) : 'T option =
        // Preconditions
        checkNonNull "array" array

        let len = Array.length array
        if len = 0 then None
        else
            let mutable index = len - 1
            let mutable result = None

            while index >= 0 && Option.isNone result do
                let el = array.[index]
                if predicate el then
                    result <- Some el
                index <- index - 1

            result

    /// <summary></summary>
    /// <param name="predicate"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("FindBack")>]
    let findBack (predicate : 'T -> bool) (array : 'T[]) : 'T =
        match tryFindBack predicate array with
        | Some x -> x
        | None ->
            // TODO : Provide a better exception message.
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    /// <summary></summary>
    /// <param name="predicate"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("TryFindIndexBack")>]
    let tryFindIndexBack (predicate : 'T -> bool) (array : 'T[]) : int option =
        // Preconditions
        checkNonNull "array" array

        let len = Array.length array
        if len = 0 then None
        else
            let mutable index = len - 1
            let mutable result = None

            while index >= 0 && Option.isNone result do
                let el = array.[index]
                if predicate el then
                    result <- Some index
                index <- index - 1

            result

    /// <summary></summary>
    /// <param name="predicate"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("FindIndexBack")>]
    let findIndexBack (predicate : 'T -> bool) (array : 'T[]) : int =
        match tryFindIndexBack predicate array with
        | Some x -> x
        | None ->
            // TODO : Provide a better exception message.
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    /// <summary></summary>
    /// <param name="picker"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("TryPickBack")>]
    let tryPickBack (picker : 'T -> 'U option) (array : 'T[]) : 'U option =
        // Preconditions
        checkNonNull "array" array
    
        let len = Array.length array
        if len = 0 then None
        else
            let mutable index = len - 1
            let mutable result = None

            while index >= 0 && Option.isNone result do
                let el = array.[index]
                match picker el with
                | None -> ()
                | res ->
                    result <- res
                index <- index - 1

            result

    /// <summary></summary>
    /// <param name="picker"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("PickBack")>]
    let pickBack (picker : 'T -> 'U option) (array : 'T[]) : 'U =
        match tryPickBack picker array with
        | Some x -> x
        | None ->
            // TODO : Provide a better exception message.
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    /// <summary>
    /// Returns a new collection containing the indices of the elements for which the given predicate returns <c>true</c>.
    /// </summary>
    /// <param name="predicate"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("FindIndices")>]
    let findIndices (predicate : 'T -> bool) array : int[] =
        // Preconditions
        checkNonNull "array" array

        let indices = ResizeArray ()
        array |> Array.iteri (fun idx el ->
            if predicate el then indices.Add idx)
        indices.ToArray ()

    /// <summary>
    /// Applies the given function to each element of the array.
    /// Returns the array comprised of the results <c>x</c> for each element where the function returns <c>Some(x)</c>.
    /// The integer index passed to the function indicates the index of the element being transformed.
    /// </summary>
    /// <param name="chooser"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("ChooseIndexed")>]
    let choosei (chooser : int -> 'T -> 'U option) array : 'U[] =
        // Preconditions
        checkNonNull "array" array

        let chooser = FSharpFunc<_,_,_>.Adapt chooser

        let chosen = ResizeArray ()
        let len = Array.length array

        for i = 0 to len - 1 do
            match chooser.Invoke (i, array.[i]) with
            | None -> ()
            | Some value ->
                chosen.Add value

        chosen.ToArray ()

    /// <summary>
    /// Applies the given function pairwise to the two arrays.
    /// Returns the array comprised of the results <c>x</c> for each element where the function returns <c>Some(x)</c>.
    /// </summary>
    /// <param name="chooser"></param>
    /// <param name="array1"></param>
    /// <param name="array2"></param>
    /// <returns></returns>
    [<CompiledName("Choose2")>]
    let choose2 (chooser : 'T1 -> 'T2 -> 'U option) array1 array2 : 'U[] =
        // Preconditions
        checkNonNull "array1" array1
        checkNonNull "array2" array2
        if Array.length array1 <> Array.length array2 then
            invalidArg "array2" "The arrays have different lengths."

        let chooser = FSharpFunc<_,_,_>.Adapt chooser

        let chosen = ResizeArray ()
        let len = Array.length array1

        for i = 0 to len - 1 do
            match chooser.Invoke (array1.[i], array2.[i]) with
            | None -> ()
            | Some value ->
                chosen.Add value

        chosen.ToArray ()

    /// <summary>
    /// Applies a function to each element of the collection, threading an accumulator argument through the computation.
    /// The integer index passed to the function indicates the array index of the element being transformed.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : 'State -> int -> 'T -> 'State) (state : 'State) (array : 'T[]) =
        // Preconditions
        checkNonNull "array" array

        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        let mutable state = state
        let len = array.Length
        for i = 0 to len - 1 do
            state <- folder.Invoke (state, i, array.[i])
        state

    /// <summary>
    /// Applies a function to each element of the collection, threading an accumulator argument through the computation.
    /// The integer index passed to the function indicates the array index of the element being transformed.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="array"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    [<CompiledName("FoldBackIndexed")>]
    let foldiBack (folder : int -> 'T -> 'State -> 'State) (array : 'T[]) (state : 'State) : 'State =
        // Preconditions
        checkNonNull "array" array

        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        
        let mutable state = state
        for i = Array.length array - 1 downto 0 do
            state <- folder.Invoke (i, array.[i], state)
        state

    /// <summary>
    /// Splits an array into one or more arrays; the specified predicate is applied to each element in the array, and whenever it
    /// returns <c>true</c>, that element will be the first element in one of the "subarrays".
    /// </summary>
    /// <param name="predicate"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("Split")>]
    let split (predicate : 'T -> bool) (array : 'T[]) =
        // Preconditions
        checkNonNull "array" array

        let segments = ResizeArray<_> ()
        let mutable currentSegment = ResizeArray<_> ()

        let len = array.Length
        for i = 0 to len - 1 do
            let el = array.[i]
            if currentSegment.Count > 0 && predicate el then
                segments.Add <| currentSegment.ToArray ()
                currentSegment <- ResizeArray<_> ()

            currentSegment.Add el

        // Append the last segment to the segment list,
        // then return the segment list as an array.
        segments.Add <| currentSegment.ToArray ()
        segments.ToArray ()

    /// <summary>
    /// Splits an array into one or more segments by applying the specified predicate to each element of the array and starting
    /// a new view at each element where the predicate returns <c>true</c>.
    /// </summary>
    /// <param name="predicate"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("Segment")>]
    let segment (predicate : 'T -> bool) (array : 'T[]) : ArrayView<'T>[] =
        // Preconditions
        checkNonNull "array" array
        
        let segments = ResizeArray<_> ()

        let len = Array.length array
        let mutable segmentLength = 0
        for i = 0 to len - 1 do
            //
            if segmentLength > 0 && predicate array.[i] then
                // NOTE : The current element is the first element in the *new* segment!
                let offset = i - segmentLength

                ArrayView<_> (array, offset, segmentLength)
                |> segments.Add

                segmentLength <- 1
            else
                // The existing segment is empty, or the predicate returned false --
                // so "append" the element to the existing array segment.
                segmentLength <- segmentLength + 1

        // Finish the last/current segment, then return the list of segments as an array.
        let offset = len - segmentLength

        ArrayView<_> (array, offset, segmentLength)
        |> segments.Add

        segments.ToArray()

    /// <summary>
    /// Splits two arrays into one or more segments by applying the specified predicate to the each pair of array elements and
    /// starting a new view whenever the predicate returns <c>true</c>.
    /// </summary>
    /// <param name="predicate"></param>
    /// <param name="array1"></param>
    /// <param name="array2"></param>
    /// <returns></returns>
    [<CompiledName("Segment2")>]
    let segment2 (predicate : 'T -> 'U -> bool) (array1 : 'T[]) (array2 : 'U[])
        : ArrayView<'T>[] * ArrayView<'U>[] =
        // Preconditions
        checkNonNull "array1" array1
        checkNonNull "array2" array2

        let predicate = FSharpFunc<_,_,_>.Adapt predicate
        let len1 = array1.Length 
        if len1 <> array2.Length then
            invalidArg "array2" "The arrays have differing lengths."

        let segments1 = ResizeArray<_> ()
        let segments2 = ResizeArray<_> ()

        let mutable segmentLength = 0
        for i = 0 to len1 - 1 do
            //
            if segmentLength > 0 && predicate.Invoke (array1.[i], array2.[i]) then
                // NOTE : The current element is the first element in the *new* segment!
                let offset = i - segmentLength

                ArrayView<_> (array1, offset, segmentLength)
                |> segments1.Add
                ArrayView<_> (array2, offset, segmentLength)
                |> segments2.Add

                segmentLength <- 1
            else
                // The existing segment is empty, or the predicate returned false --
                // so "append" the element to the existing array segment.
                segmentLength <- segmentLength + 1

        // Finish the last/current segment, then return the list of segments as an array.
        let offset = len1 - segmentLength

        ArrayView<_> (array1, offset, segmentLength)
        |> segments1.Add
        ArrayView<_> (array2, offset, segmentLength)
        |> segments2.Add

        segments1.ToArray(), segments2.ToArray()

    /// <summary>
    /// Splits the collection into two (2) collections, containing the elements for which the given function returns
    /// <c>Choice1Of2</c> or <c>Choice2Of2</c>, respectively.
    /// </summary>
    /// <param name="partitioner"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    /// <remarks>
    /// This function is similar to Array.partition, but it allows the returned collections to have different types.
    /// </remarks>
    [<CompiledName("MapPartition")>]
    let mapPartition (partitioner : 'T -> Choice<'U1, 'U2>) array : 'U1[] * 'U2[] =
        // Preconditions
        checkNonNull "array" array
    
        // OPTIMIZATION : If the input array is empty, immediately return empty results.
        if Array.isEmpty array then
            Array.empty, Array.empty
        else
            // Use ResizeArrays to hold the mapped values.
            let resultList1 = ResizeArray ()
            let resultList2 = ResizeArray ()

            // Partition the array, adding each element to the ResizeArray
            // specific by the partition function.
            array
            |> Array.iter (fun el ->
                match partitioner el with
                | Choice1Of2 value ->
                    resultList1.Add value
                | Choice2Of2 value ->
                    resultList2.Add value)

            // Convert the ResizeArrays to arrays and return them.
            resultList1.ToArray (),
            resultList2.ToArray ()

    /// <summary>
    /// Splits the collection into two (2) collections, containing the elements for which the given function returns
    /// <c>Choice1Of2</c> or <c>Choice2Of2</c>, respectively.
    /// The index passed to the function indicates the index of the element.
    /// </summary>
    /// <param name="partitioner"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    /// <remarks>
    /// This function is similar to Array.partition, but it allows the returned collections to have different types.
    /// </remarks>
    [<CompiledName("MapPartitionIndexed")>]
    let mapiPartition (partitioner : int -> 'T -> Choice<'U1, 'U2>) array : 'U1[] * 'U2[] =
        // Preconditions
        checkNonNull "array" array
    
        // OPTIMIZATION : If the input array is empty, immediately return empty results.
        if Array.isEmpty array then
            Array.empty, Array.empty
        else
            // Use ResizeArrays to hold the mapped values.
            let resultList1 = ResizeArray ()
            let resultList2 = ResizeArray ()

            let partitioner = FSharpFunc<_,_,_>.Adapt partitioner

            // Partition the array, adding each element to the ResizeArray
            // specific by the partition function.
            let len = Array.length array
            for i = 0 to len - 1 do
                match partitioner.Invoke (i, array.[i]) with
                | Choice1Of2 value ->
                    resultList1.Add value
                | Choice2Of2 value ->
                    resultList2.Add value
                
            // Convert the ResizeArrays to arrays and return them.
            resultList1.ToArray (),
            resultList2.ToArray ()

    /// <summary>
    /// Splits the collection into three (3) collections, containing the elements for which the given
    /// function returns <c>Choice1Of3</c>, <c>Choice2Of3</c>, or <c>Choice3Of3</c>, respectively.
    /// </summary>
    /// <param name="partitioner"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    /// <remarks>
    /// This function is similar to Array.partition, but it allows the returned collections to have different types.
    /// </remarks>
    [<CompiledName("MapPartition3")>]
    let mapPartition3 (partitioner : 'T -> Choice<'U1, 'U2, 'U3>) array : 'U1[] * 'U2[] * 'U3[] =
        // Preconditions
        checkNonNull "array" array

        // OPTIMIZATION : If the input array is empty, immediately return empty results.
        if Array.isEmpty array then
            Array.empty, Array.empty, Array.empty
        else
            // Use ResizeArrays to hold the mapped values.
            let resultList1 = ResizeArray ()
            let resultList2 = ResizeArray ()
            let resultList3 = ResizeArray ()

            // Partition the array, adding each element to the ResizeArray
            // specific by the partition function.
            array
            |> Array.iter (fun el ->
                match partitioner el with
                | Choice1Of3 value ->
                    resultList1.Add value
                | Choice2Of3 value ->
                    resultList2.Add value
                | Choice3Of3 value ->
                    resultList3.Add value)

            // Convert the ResizeArrays to arrays and return them.
            resultList1.ToArray (),
            resultList2.ToArray (),
            resultList3.ToArray ()

    /// <summary>
    /// Applies a mapping function to each element of the array, then repeatedly applies
    /// a reduction function to each pair of results until one (1) result value remains.
    /// </summary>
    /// <param name="mapReduction"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("MapReduce")>]
    let mapReduce (mapReduction : IMapReduction<'Key, 'T>) (array : 'Key[]) : 'T =
        // Preconditions
        checkNonNull "mapReduction" mapReduction
        checkNonNull "array" array
        if Array.isEmpty array then
            invalidArg "array" "The array is empty."

        // Map the first element of the array so it can be
        // used as the seed for the fold.
        let mutable state = mapReduction.Map array.[0]

        // Implement an imperative-style fold, mapping each element
        // then reducing it with the current state to get the new state.
        let len = Array.length array
        for i = 1 to len - 1 do
            state <- mapReduction.Reduce state (mapReduction.Map array.[i])

        // Return the final state.
        state

    /// <summary></summary>
    /// <param name="mapping"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("MapInPlace")>]
    let mapInPlace (mapping : 'T -> 'T) (array : 'T[]) : unit =
        // Preconditions
        checkNonNull "array" array

        // Iterate over the array, mapping the elements and storing the results in-place.
        let len = Array.length array
        for i = 0 to len - 1 do
            array.[i] <- mapping array.[i]

    /// <summary></summary>
    /// <param name="mapping"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("MapIndexedInPlace")>]
    let mapiInPlace (mapping : int -> 'T -> 'T) (array : 'T[]) : unit =
        // Preconditions
        checkNonNull "array" array

        // If the array is empty, return immediately.
        if not <| Array.isEmpty array then
            let mapping = FSharpFunc<_,_,_>.Adapt mapping

            // Iterate over the array, mapping the elements and storing the results in-place.
            let len = Array.length array
            for i = 0 to len - 1 do
                array.[i] <- mapping.Invoke (i, array.[i])

    /// <summary></summary>
    /// <param name="chooser"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("ChooseInPlace")>]
    let chooseInPlace (chooser : 'T -> 'T option) (array : 'T[]) : unit =
        // Preconditions
        checkNonNull "array" array

        // Iterate over the array, mapping the elements and storing the results in-place.
        let len = Array.length array
        for i = 0 to len - 1 do
            match chooser array.[i] with
            | None -> ()
            | Some result ->
                array.[i] <- result

    /// <summary></summary>
    /// <param name="chooser"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("ChooseIndexedInPlace")>]
    let chooseiInPlace (chooser : int -> 'T -> 'T option) (array : 'T[]) : unit =
        // Preconditions
        checkNonNull "array" array

        // If the array is empty, return immediately.
        if not <| Array.isEmpty array then
            let chooser = FSharpFunc<_,_,_>.Adapt chooser

            // Iterate over the array, mapping the elements and storing the results in-place.
            let len = Array.length array
            for i = 0 to len - 1 do
                match chooser.Invoke (i, array.[i]) with
                | None -> ()
                | Some result ->
                    array.[i] <- result

    /// <summary>Returns the number of array elements matching a given predicate.</summary>
    /// <param name="predicate"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    /// <remarks><c>Array.countWith predicate array = (Array.filter predicate array |> Array.length)</c></remarks>
    [<CompiledName("CountWith")>]
    let countWith (predicate : 'T -> bool) (array : 'T[]) : int =
        // Preconditions
        checkNonNull "array" array

        let mutable matches = 0
        let len = Array.length array
        for i = 0 to len - 1 do
            if predicate array.[i] then
                // TODO : Should we use checked addition here? .NET currently limits array
                // sizes to less than Int32.MaxValue elements, but it is possible to create
                // larger arrays on some versions of Mono.
                matches <- matches + 1
    
        // Return the number of matching array elements.
        matches

    /// <summary>
    /// Applies a function to each character in the string and the character which
    /// follows it, threading an accumulator argument through the computation.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("FoldPairwise")>]
    let foldPairwise (folder : 'State -> 'T -> 'T -> 'State) (state : 'State) (array : 'T[]) : 'State =
        // Preconditions
        checkNonNull "array" array

        // OPTIMIZATION : If the array is empty or contains only one element, return immediately.
        let len = Array.length array
        if len < 2 then state
        else
            let folder = FSharpFunc<_,_,_,_>.Adapt folder
            let mutable state = state
    
            // Fold backwards over each pair of elements in the array.
            for i = 0 to len - 2 do
                state <- folder.Invoke (state, array.[i], array.[i + 1])

            // Return the final state value.
            state

    /// <summary>
    /// Applies a function to each character in the string and the character which
    /// proceeds it, threading an accumulator argument through the computation.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="array"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    [<CompiledName("FoldBackPairwise")>]
    let foldBackPairwise (folder : 'T -> 'T -> 'State -> 'State) (array : 'T[]) (state : 'State) : 'State =
        // Preconditions
        checkNonNull "array" array

        // OPTIMIZATION : If the array is empty or contains only one element, return immediately.
        let len = Array.length array
        if len < 2 then state
        else
            let folder = FSharpFunc<_,_,_,_>.Adapt folder
            let mutable state = state

            // Fold over each pair of elements in the array.
            for i = len - 1 downto 1 do
                state <- folder.Invoke (array.[i], array.[i - 1], state)

            // Return the final state value.
            state

    /// <summary>
    /// Returns a list containing the elements generated by the given computation.
    /// The given initial state argument is passed to the element generator, which is applied repeatedly until a <c>None</c> value
    /// is returned. Each call to the element generator returns a new residual state.
    /// </summary>
    /// <param name="generator"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    [<CompiledName("Unfold")>]
    let unfold (generator : 'State -> ('T * 'State) option) (state : 'State) : 'T[] =
        // Preconditions
        // (None)

        let results = ResizeArray ()
        let mutable state = state
        let mutable finished = false

        // Generate elements and append them to the result list.
        while not finished do
            match generator state with
            | Some (result, state') ->
                results.Add result
                state <- state'
            | None ->
                finished <- true

        // Return the result array.
        results.ToArray ()

    /// <summary>
    /// Returns a list containing the elements generated by the given computation.
    /// The given initial state argument is passed to the element generator, which is applied
    /// repeatedly until a <c>None</c> value is returned. Each call to the element generator returns
    /// a new residual state.
    /// </summary>
    /// <param name="generator"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    [<CompiledName("UnfoldBack")>]
    let unfoldBack (generator : 'State -> ('T * 'State) option) (state : 'State) : 'T[] =
        // Preconditions
        // (None)

        let results = ResizeArray ()
        let mutable state = state
        let mutable finished = false

        // Generate elements and append them to the result list.
        while not finished do
            match generator state with
            | Some (result, state') ->
                results.Add result
                state <- state'
            | None ->
                finished <- true

        // Reverse the result array before returning.
        let resultArray = results.ToArray ()
        System.Array.Reverse resultArray
        resultArray

    /// <summary>
    /// 
    /// </summary>
    /// <param name="length"></param>
    /// <param name="generator"></param>
    /// <returns></returns>
    [<CompiledName("Initialize2")>]
    let init2 length (generator : int -> 'T1 * 'T2) : 'T1[] * 'T2[] =
        // Preconditions
        if length < 0 then
            invalidArg "length" "The length of the array to initialize cannot be negative."

        let array1 = Array.zeroCreate length
        let array2 = Array.zeroCreate length

        // Create each of the array elements using the generator function.
        for i = 0 to length - 1 do
            let x, y = generator i
            array1.[i] <- x
            array2.[i] <- y

        // Return the initialized arrays.
        array1, array2
    

    #if FX_NO_TPL_PARALLEL
    #else


/// Provides additional parallel operations on arrays.
    module Parallel =
        open System.Threading
        open System.Threading.Tasks

        /// Sorts key-value pairs with integer keys according to the value of the key.
        [<Sealed>]
        type private ElementIndexComparer<'T> () =
            /// The single instance of this type.
            static let instance = ElementIndexComparer<'T> ()
            /// The single instance of the ElementIndexComparer`1 type.
            static member Instance : IComparer<KeyValuePair<int, 'T>> =
                upcast instance

            interface IComparer<KeyValuePair<int, 'T>> with
                member __.Compare (x, y) =
                    // Compare key values (element indices).
                    x.Key.CompareTo y.Key

        //
        [<Sealed>]
        type private LastElementComparer<'T> () =
            /// The single instance of this type.
            static let instance = LastElementComparer<'T> ()
            /// The single instance of the LastElementComparer`1 type.
            static member Instance : IComparer<ResizeArray<KeyValuePair<int, 'T>>> =
                upcast instance

            interface IComparer<ResizeArray<KeyValuePair<int, 'T>>> with
                member __.Compare (x, y) =
                    // Assumes the lists are non-null.

                    let count_x = x.Count
                    let count_y = y.Count

                    // Is 'x' empty?
                    if count_x = 0 then
                        // If 'y' is also empty, then the lists are considered equal.
                        if count_y = 0 then 0
                        else
                            // x < y
                            -1
                    // Is 'y' empty?
                    elif count_y = 0 then
                        // y < x
                        1
                    else
                        // Compare the key values of the last elements of the two lists to
                        // determine how the lists compare to each other.
                        ElementIndexComparer.Instance.Compare (x.[count_x - 1], y.[count_y - 1])

        /// <summary>
        /// Combines the results of individual workers (e.g., threads, tasks), according to the ordering
        /// of elements in the original input array.
        /// </summary>
        /// <param name="workerResults"></param>
        /// <returns></returns>
        let internal combineWorkerResults (workerResults : ResizeArray<ResizeArray<KeyValuePair<int, 'T>>>) : 'T[] =
            /// The total number of chosen values (i.e., the length of the results array).
            let chosenValueCount =
                workerResults
                |> ResizeArray.sumBy (fun localResults -> localResults.Count)

            /// The array of results (chosen values).
            let results = Array.zeroCreate chosenValueCount

            /// The number of worker result lists.
            let workerResultCount = workerResults.Count

            // If there was only one worker result list (which may be the case if the array was small,
            // or the system only has a single core), we don't need the fancy logic for merging result lists.
            if workerResultCount = 1 then
                let singleWorkerResult = workerResults.[0]
                for i = 0 to results.Length - 1 do
                    results.[i] <- singleWorkerResult.[i].Value
            else
                // Sort the worker results before entering the loop.
                workerResults.Sort LastElementComparer<_>.Instance

                // Copy elements from the individual worker result lists into results.
                // Each local results list has the chosen elements sorted by their original index in ascending order.
                // Removing an element from the end of a list is a fast operation (only decrements an internal counter);
                // we take advantage of that by copying elements into the results list backwards.
                for resultIndex = chosenValueCount - 1 downto 0 do
                    // If the greatest index in the "next" worker result list is greater than the greatest index in
                    // the current best worker result list, we've finished processing a consecutive chunk of elements
                    // from the current best result list. Sort the worker lists according to the last elements of the lists.
                    // This avoids unnecessarily sorting the whole list of worker results on each iteration of the loop.
                    if LastElementComparer<_>.Instance.Compare (workerResults.[workerResultCount - 1], workerResults.[workerResultCount - 2]) < 0 then
                        workerResults.Sort LastElementComparer<_>.Instance

                    /// The current worker result list.
                    let currentResultList = workerResults.[workerResultCount - 1]

                    /// The last index in the current worker result list.
                    let currentResultListLastIndex = currentResultList.Count - 1

                    // Copy the chosen value with the greatest element index into the current index of the results array.
                    results.[resultIndex] <- currentResultList.[currentResultListLastIndex].Value

                    // Remove the copied chosen value from the end of the current worker list.
                    currentResultList.RemoveAt currentResultListLastIndex

            // Return the results array.
            results

        #if DEBUG
        /// Debugging function to check that a parallel loop completed successfully.
        let private checkParallelLoopResult (loopResult : ParallelLoopResult) : unit =
            let msg =
                let msgBase = "The parallel-for loop did not complete successfully."
                let breakIteration = loopResult.LowestBreakIteration
                if breakIteration.HasValue then
                    msgBase + sprintf " The loop was broken at index %i." breakIteration.Value
                else msgBase
            System.Diagnostics.Debug.Assert (loopResult.IsCompleted, msg)
        #endif

        /// <summary>
        /// 
        /// </summary>
        /// <param name="length"></param>
        /// <param name="generator"></param>
        /// <returns></returns>
        [<CompiledName("Initialize2")>]
        let init2 length (generator : int -> 'T1 * 'T2) : 'T1[] * 'T2[] =
            // Preconditions
            if length < 0 then
                invalidArg "length" "The length of the array to initialize cannot be negative."

            let array1 = Array.zeroCreate length
            let array2 = Array.zeroCreate length

            // Create each of the array elements using the generator function.
            // TODO : Instead of ignoring the result, keep it and check the IsCompleted property to
            //        ensure the loop completed as expected; if it didn't, raise an exception instead
            //        of returning a partially-initialized result.
            Parallel.For (0, length, fun i ->
                let x, y = generator i
                array1.[i] <- x
                array2.[i] <- y)
        #if DEBUG
            |> tap checkParallelLoopResult
        #endif
            |> ignore

            // Return the initialized arrays.
            array1, array2

        /// <summary></summary>
        /// <param name="mapping"></param>
        /// <param name="array"></param>
        /// <returns></returns>
        [<CompiledName("MapInPlace")>]
        let mapInPlace (mapping : 'T -> 'T) (array : 'T[]) : unit =
            // Preconditions
            checkNonNull "array" array

            Parallel.For (0, array.Length, fun i ->
                array.[i] <- mapping array.[i])
        #if DEBUG
            |> tap checkParallelLoopResult
        #endif
            |> ignore

        /// <summary></summary>
        /// <param name="mapping"></param>
        /// <param name="array"></param>
        /// <returns></returns>
        [<CompiledName("MapIndexedInPlace")>]
        let mapiInPlace (mapping : int -> 'T -> 'T) (array : 'T[]) : unit =
            // Preconditions
            checkNonNull "array" array

            let mapping = FSharpFunc<_,_,_>.Adapt mapping

            Parallel.For (0, array.Length, fun i ->
                array.[i] <- mapping.Invoke (i, array.[i]))
        #if DEBUG
            |> tap checkParallelLoopResult
        #endif
            |> ignore

        /// <summary></summary>
        /// <param name="predicate"></param>
        /// <param name="array"></param>
        /// <returns></returns>
        [<CompiledName("CountWith")>]
        let countWith (predicate : 'T -> bool) (array : 'T[]) : int =
            // Preconditions
            checkNonNull "array" array

            let matchCount = ref 0

            Parallel.For (0, array.Length,
                System.Func<_> (fun _ -> 0),
                System.Func<_,_,_,_> (fun idx _ localMatchCount ->
                    if predicate array.[idx] then
                        localMatchCount + 1
                    else localMatchCount),
                Action<_> (fun localMatchCount ->
                    Interlocked.Add (matchCount, localMatchCount)
                    |> ignore)
                )
        #if DEBUG
            |> tap checkParallelLoopResult
        #endif
            |> ignore

            // Return the number of matching elements.
            !matchCount

        /// <summary>Applies a function to each element of the array, threading an accumulator argument
        /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
        /// then computes <c>f (... (f i0 i1)...) iN</c>.
        /// Raises ArgumentException if the array has size zero.</summary>
        /// <param name="reduction">The function to reduce a pair of elements to a single element.</param>
        /// <param name="array">The input array.</param>
        /// <exception cref="System.ArgumentNullException">Thrown when <paramref name="array"/> is null.</exception>
        /// <exception cref="System.ArgumentException">Thrown when <paramref name="array"/> is empty.</exception>
        /// <returns>The final result of the reductions.</returns>
        [<CompiledName("Reduce")>]
        let reduce (reduction : 'T -> 'T -> 'T) (array : 'T[]) : 'T =
            // Preconditions
            checkNonNull "array" array
            if Array.isEmpty array then
                invalidArg "array" "The array is empty."

            /// Holds the reduction state, if any.
            /// This is used when parallel workers finish their local results and need to combine
            /// with the other worker results.
            let state = ref None

            /// Synchronization object for the reduction state.
            let stateSyncRoot = obj ()

            let reduction = FSharpFunc<_,_,_>.Adapt reduction

            Parallel.For (0, array.Length,
                System.Func<_> (fun _ -> None),
                System.Func<_,_,_,_> (fun idx _ (localResult : _ option) ->
                    /// The current array element.
                    let el = array.[idx]

                    // If the local result hasn't been initialized yet, the result is just the
                    // current array element.
                    match localResult with
                    | None ->
                        Some el
                    | Some localResult ->
                        // Combine (reduce) the existing local result with the current array element.
                        Some <| reduction.Invoke (localResult, el)),
                Action<_> (fun (localResult : _ option) ->
                    match localResult with
                    | None ->
                        // There is no local result, so there's nothing to be done.
                        ()
                    | Some result ->
                        // We need to modify the global state, so lock for synchronization.
                        lock stateSyncRoot <| fun () ->
                            state :=
                                match !state with
                                | None ->
                                    // The state has not yet been set; set it to this local result.
                                    localResult
                                | Some existingState ->
                                    // There is already an existing state, so we need to combine (reduce) the
                                    // two states to get the new state value.
                                    Some <| reduction.Invoke (existingState, result))
                )
        #if DEBUG
            |> tap checkParallelLoopResult
        #endif
            |> ignore

            // Since we know the input array isn't empty, a state value _must_ have been set.
            Option.get !state

        /// <summary>
        /// Applies a mapping function to each element of the array, then repeatedly applies
        /// a reduction function to each pair of results until one (1) result value remains.
        /// </summary>
        /// <param name="mapReduction"></param>
        /// <param name="array"></param>
        /// <returns></returns>
        [<CompiledName("MapReduce")>]
        let mapReduce (mapReduction : IMapReduction<'Key, 'T>) (array : 'Key[]) : 'T =
            // Preconditions
            checkNonNull "mapReduction" mapReduction
            checkNonNull "array" array
            if Array.isEmpty array then
                invalidArg "array" "The array is empty."

            /// Holds the reduction state, if any.
            /// This is used when parallel workers finish their local results and need to combine
            /// with the other worker results.
            let state = ref None

            /// Synchronization object for the reduction state.
            let stateSyncRoot = obj ()

            Parallel.For (0, array.Length,
                System.Func<_> (fun _ -> None),
                System.Func<_,_,_,_> (fun idx _ (localResult : _ option) ->
                    /// The result of applying the mapping function to the current array element.
                    let mappedElement = mapReduction.Map array.[idx]

                    // If the local result hasn't been initialized yet, the result is just the
                    // current mapped array element.
                    match localResult with
                    | None ->
                        Some mappedElement
                    | Some localResult ->
                        // Combine (reduce) the existing local result with the mapped array element.
                        Some (mapReduction.Reduce localResult mappedElement)),
                Action<_> (fun (localResult : _ option) ->
                    match localResult with
                    | None ->
                        // There is no local result, so there's nothing to be done.
                        ()
                    | Some result ->
                        // We need to modify the global state, so lock for synchronization.
                        lock stateSyncRoot <| fun () ->
                            state :=
                                match !state with
                                | None ->
                                    // The state has not yet been set; set it to this local result.
                                    localResult
                                | Some existingState ->
                                    // There is already an existing state, so we need to combine (reduce) the
                                    // two states to get the new state value.
                                    Some (mapReduction.Reduce existingState result))
                )
        #if DEBUG
            |> tap checkParallelLoopResult
        #endif
            |> ignore

            // Since we know the input array isn't empty, a state value _must_ have been set.
            Option.get !state

        /// <summary>Returns a new collection containing only the elements of the collection
        /// for which the given predicate returns "true".</summary>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="array">The input array.</param>
        /// <returns>An array containing the elements for which the given predicate returns true.</returns>
        // TODO : This could be implemented more efficiently by creating a bitvector where each bit corresponds
        //        to an array element, then having the workers set the bits of the bitvector where the predicate
        //        returns true. Then, we can compute the population count, create the output array, and copy the
        //        filtered elements into it. This would probably only be slightly faster, but would reduce memory
        //        usage of the order of the output size because we wouldn't need to keep an additional copy of each
        //        filtered element in a ResizeArray before copying to the output array.
        [<CompiledName("Filter")>]
        let filter (predicate : 'T -> bool) (array : 'T[]) : 'T[] =
            // Preconditions
            checkNonNull "array" array

            // If the input array is empty, immediately return an empty output array.
            if Array.isEmpty array then Array.empty
            else

            /// Holds ResizeArrays containing the values chosen by each worker task in the loop.
            /// The index of each chosen value is included as a key so that the values can
            /// be re-assembled in the same order as their original source elements.
            let workerResults = ResizeArray<_> (System.Environment.ProcessorCount)

            // Choose values from the array in parallel.
            Parallel.For (0, array.Length,
                System.Func<_> (fun _ -> ResizeArray ()),
                System.Func<_,_,_,_> (fun idx _ (localResults : ResizeArray<_>) ->
                    let el = array.[idx]
                    if predicate el then
                        // Add this element, along with its index, to the list of local results for this worker.
                        localResults.Add (KeyValuePair<_,_> (idx, el))
                    
                    // Return the local results so they're passed to the next loop iteration.
                    localResults),
                    Action<_> (fun (localResults : ResizeArray<_>) ->
                    // Sort the local results from this worker in ascending order of element index.
                    // This is necessary because workers may steal chunks of elements from each other, meaning workers
                    // may process elements out of order.
                    localResults.Sort (ElementIndexComparer<_>.Instance)

                    // The worker results list must be locked when adding the results
                    // from this worker, to avoid concurrency issues.
                    lock workerResults <| fun () ->
                        workerResults.Add localResults)
                )
        #if DEBUG
                |> tap checkParallelLoopResult
        #endif
                |> ignore

            // Combine worker results and return.
            combineWorkerResults workerResults

        /// <summary>
        /// Applies the given function pairwise to the two arrays.
        /// Returns the array comprised of the results <c>x</c> for each element where the function returns <c>Some(x)</c>.
        /// </summary>
        /// <param name="chooser"></param>
        /// <param name="array1"></param>
        /// <param name="array2"></param>
        /// <returns></returns>
        [<CompiledName("Choose2")>]
        let choose2 (chooser : 'T1 -> 'T2 -> 'U option) array1 array2 : 'U[] =
            // Preconditions
            checkNonNull "array1" array1
            checkNonNull "array2" array2
            if Array.length array1 <> Array.length array2 then
                invalidArg "array2" "The arrays have different lengths."

            // If the input arrays are empty, immediately return an empty output array.
            if Array.isEmpty array1 then Array.empty
            else

            let chooser = FSharpFunc<_,_,_>.Adapt chooser

            /// Holds ResizeArrays containing the values chosen by each worker task in the loop.
            /// The index of each chosen value is included as a key so that the values can
            /// be re-assembled in the same order as their original source elements.
            let workerResults = ResizeArray<_> (System.Environment.ProcessorCount)

            // Choose values from the array in parallel.
            Parallel.For (0, array1.Length,
                System.Func<_> (fun _ -> ResizeArray ()),
                System.Func<_,_,_,_> (fun idx _ (localResults : ResizeArray<_>) ->
                    match chooser.Invoke (array1.[idx], array2.[idx]) with
                    | None -> ()
                    | Some result ->
                        // Add this result, along with it's original element index, to the list of local results for this worker.
                        localResults.Add (KeyValuePair<_,_> (idx, result))
                    
                    // Return the local results so they're passed to the next loop iteration.
                    localResults),
                    Action<_> (fun (localResults : ResizeArray<_>) ->
                    // Sort the local results from this worker in ascending order of element index.
                    // This is necessary because workers may steal chunks of elements from each other, meaning workers
                    // may process elements out of order.
                    localResults.Sort (ElementIndexComparer<_>.Instance)

                    // The worker results list must be locked when adding the results
                    // from this worker, to avoid concurrency issues.
                    lock workerResults <| fun () ->
                        workerResults.Add localResults)
                )
        #if DEBUG
                |> tap checkParallelLoopResult
        #endif
                |> ignore

            // Combine worker results and return.
            combineWorkerResults workerResults

        /// <summary>
        /// 
        /// </summary>
        /// <param name="chooser"></param>
        /// <param name="array"></param>
        /// <returns></returns>
        [<CompiledName("ChooseIndexed")>]
        let choosei (chooser : int -> 'T -> 'U option) (array: 'T[]) : 'U[] =
            // Preconditions
            checkNonNull "array" array

            // If the input array is empty, immediately return an empty output array.
            if Array.isEmpty array then Array.empty
            else

            let chooser = FSharpFunc<_,_,_>.Adapt chooser

            /// Holds ResizeArrays containing the values chosen by each worker task in the loop.
            /// The index of each chosen value is included as a key so that the values can
            /// be re-assembled in the same order as their original source elements.
            let workerResults = ResizeArray<_> (System.Environment.ProcessorCount)

            // Choose values from the array in parallel.
            Parallel.For (0, array.Length,
                System.Func<_> (fun _ -> ResizeArray ()),
                System.Func<_,_,_,_> (fun idx _ (localResults : ResizeArray<_>) ->
                    match chooser.Invoke (idx, array.[idx]) with
                    | None -> ()
                    | Some result ->
                        // Add this result, along with it's original element index, to the list of local results for this worker.
                        localResults.Add (KeyValuePair<_,_> (idx, result))
                    
                    // Return the local results so they're passed to the next loop iteration.
                    localResults),
                    Action<_> (fun (localResults : ResizeArray<_>) ->
                    // Sort the local results from this worker in ascending order of element index.
                    // This is necessary because workers may steal chunks of elements from each other, meaning workers
                    // may process elements out of order.
                    localResults.Sort (ElementIndexComparer<_>.Instance)

                    // The worker results list must be locked when adding the results
                    // from this worker, to avoid concurrency issues.
                    lock workerResults <| fun () ->
                        workerResults.Add localResults)
                )
        #if DEBUG
                |> tap checkParallelLoopResult
        #endif
                |> ignore

            // Combine worker results and return.
            combineWorkerResults workerResults

        /// <summary>
        /// Splits the collection into two (2) collections, containing the elements for which the given function returns
        /// <c>Choice1Of2</c> or <c>Choice2Of2</c>, respectively.
        /// </summary>
        /// <param name="partitioner"></param>
        /// <param name="array"></param>
        /// <returns></returns>
        /// <remarks>
        /// This function is similar to Array.partition, but it allows the returned collections to have different types.
        /// </remarks>
        [<CompiledName("MapPartition")>]
        let mapPartition (partitioner : 'T -> Choice<'U1, 'U2>) (array : 'T[]) : 'U1[] * 'U2[] =
            // Preconditions
            checkNonNull "array" array

            // If the input array is empty, immediately return empty output arrays.
            if Array.isEmpty array then Array.empty, Array.empty
            else

            /// Holds ResizeArrays containing the Choice1Of2 values from each worker task in the loop.
            /// The index of each chosen value is included as a key so that the values can
            /// be re-assembled in the same order as their original source elements.
            let workerResults1 = ResizeArray<_> (System.Environment.ProcessorCount)
            /// Holds ResizeArrays containing the Choice1Of2 values from each worker task in the loop.
            let workerResults2 = ResizeArray<_> (System.Environment.ProcessorCount)

            // Partition values from the array in parallel.
            // OPTIMIZATION : The thread-local results are passed around as a tuple of ResizeArrays. Since ResizeArrays are mutable
            //                this means we can pass a single instance of the tuple through the loop instead of creating a new tuple
            //                instance on each loop iteration.
            Parallel.For (0, array.Length,
                System.Func<_> (fun _ -> ResizeArray (), ResizeArray ()),
                System.Func<_,_,_,_> (fun idx _ (localResults : ResizeArray<_> * ResizeArray<_>) ->
                    match partitioner array.[idx] with
                    | Choice1Of2 result ->
                        // Add this result, along with it's original element index, to the list of local results for this worker.
                        (fst localResults).Add (KeyValuePair<_,_> (idx, result))
                    | Choice2Of2 result ->
                        // Add this result, along with it's original element index, to the list of local results for this worker.
                        (snd localResults).Add (KeyValuePair<_,_> (idx, result))

                    // Return the local results so they're passed to the next loop iteration.
                    localResults),
                    Action<_> (fun (localResults1 : ResizeArray<_>, localResults2 : ResizeArray<_>) ->
                    // Sort the local results from this worker in ascending order of element index.
                    // This is necessary because workers may steal chunks of elements from each other, meaning workers
                    // may process elements out of order.
                    localResults1.Sort (ElementIndexComparer<_>.Instance)
                    localResults2.Sort (ElementIndexComparer<_>.Instance)

                    // The worker results lists must be locked when adding the results
                    // from this worker, to avoid concurrency issues.
                    lock workerResults1 <| fun () ->
                        workerResults1.Add localResults1
                        workerResults2.Add localResults2)
                )
        #if DEBUG
                |> tap checkParallelLoopResult
        #endif
                |> ignore

            // Combine worker results and return.
            // TODO : Perhaps utilize parallelism here -- instead of combining the worker result lists
            //        serially, they could be combined in parallel to improve performance for large inputs.
            combineWorkerResults workerResults1,
            combineWorkerResults workerResults2

        /// <summary>
        /// Splits the collection into two (2) collections, containing the elements for which the given function returns
        /// <c>Choice1Of2</c> or <c>Choice2Of2</c>, respectively.
        /// The index passed to the function indicates the index of the element.
        /// </summary>
        /// <param name="partitioner"></param>
        /// <param name="array"></param>
        /// <returns></returns>
        /// <remarks>
        /// This function is similar to Array.partition, but it allows the returned collections to have different types.
        /// </remarks>
        [<CompiledName("MapPartitionIndexed")>]
        let mapiPartition (partitioner : int -> 'T -> Choice<'U1, 'U2>) (array : 'T[]) : 'U1[] * 'U2[] =
            // Preconditions
            checkNonNull "array" array

            // If the input array is empty, immediately return empty output arrays.
            if Array.isEmpty array then Array.empty, Array.empty
            else

            let partitioner = FSharpFunc<_,_,_>.Adapt partitioner

            /// Holds ResizeArrays containing the Choice1Of2 values from each worker task in the loop.
            /// The index of each chosen value is included as a key so that the values can
            /// be re-assembled in the same order as their original source elements.
            let workerResults1 = ResizeArray<_> (System.Environment.ProcessorCount)
            /// Holds ResizeArrays containing the Choice1Of2 values from each worker task in the loop.
            let workerResults2 = ResizeArray<_> (System.Environment.ProcessorCount)

            // Partition values from the array in parallel.
            // OPTIMIZATION : The thread-local results are passed around as a tuple of ResizeArrays. Since ResizeArrays are mutable
            //                this means we can pass a single instance of the tuple through the loop instead of creating a new tuple
            //                instance on each loop iteration.
            Parallel.For (0, array.Length,
                System.Func<_> (fun _ -> ResizeArray (), ResizeArray ()),
                System.Func<_,_,_,_> (fun idx _ (localResults : ResizeArray<_> * ResizeArray<_>) ->
                    match partitioner.Invoke (idx, array.[idx]) with
                    | Choice1Of2 result ->
                        // Add this result, along with it's original element index, to the list of local results for this worker.
                        (fst localResults).Add (KeyValuePair<_,_> (idx, result))
                    | Choice2Of2 result ->
                        // Add this result, along with it's original element index, to the list of local results for this worker.
                        (snd localResults).Add (KeyValuePair<_,_> (idx, result))

                    // Return the local results so they're passed to the next loop iteration.
                    localResults),
                    Action<_> (fun (localResults1 : ResizeArray<_>, localResults2 : ResizeArray<_>) ->
                    // Sort the local results from this worker in ascending order of element index.
                    // This is necessary because workers may steal chunks of elements from each other, meaning workers
                    // may process elements out of order.
                    localResults1.Sort (ElementIndexComparer<_>.Instance)
                    localResults2.Sort (ElementIndexComparer<_>.Instance)

                    // The worker results lists must be locked when adding the results
                    // from this worker, to avoid concurrency issues.
                    lock workerResults1 <| fun () ->
                        workerResults1.Add localResults1
                        workerResults2.Add localResults2)
                )
        #if DEBUG
                |> tap checkParallelLoopResult
        #endif
                |> ignore

            // Combine worker results and return.
            // TODO : Perhaps utilize parallelism here -- instead of combining the worker result lists
            //        serially, they could be combined in parallel to improve performance for large inputs.
            combineWorkerResults workerResults1,
            combineWorkerResults workerResults2

        /// <summary>Returns the greatest of all elements of the array, compared via Operators.max on the function result.</summary>
        /// <param name="array">The input array.</param>
        /// <returns>The maximum element.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the input array is empty.</exception>
        [<CompiledName("Max")>]
        let max (array : 'T[]) : 'T =
            // Preconditions
            checkNonNull "array" array
            if Array.isEmpty array then
                invalidArg "array" "The array is empty."

            // Reduce using the 'max' operator.
            reduce max array

        /// <summary>Returns the greatest of all elements of the array, compared via Operators.max on the function result.</summary>
        /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
        /// <param name="array">The input array.</param>
        /// <returns>The maximum element.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the input array is empty.</exception>
        [<CompiledName("MaxBy")>]
        let maxBy (projection : 'T -> 'U) (array : 'T[]) : 'T =
            // Preconditions
            checkNonNull "array" array
            if Array.isEmpty array then
                invalidArg "array" "The array is empty."

            // Reduce using the 'maxBy' operator with the projection function.
            reduce (maxBy projection) array

        /// <summary>Returns the least of all elements of the array, compared via Operators.min on the function result.</summary>
        /// <param name="array">The input array.</param>
        /// <returns>The minimum element.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the input array is empty.</exception>
        [<CompiledName("Min")>]
        let min (array : 'T[]) : 'T =
            // Preconditions
            checkNonNull "array" array
            if Array.isEmpty array then
                invalidArg "array" "The array is empty."

            // Reduce using the 'min' operator.
            reduce min array

        /// <summary>Returns the least of all elements of the array, compared via Operators.min on the function result.</summary>
        /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
        /// <param name="array">The input array.</param>
        /// <returns>The minimum element.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the input array is empty.</exception>
        [<CompiledName("MinBy")>]
        let minBy (projection : 'T -> 'U) (array : 'T[]) : 'T =
            // Preconditions
            checkNonNull "array" array
            if Array.isEmpty array then
                invalidArg "array" "The array is empty."

            // Reduce using the 'minBy' operator with the projection function.
            reduce (minBy projection) array

        /// <summary>Returns the sum of the elements in the array.</summary>
        /// <param name="array">The input array.</param>
        /// <returns>The resulting sum.</returns>
        [<CompiledName("Sum")>]
        let inline sum (array: (^T)[] ) : ^T = 
            // Preconditions
            checkNonNull "array" array

            // If the array is empty, immediately return zero.
            if Array.isEmpty array then
                LanguagePrimitives.GenericZero<_>
            else
                // Reduce the array using checked addition, just like Array.sum.
                reduce (Checked.(+)) array

        /// <summary>Returns the sum of the results generated by applying the function to each element of the array.</summary>
        /// <param name="projection">The function to transform the array elements into the type to be summed.</param>
        /// <param name="array">The input array.</param>
        /// <returns>The resulting sum.</returns>
        [<CompiledName("SumBy")>]
        let inline sumBy (projection : 'T -> ^U) (array: (^T)[] ) : ^U =
            // Preconditions
            checkNonNull "array" array

            // If the array is empty, immediately return zero.
            if Array.isEmpty array then
                LanguagePrimitives.GenericZero<_>
            else
                // Reduce the array using checked addition, just like Array.sumBy.
                array
                |> mapReduce
                    { new IMapReduction<_,_> with
                        member __.Map x =
                            projection x
                        member __.Reduce x y =
                            Checked.(+) x y }

        let rec concatAddLengths (arrs:'T[][]) i acc =
            if i >= arrs.Length then acc 
            else concatAddLengths arrs (i+1) (acc + arrs.[i].Length)

        let rec concatBlit (arrs:'T[][]) i j (tgt:'T[]) =
            if i < arrs.Length then 
                let h = arrs.[i]
                let len = h.Length 
                Array.Copy(h, 0, tgt, j, len)
                concatBlit arrs (i+1) (j+len) tgt



        let concatArrays (arrs : 'T[][]) =
            let res = Array.zeroCreate (concatAddLengths arrs 0 0) 
            concatBlit arrs 0 0 res ;
            res       



        #endif  // #if !FX_NO_TPL_PARALLEL


        [<CompiledName("Choose")>]
        let choose f (array: 'T[]) = 
            checkNonNull "array" array
            let inputLength = array.Length
            let lastInputIndex = inputLength - 1

            let isChosen : bool [] = Array.zeroCreate inputLength
            let results : 'U [] = Array.zeroCreate inputLength
                
            Parallel.For(0, inputLength, (fun i -> 
                match f array.[i] with 
                | None -> () 
                | Some v -> 
                    isChosen.[i] <- true; 
                    results.[i] <- v
            )) |> ignore         
                                                                                      
            let mutable outputLength = 0                
            for i = 0 to lastInputIndex do 
                if isChosen.[i] then 
                    outputLength <- outputLength + 1
                        
            let output = Array.zeroCreate outputLength
            let mutable curr = 0
            for i = 0 to lastInputIndex do 
                if isChosen.[i] then 
                    output.[curr] <- results.[i]
                    curr <- curr + 1
            output
                
        [<CompiledName("Collect")>]
        let collect (f : 'T -> 'U[])  (array : 'T[]) : 'U[]=
            checkNonNull "array" array
            let inputLength = array.Length
            let result = Array.zeroCreate inputLength
            Parallel.For(0, inputLength, 
                (fun i -> result.[i] <- f array.[i])) |> ignore
            concatArrays result
                
        [<CompiledName("Map")>]
        let map (f: 'T -> 'U) (array : 'T[]) : 'U[]=
            checkNonNull "array" array
            let inputLength = array.Length
            let result = Array.zeroCreate inputLength
            Parallel.For(0, inputLength, fun i ->
                result.[i] <- f array.[i]) |> ignore
            result
                
        [<CompiledName("MapIndexed")>]
        let mapi f (array: 'T[]) =
            checkNonNull "array" array
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let inputLength = array.Length
            let result = Array.zeroCreate inputLength 
            Parallel.For(0, inputLength, fun i ->
                result.[i] <- f.Invoke (i, array.[i])) |> ignore
            result
                
        [<CompiledName("Iterate")>]
        let iter f (array : 'T[]) =
            checkNonNull "array" array
            Parallel.For (0, array.Length, fun i -> f array.[i]) |> ignore  
                
        [<CompiledName("IterateIndexed")>]
        let iteri f (array : 'T[]) =
            checkNonNull "array" array
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            Parallel.For (0, array.Length, fun i -> f.Invoke(i, array.[i])) |> ignore        
                
        [<CompiledName("Initialize")>]
        let init count f =
            let result = Array.zeroCreate count
            Parallel.For (0, count, fun i -> result.[i] <- f i) |> ignore
            result
                
        [<CompiledName("Partition")>]
        let partition predicate (array : 'T[]) =
            checkNonNull "array" array
            let inputLength = array.Length
            let lastInputIndex = inputLength - 1

            let isTrue = Array.zeroCreate inputLength
            Parallel.For(0, inputLength, 
                fun i -> isTrue.[i] <- predicate array.[i]
                ) |> ignore
                
            let mutable trueLength = 0
            for i in 0 .. lastInputIndex do
                if isTrue.[i] then trueLength <- trueLength + 1
                
            let trueResult = Array.zeroCreate trueLength
            let falseResult = Array.zeroCreate (inputLength - trueLength)
            let mutable iTrue = 0
            let mutable iFalse = 0
            for i = 0 to lastInputIndex do
                if isTrue.[i] then
                    trueResult.[iTrue] <- array.[i]
                    iTrue <- iTrue + 1
                else
                    falseResult.[iFalse] <- array.[i]
                    iFalse <- iFalse + 1

            (trueResult, falseResult)

