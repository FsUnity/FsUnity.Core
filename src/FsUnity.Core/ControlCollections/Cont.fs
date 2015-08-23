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

//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FsUnity.Control.Collections.Cps.Cont
    
open Microsoft.FSharp.Control
open OptimizedClosures
open FsUnity
open FsUnity.Collections
open FsUnity.Control.Cps


/// The standard F# Array module, adapted for use within 'cont' workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array =
    /// Cont implementation of Array.fold.
    let rec private foldImpl (folder : FSharpFunc<_,_,_>, array : 'T[], state : 'State, currentIndex) : ContFunc<'State, 'K> =
        cont {
        if currentIndex >= array.Length then
            // We've reached the end of the array so return the final state value.
            return state
        else
            // Invoke the folder with the current array element and state value.
            let! state = folder.Invoke (state, array.[currentIndex])

            // Continue folding over the remaining array elements.
            return! foldImpl (folder, array, state, currentIndex + 1)
        }

    /// Cont implementation of Array.fold.
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> ContFunc<'State, 'K>) (state : 'State) (array : 'T[]) : ContFunc<'State, 'K> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_>.Adapt folder
        foldImpl (folder, array, state, 0)


/// The standard F# List module, adapted for use within 'cont' workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module List =
    /// Cont implementation of List.fold.
    let rec private foldImpl (folder : FSharpFunc<_,_,_>, list : 'T list, state : 'State) : ContFunc<'State, 'K> =
        cont {
        match list with
        | [] ->
            // We've reached the end of the list so return the final state value.
            return state
        | hd :: tl ->
            // Invoke the folder with the head element of the list and the current state value.
            let! state = folder.Invoke (state, hd)

            // Continue folding over the rest of the list.
            return! foldImpl (folder, tl, state)
        }

    /// Cont implementation of List.fold.
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> ContFunc<'State, 'K>) (state : 'State) (list : 'T list) : ContFunc<'State, 'K> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_>.Adapt folder
        foldImpl (folder, list, state)

