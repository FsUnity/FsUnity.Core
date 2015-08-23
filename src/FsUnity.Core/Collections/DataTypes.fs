namespace FsUnity

open System.Collections
open System.Collections.Generic
open FsUnity


/// <summary>Immutable array with constant-time access to elements.</summary>
[<Struct; CompiledName("FSharpVector`1")>]
type vector<'T> private (elements : 'T[]) =
    /// The empty vector instance.
    static let empty : vector<'T> = vector (Array.empty)

    /// The value representing the equivalent of 'null' for a vector.
    static member Null
        with get () : vector<'T> =
            Unchecked.defaultof<vector<'T>>

    /// The empty vector.
    static member Empty
        with get () = empty

    /// Gets the array containing the vector's elements.
    member internal __.Elements
        with get () = elements

    /// Is the vector empty?
    member __.IsEmpty
        with get () =
            Array.isEmpty elements

    /// Is the vector 'null' (uninitialized)?
    member __.IsNull
        with get () =
            isNull elements

    /// Gets a 32-bit integer that represents the total number of elements in the Vector.
    member __.Length
        with get () =
            elements.Length

#if FX_ATLEAST_PORTABLE
#else
    /// Gets a 64-bit integer that represents the total number of elements in the Vector.
    member __.LongLength
        with get () =
            elements.LongLength
#endif

    /// Returns the vector element at the specified index.
    member this.Item
        with get index =
            // Preconditions
            if this.IsNull then
                raise <| System.NullReferenceException "Cannot retrieve an item from a null-equivalent vector."

            // None -- The CLR inserts it's own bounds check automatically so adding one here
            // would impact performance without gaining any additional safety.
            elements.[index]

    /// Creates a new Vector from the given array.
    static member Create (source : 'T[]) : vector<'T> =
        // Preconditions
        checkNonNull "source" source

        // Create a shallow copy of the source array, then pass it to
        // the private Vector constructor and return the new Vector value.
        vector (Array.copy source)

    /// Creates a new Vector from the given array.
    /// This method is considered "unsafe" and should be used with caution because
    /// the given array is used directly instead of being copied; if the array is
    /// modified by some other code, the vector will also be modified (which violates
    /// the semantics of the type).
    static member UnsafeCreate (source : 'T[]) : vector<'T> =
        // Preconditions
        checkNonNull "source" source

        // Create a new vector directly from the source array.
        vector (source)

    interface IEnumerable with
        /// <inherit />
        member this.GetEnumerator () : IEnumerator =
            // Preconditions
            if this.IsNull then
                invalidOp "Cannot get an enumerator for a null-equivalent vector."

            elements.GetEnumerator ()

    interface IEnumerable<'T> with
        /// <inherit />
        member this.GetEnumerator () : IEnumerator<'T> =
            // Preconditions
            if this.IsNull then
                invalidOp "Cannot get an enumerator for a null-equivalent vector."

            // Use the built-in generic array enumerator -- the one implicitly provided
            // by the array type is non-generic and can't be cast to a generic form.
            let enum = Array.toSeq elements
            enum.GetEnumerator ()