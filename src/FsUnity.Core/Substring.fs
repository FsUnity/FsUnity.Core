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

namespace FsUnity

open System
//open System.Diagnostics.Contracts
open System.Globalization
open System.Runtime.InteropServices


/// Represents a segment of a string.
[<Struct; CompiledName("Substring")>]
[<CustomEquality; CustomComparison>]
type substring =
    /// The underlying string for this substring.
    val String : string
    /// The position of the first character in the substring, relative to the start of the underlying string.
    val Offset : int
    /// The number of characters spanned by the substring.
    val Length : int

    /// <summary>Create a new substring value spanning the entirety of a specified string.</summary>
    /// <param name="string">The string to use as the substring's underlying string.</param>
    new (string : string) =
        // Preconditions
        checkNonNull "string" string

        { String = string;
          Offset = 0;
          Length = string.Length; }
    
    /// <summary>Create a new substring value from a specified string.</summary>
    /// <param name="string"></param>
    /// <param name="offset"></param>
    /// <param name="length"></param>
    new (string : string, offset : int, length : int) =
        // Preconditions
        checkNonNull "string" string
        if offset < 0 then
            argOutOfRange "offset" "The offset must be greater than or equal to zero."
        
        /// The length of the underlying string.
        let strLen = String.length string

        // More preconditions
        if offset > strLen then
            argOutOfRange "offset" "The offset must be less than the length of the string."
        elif length < 0 then
            argOutOfRange "length" "The substring length must be greater than or equal to zero."
        elif offset + length > strLen then
            argOutOfRange "length" "The specified length is greater than the number of characters \
                                    in the string from the given offset."

        { String = string;
          Offset = offset;
          Length = length; }

    /// Is this an empty substring?
    member this.IsEmpty
        with get () =
            this.Length = 0

    /// Gets the character at the specified index in the substring.
    member this.Item
        with get index =
            // Preconditions
            if index < 0 || index >= this.Length then
                // TODO : Provide a better error message here.
                raise <| System.IndexOutOfRangeException ()
            
            // Return the specified character from the underlying string.
            this.String.[this.Offset + index]

    /// <summary></summary>
    /// <param name="value"></param>
    /// <returns></returns>
    member this.Contains (value : substring) : bool =
        // Preconditions
        // TODO

        // If 'value' is empty, return true because any substring
        // (even an empty one) always contains an empty substring.
        if value.Length = 0 then true
        else
            // Compare the length of 'value' with the length of this substring.
            match compare this.Length value.Length with
            | Less ->
                // Return false, because this substring cannot contain a substring longer than itself.
                false

            | Equal ->
                // If the substrings have equal lengths, compare the contents of the two substrings for structural equality.
                substring.CompareOrdinal (this, value) = 0

            | Greater ->
                // Does the beginning of this substring match 'value'?
                if this.StartsWith value then true
                else
                    // Try to find the next instance of the first character of 'value' within the remainder of this substring.
                    // If found, continue the search for a matching substring instance at that point.
                    match this.String.IndexOf (value.[0], this.Offset + 1) with
                    | -1 ->
                        // No more matches are possible, so this string does not contain 'value'.
                        false
                    | nextIndexAbsolute ->
                        /// A substring of this substring, beginning at the next instance of the first character in 'value'.
                        let next = substring (this.String, nextIndexAbsolute, this.Length - (nextIndexAbsolute - this.Offset))

                        // Continue searching (recursively) for an instance of 'value'.
                        next.Contains value

    /// <summary></summary>
    /// <param name="value"></param>
    /// <returns></returns>
    member this.Contains (value : string) : bool =
        // Preconditions
        checkNonNull "value" value

        // Create a substring from the string and call the substring-based implementation.
        this.Contains (substring (value))

    /// <summary>Determines whether the end of this substring value matches the specified substring.</summary>
    /// <param name="value">The substring to compare.</param>
    /// <returns></returns>
    member this.EndsWith (value : substring) : bool =
        // Preconditions
        // TODO
        
        // If the value string is larger than this substring, the substring cannot start with the value.
        if value.Length > this.Length then false
        else
            let comparisonLength = min value.Length this.Length
            let thisStartOffset = this.Length - comparisonLength
            let valueStartOffset = value.Length - comparisonLength

#if INVARIANT_CULTURE_STRING_COMPARISON
            System.String.Compare (
                this.String, thisStartOffset,
                value, valueStartOffset,
                comparisonLength,
                false,
                CultureInfo.InvariantCulture) = 0
#else
            System.String.CompareOrdinal (
                this.String, thisStartOffset,
                value.String, valueStartOffset,
                comparisonLength) = 0
#endif

    /// <summary>Determines whether the end of this substring value matches the specified string.</summary>
    /// <param name="value">The string to compare.</param>
    /// <returns></returns>
    member this.EndsWith (value : string) : bool =
        // Preconditions
        checkNonNull "value" value

        let valueLen = String.length value
        
        // If the value string is larger than this substring, the substring cannot start with the value.
        if valueLen > this.Length then false
        else
            let comparisonLength = min valueLen this.Length
            let thisStartOffset = this.Length - comparisonLength
            let valueStartOffset = valueLen - comparisonLength

#if INVARIANT_CULTURE_STRING_COMPARISON
            System.String.Compare (
                this.String, thisStartOffset,
                value, valueStartOffset,
                comparisonLength,
                false,
                CultureInfo.InvariantCulture) = 0
#else
            System.String.CompareOrdinal (
                this.String, thisStartOffset,
                value, valueStartOffset,
                comparisonLength) = 0
#endif

    /// <inherit />
    override this.Equals other =
        match other with
        | :? substring as other ->
            substring.CompareOrdinal (this, other) = 0
        | _ ->
            invalidArg "other" "The value is not a substring."

    /// <inherit />
    override this.GetHashCode () =
        if isNull this.String then 0
        else
            // OPTIMIZE : This needs to be re-implemented ASAP so it directly computes the
            // hash value of the substring (i.e., without creating the substring).
            this.ToString().GetHashCode ()

    /// <summary>Implements F# slicing syntax for substrings.</summary>
    /// <param name="startIndex"></param>
    /// <param name="endIndex"></param>
    /// <returns></returns>
    member this.GetSlice (startIndex, endIndex) : substring =
        let len = this.Length
        let startIndex = defaultArg startIndex 0
        let endIndex = defaultArg endIndex (len - 1)

        // Validate preconditions.
        if startIndex < 0 then
            invalidArg "startIndex" "The start index cannot be negative."
        elif startIndex >= len then
            invalidArg "startIndex" "The start index must be less than the length of the substring."
        elif endIndex < 0 then
            invalidArg "endIndex" "The end index cannot be negative."
        elif endIndex >= len then
            invalidArg "endIndex" "The end index must be less than the length of the substring."

        // To emulate the same behavior used in other F# slicing operators (e.g., on array),
        // when 'startIndex' > 'endIndex' it's not considered an error --
        // just return an empty substring based on the input substring.
        if startIndex > endIndex then
            substring (this.String, 0, 0)
        else
            let startOffset = this.Offset + startIndex
            let sliceLength = this.Offset + ((endIndex - startIndex) + 1)
            substring (this.String, startOffset, sliceLength)

    /// <summary>
    /// Reports the zero-based index of the first occurrence of the specified Unicode character in this substring.
    /// </summary>
    /// <param name="value">A Unicode character to seek.</param>
    /// <returns>The zero-based index position of <paramref name="value"/> if that character is found, or -1 if it is not.</returns>
    member this.IndexOf (value : char) : int =
        this.IndexOf (value, 0)

    /// <summary>
    /// Reports the zero-based index of the first occurrence of the specified Unicode character in this substring.
    /// The search starts at a specified character position.
    /// </summary>
    /// <param name="value">A Unicode character to seek.</param>
    /// <param name="startIndex">The search starting position.</param>
    /// <returns>The zero-based index position of <paramref name="value"/> if that character is found, or -1 if it is not.</returns>
    member this.IndexOf (value : char, startIndex : int) : int =
        // Preconditions
        if startIndex < 0 then
            argOutOfRange "startIndex" "The start index is negative."
        elif startIndex > this.Length then
            argOutOfRange "startIndex" "The start index is past the end of the substring."

        // OPTIMIZATION : If this is an empty substring, we can return immediately.
        if this.Length = 0 then -1
        else
            match this.String.IndexOf (value, this.Offset + startIndex, this.Length - startIndex) with
            | -1 -> -1
            | baseIndex ->
                baseIndex - this.Offset

    /// <summary>
    /// Reports the zero-based index position of the last occurrence of a specified Unicode character within this instance.
    /// </summary>
    /// <param name="value">The Unicode character to seek.</param>
    /// <returns>
    /// The zero-based index position of <paramref name="value"/> if that character is found, or -1 if it is not found
    /// or if the current instance is an empty substring.
    /// </returns>
    member this.LastIndexOf (value : char) : int =
        // OPT : Immediately return -1 if this is an empty substring.
        if this.Length = 0 then -1
        else
            this.LastIndexOf (value, this.Length - 1)

    /// <summary>
    /// Reports the zero-based index position of the last occurrence of a specified Unicode character within this instance.
    /// The search starts at a specified character position and proceeds backward toward the beginning of the substring.
    /// </summary>
    /// <param name="value">The Unicode character to seek.</param>
    /// <param name="startIndex">
    /// The starting position of the search. The search proceeds from <paramref name="startIndex"/> toward the beginning of this instance.
    /// </param>
    /// <returns>The zero-based index position of <paramref name="value"/> if that character is found, or -1 if it is not.</returns>
    member this.LastIndexOf (value : char, startIndex : int) : int =
        // Preconditions
        if startIndex < 0 then
            argOutOfRange "startIndex" "The start index is negative."
        elif startIndex >= this.Length then
            argOutOfRange "startIndex" "The start index is past the end of the substring."

        // OPTIMIZATION : If this is an empty substring, we can return immediately.
        if this.Length = 0 then -1
        else
            match this.String.LastIndexOf (value, this.Offset + startIndex, this.Length - startIndex) with
            | -1 -> -1
            | baseIndex ->
                baseIndex - this.Offset

    /// <summary>Determines whether the beginning of this substring value matches the specified substring.</summary>
    /// <param name="value">The substring to compare.</param>
    /// <returns></returns>
    member this.StartsWith (value : substring) : bool =
        // Preconditions
        // TODO
        
        // If the value string is larger than this substring, the substring cannot start with the value.
        if value.Length > this.Length then false
        else
            let comparisonLength = min value.Length this.Length

#if INVARIANT_CULTURE_STRING_COMPARISON
            System.String.Compare (
                this.String, this.Offset,
                value, 0,
                comparisonLength,
                false,
                CultureInfo.InvariantCulture) = 0
#else
            System.String.CompareOrdinal (
                this.String, this.Offset,
                value.String, value.Offset,
                comparisonLength) = 0
#endif

    /// <summary>Determines whether the beginning of this substring value matches the specified string.</summary>
    /// <param name="value">The string to compare.</param>
    /// <returns></returns>
    member this.StartsWith (value : string) : bool =
        // Preconditions
        checkNonNull "value" value

        let valueLen = String.length value
        
        // If the value string is larger than this substring, the substring cannot start with the value.
        if valueLen > this.Length then false
        else
            let comparisonLength = min valueLen this.Length

#if INVARIANT_CULTURE_STRING_COMPARISON
            System.String.Compare (
                this.String, this.Offset,
                value, 0,
                comparisonLength,
                false,
                CultureInfo.InvariantCulture) = 0
#else
            System.String.CompareOrdinal (
                this.String, this.Offset,
                value, 0,
                comparisonLength) = 0
#endif

    /// <summary>Copies the characters in this substring into a Unicode character array.</summary>
    /// <returns></returns>
    member this.ToCharArray () : char[] =
        #if FX_ATLEAST_PORTABLE
        // Manually implement String.ToCharArray(int,int) for portable libraries.
        let chars = Array.zeroCreate this.Length
        for i = 0 to this.Length - 1 do
            chars.[i] <- this.String.[this.Offset + i]
        chars

        #else
        this.String.ToCharArray (this.Offset, this.Length)
        #endif

    /// <inherit />
    override this.ToString () =
        // OPTIMIZATION : Immediately return if this is an empty substring;
        // or, if the substring covers the entire string, just return the string.
        if this.Length = 0 then
            System.String.Empty
        elif this.Offset = 0 && this.Length = this.String.Length then
            this.String
        else
            this.String.Substring (this.Offset, this.Length)

    //
    member this.TryRead ([<Out>] c : byref<char>, [<Out>] remaining : byref<substring>) : bool =
        if this.Length = 0 then
            // Assign default values to the output parameters.
            c <- Unchecked.defaultof<_>
            remaining <- Unchecked.defaultof<_>
            false
        else
            // Extract the first (left-most) character from the substring.
            c <- this.[0]
            remaining <- this.[1..]
            true

    /// <summary>
    /// Compares two specified <see cref="substring"/> objects by evaluating the numeric values of the corresponding
    /// <see cref="Char"/> objects in each substring.
    /// </summary>
    /// <param name="strA">The first string to compare.</param>
    /// <param name="strB">The second string to compare.</param>
    /// <returns>An integer that indicates the lexical relationship between the two comparands.</returns>
    static member CompareOrdinal (strA : substring, strB : substring) =
        // If both substrings are empty they are considered equal, regardless of their offset or underlying string.
        if strA.Length = 0 && strB.Length = 0 then 0

        // OPTIMIZATION : If the substrings have the same (identical) underlying string
        // and offset, the comparison value will depend only on the length of the substrings.
        elif strA.String == strB.String && strA.Offset = strB.Offset then
            compare strA.Length strB.Length

        else
            (* Structural comparison on substrings -- this uses the same comparison
               technique as the structural comparison on strings in FSharp.Core. *)
#if INVARIANT_CULTURE_STRING_COMPARISON
            // NOTE: we don't have to null check here because System.String.Compare
            // gives reliable results on null values.
            System.String.Compare (
                strA.String, strA.Offset,
                strB.String, strB.Offset,
                min strA.Length strB.Length,
                false,
                CultureInfo.InvariantCulture)
#else
            // NOTE: we don't have to null check here because System.String.CompareOrdinal
            // gives reliable results on null values.
            System.String.CompareOrdinal (
                strA.String, strA.Offset,
                strB.String, strB.Offset,
                min strA.Length strB.Length)
#endif

    interface IEquatable<substring> with
        /// <inherit />
        member this.Equals other =
            substring.CompareOrdinal (this, other) = 0

    interface IComparable with
        /// <inherit />
        member this.CompareTo other =
            match other with
            | :? substring as other ->
                substring.CompareOrdinal (this, other)
            | _ ->
                invalidArg "other" "The value is not a substring."

    interface IComparable<substring> with
        /// <inherit />
        member this.CompareTo other =
            substring.CompareOrdinal (this, other)

    interface System.Collections.IEnumerable with
        /// <inherit />
        member this.GetEnumerator () =
            let substringSeq =
                let thisLocal = this
                seq {
                for i in 0 .. thisLocal.Length - 1 do
                    yield thisLocal.String.[thisLocal.Offset + i] }
            substringSeq.GetEnumerator () :> System.Collections.IEnumerator

    interface System.Collections.Generic.IEnumerable<char> with
        /// <inherit />
        member this.GetEnumerator () =
            let substringSeq =
                let thisLocal = this
                seq {
                for i in 0 .. thisLocal.Length - 1 do
                    yield thisLocal.String.[thisLocal.Offset + i] }
            substringSeq.GetEnumerator ()


/// Functional operators related to substrings.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Substring =
    open OptimizedClosures

    /// An empty substring value.
    [<CompiledName("Empty")>]
    let empty : substring =
        substring (System.String.Empty, 0, 0)

    /// <summary>Returns the string underlying the given substring.</summary>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("String")>]
    let inline string (substr : substring) : string =
        substr.String

    /// <summary>The starting offset of the substring within it's underlying string.</summary>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("Offset")>]
    let inline offset (substr : substring) : int =
        substr.Offset

    /// <summary>Returns the length of the substring.</summary>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("Length")>]
    let inline length (substr : substring) : int =
        substr.Length

    /// <summary>Gets a character from the substring.</summary>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("Get")>]
    let inline get (substr : substring) index : char =
        substr.[index]

    /// <summary>Is the substring empty?</summary>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (substr : substring) : bool =
        substr.IsEmpty

    /// <summary>Returns a substring which covers the entire length of the given string.</summary>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("OfString")>]
    let inline ofString (str : string) : substring =
        substring (str, 0, str.Length)

    /// <summary>Instantiates the substring as a string.</summary>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("ToString")>]
    let inline toString (substr : substring) : string =
        substr.ToString ()

    /// <summary>Returns the characters in the given substring as a Unicode character array.</summary>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("ToArray")>]
    let inline toArray (substr : substring) : char[] =
        substr.ToCharArray ()

    /// <summary>Determines whether the beginning of a substring matches the specified string.</summary>
    /// <param name="value"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("StartsWith")>]
    let inline startsWith (value : string) (substr : substring) : bool =
        substr.StartsWith value

    /// <summary>Determines whether the end of a substring matches the specified string.</summary>
    /// <param name="value"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("EndsWith")>]
    let inline endsWith (value : string) (substr : substring) : bool =
        substr.EndsWith value

    /// <summary>
    /// Extracts the first (left-most) character from a substring, returning a <c>Some</c> value
    /// containing the character and the remaining substring.
    /// Returns <c>None</c> if the given substring is empty.
    /// </summary>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("Read")>]
    let read (substr : substring) : (char * substring) option =
        // Wrap the Substring.TryRead method in a more functional style.
        let mutable c = Unchecked.defaultof<_>
        let mutable remaining = Unchecked.defaultof<_>
        if substr.TryRead (&c, &remaining) then
            Some (c, remaining)
        else None

    //
    let inline private subUnsafe (substr : substring) offset count : substring =
        // Create a new substring based on the input substring.
        substring (substr.String, substr.Offset + offset, count)

    /// <summary>Gets a substring of a substring.</summary>
    /// <param name="substr"></param>
    /// <param name="offset"></param>
    /// <param name="count"></param>
    /// <returns></returns>
    [<CompiledName("Sub")>]
    let sub (substr : substring) offset count : substring =
        // Preconditions
        if offset < 0 then
            argOutOfRange "offset" "The offset cannot be negative."
        elif count < 0 then
            argOutOfRange "count" "The length of the substring cannot be negative."
        elif offset >= substr.Length then
            argOutOfRange "offset" "The offset must be less than the length of the input substring."
        elif (offset + count) >= substr.Length then
            argOutOfRange "count" "There are fewer than 'count' elements in the \
                                   input substring when starting at the given offset."

        // Create a new substring based on the input substring.
        subUnsafe substr offset count

    /// <summary>Builds a new string by concatenating the given sequence of substrings.</summary>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("Concat")>]
    let concat (source : seq<substring>) : string =
        // Preconditions
        checkNonNull "source" source

        // If the sequence is empty, return immediately.
        if Seq.isEmpty source then
            System.String.Empty
        else
            let sb =
                // OPT: If the input source is an array, we can easily determine the exact capacity needed for the StringBuilder,
                //      thereby avoiding (potentially) multiple resizes of the underlying buffer.
                match source with
                | :? (substring[]) as sourceArr ->
                    let resultLength = Array.sumBy length sourceArr
                    System.Text.StringBuilder (resultLength)
                | _ ->
                    System.Text.StringBuilder ()
            for substr in source do
                sb.Append substr |> ignore
            sb.ToString ()

    /// <summary>Returns the index of the first occurrence of a specified character within a substring.</summary>
    /// <param name="value"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("TryFindIndexOf")>]
    let tryFindIndexOf (value : char) (substr : substring) =
        // Preconditions
        // (None)

        match substr.IndexOf value with
        | -1 -> None
        | idx -> Some idx

    /// <summary>Returns the index of the first occurrence of a specified character within a substring.</summary>
    /// <param name="value"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("FindIndexOf")>]
    let findIndexOf (value : char) (substr : substring) =
        // Preconditions
        // (None)

        match substr.IndexOf value with
        | -1 ->
            // TODO : Return a better error message.
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

        | idx -> idx

    /// <summary>Returns the index of the last occurrence of a specified character within a substring.</summary>
    /// <param name="value"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("TryFindIndexOfBack")>]
    let tryFindIndexOfBack (value : char) (substr : substring) =
        // Preconditions
        // (None)

        match substr.LastIndexOf value with
        | -1 -> None
        | idx -> Some idx

    /// <summary>Returns the index of the last occurrence of a specified character within a substring.</summary>
    /// <param name="value"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("FindIndexOfBack")>]
    let findIndexOfBack (value : char) (substr : substring) =
        // Preconditions
        // (None)

        match substr.LastIndexOf value with
        | -1 ->
            // TODO : Return a better error message.
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

        | idx -> idx

    /// <summary>Returns the index of the first character in the substring which satisfies the given predicate.</summary>
    /// <param name="predicate"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("TryFindIndex")>]
    let tryFindIndex (predicate : char -> bool) (substr : substring) : int option =
        // Preconditions
        // (None)

        let len = substr.Length

        let mutable index = 0
        let mutable foundMatch = false

        while index < len && not foundMatch do
            if predicate substr.[index] then
                foundMatch <- true
            else
                index <- index + 1

        // Return the index of the matching character, if any.
        if foundMatch then
            Some index
        else None

    /// <summary>Returns the index of the first character in the substring which satisfies the given predicate.</summary>
    [<CompiledName("FindIndex")>]
    let findIndex (predicate : char -> bool) (substr : substring) : int =
        // Preconditions
        // (None)

        // Use tryFindIndex to find the match; raise an exception if one is not found.
        match tryFindIndex predicate substr with
        | Some index ->
            index
        | None ->
            // TODO : Return a better error message.
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    /// <summary>Returns the index of the last character in the substring which satisfies the given predicate.</summary>
    /// <param name="predicate"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("TryFindIndexBack")>]
    let tryFindIndexBack (predicate : char -> bool) (substr : substring) : int option =
        // Preconditions
        // (None)

        let len = substr.Length

        let mutable index = len - 1
        let mutable foundMatch = false

        while index >= 0 && not foundMatch do
            if predicate substr.[index] then
                foundMatch <- true
            else
                index <- index - 1

        // Return the index of the matching character, if any.
        if foundMatch then
            Some index
        else None

    /// <summary>Returns the index of the last character in the substring which satisfies the given predicate.</summary>
    /// <param name="predicate"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("FindIndexBack")>]
    let findIndexBack (predicate : char -> bool) (substr : substring) : int =
        // Preconditions
        // (None)

        // Use tryFindIndexBack to find the match; raise an exception if one is not found.
        match tryFindIndexBack predicate substr with
        | Some index ->
            index
        | None ->
            // TODO : Return a better error message.
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    /// <summary>Returns the first character in the substring which satisfies the given predicate.</summary>
    /// <param name="predicate"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("TryFind")>]
    let tryFind (predicate : char -> bool) (substr : substring) : char option =
        // Preconditions
        // (None)

        let len = substr.Length
        
        let mutable index = 0
        let mutable foundMatch = false

        while index < len && not foundMatch do
            if predicate substr.[index] then
                foundMatch <- true
            else
                index <- index + 1

        // Return the matching character, if any.
        if foundMatch then
            Some substr.[index]
        else None

    /// <summary>Returns the first character in the substring which satisfies the given predicate.</summary>
    /// <param name="predicate"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("Find")>]
    let find (predicate : char -> bool) (substr : substring) : char =
        // Preconditions
        // (None)

        // Use tryFind to find the match; raise an exception if one is not found.
        match tryFind predicate substr with
        | Some ch ->
            ch
        | None ->
            // TODO : Return a better error message.
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    /// <summary>
    /// Applies the given function to successive characters, returning the first result where the function
    /// returns <c>Some(x)</c>. If the function never returns <c>Some(x)</c>, <c>None</c> is returned.
    /// </summary>
    /// <param name="picker"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("TryPick")>]
    let tryPick (picker : char -> 'T option) (substr : substring) : 'T option =
        // Preconditions
        // (None)

        let len = substr.Length
        
        let mutable picked = None
        let mutable index = 0

        while index < len && Option.isNone picked do
            match picker substr.[index] with
            | None ->
                index <- index + 1
            | Some _ as x ->
                picked <- x

        // Return the picked value, if any.
        picked

    /// <summary>
    /// Applies the given function to successive characters, returning the first result where the function
    /// returns <c>Some(x)</c>. If the function never returns <c>Some(x)</c>, <c>KeyNotFoundException</c> is raised.
    /// </summary>
    /// <param name="picker"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("Pick")>]
    let pick (picker : char -> 'T option) (substr : substring) : 'T =
        // Preconditions
        // (None)

        // Use tryPick to find the match; raise an exception if one is not found.
        match tryPick picker substr with
        | Some result ->
            result
        | None ->
            // TODO : Return a better error message.
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    /// <summary>Applies the given function to each character in the substring, in order from lowest to highest indices.</summary>
    /// <param name="action"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("Iterate")>]
    let iter action (substr : substring) : unit =
        let len = substr.Length
        for i = 0 to len - 1 do
            action substr.[i]

    /// <summary>
    /// Applies the given function to each character in the substring,
    /// in order from lowest to highest indices.
    /// The integer index applied to the function is the character's index within the substring.
    /// </summary>
    /// <param name="action"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("IterateIndexed")>]
    let iteri action (substr : substring) : unit =
        // OPTIMIZATION : Immediately return if the substring is empty.
        let len = substr.Length
        if len > 0 then
            let action = FSharpFunc<_,_,_>.Adapt action

            for i = 0 to len - 1 do
                action.Invoke (i, substr.[i])

    /// <summary>Applies the given function to each character in the substring, in order from highest to lowest indices.</summary>
    /// <param name="action"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("IterateBack")>]
    let iterBack action (substr : substring) : unit =
        let len = substr.Length
        for i = len - 1 downto 0 do
            action substr.[i]

    /// <summary>
    /// Applies the given function to each character in the substring, in order from highest to lowest indices.
    /// The integer index applied to the function is the character's index within the substring.
    /// </summary>
    /// <param name="action"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("IterateIndexedBack")>]
    let iteriBack action (substr : substring) : unit =
        // OPTIMIZATION : Immediately return if the substring is empty.
        let len = substr.Length
        if len > 0 then
            let action = FSharpFunc<_,_,_>.Adapt action

            for i = len - 1 downto 0 do
                action.Invoke (i, substr.[i])

    /// <summary>
    /// Applies a function to each character of the substring, threading an accumulator argument through the computation.
    /// If the input function is f and the characters are c0...cN then computes f (...(f s c0)...) cN.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> char -> 'State) state (substr : substring) : 'State =
        // OPTIMIZATION : Immediately return if the substring is empty.
        let len = substr.Length
        if len = 0 then state
        else
            let folder = FSharpFunc<_,_,_>.Adapt folder
            
            let mutable state = state
            for i = 0 to len - 1 do
                state <- folder.Invoke (state, substr.[i])
            state

    /// <summary>
    /// Applies a function to each character of the substring, threading an accumulator argument through the computation.
    /// The integer index applied to the function is the character's index within the substring.
    /// If the input function is f and the characters are c0...cN then computes f (...(f s c0)...) cN.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : 'State -> int -> char -> 'State) state (substr : substring) : 'State =
        // OPTIMIZATION : Immediately return if the substring is empty.
        let len = substr.Length
        if len = 0 then state
        else
            let folder = FSharpFunc<_,_,_,_>.Adapt folder
            
            let mutable state = state
            for i = 0 to len - 1 do
                state <- folder.Invoke (state, i, substr.[i])
            state

    /// <summary>
    /// Applies a function to each character of the string, threading an accumulator argument through the computation.
    /// If the input function is f and the characters are c0...cN then computes f c0 (...(f cN s)).
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="substr"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    [<CompiledName("FoldBack")>]
    let foldBack (folder : char -> 'State -> 'State) (substr : substring) state : 'State =
        // OPTIMIZATION : Immediately return if the substring is empty.
        let len = substr.Length
        if len = 0 then state
        else
            let folder = FSharpFunc<_,_,_>.Adapt folder
            
            let mutable state = state
            for i = len - 1 downto 0 do
                state <- folder.Invoke (substr.[i], state)
            state

    /// <summary>Removes all leading occurrences of characters not satisfying the given predicate from a substring.</summary>
    /// <param name="predicate"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("TrimStartWith")>]
    let trimStartWith (predicate : char -> bool) (substr : substring) =
        // Preconditions
        // (None)

        // OPTIMIZATION : If the substring is empty, return immediately.
        if substr.IsEmpty then substr
        else
            match tryFindIndex predicate substr with
            | None ->
                // No characters matched, return an empty substring based on the input substring.
                substring (substr.String, 0, 0)
            | Some index ->
                substr.[index..]

    /// <summary>Removes all trailing occurrences of characters not satisfying the given predicate from a substring.</summary>
    /// <param name="predicate"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("TrimEndWith")>]
    let trimEndWith (predicate : char -> bool) (substr : substring) =
        // Preconditions
        // (None)

        // OPTIMIZATION : If the substring is empty, return immediately.
        if substr.IsEmpty then substr
        else
            match tryFindIndexBack predicate substr with
            | None ->
                // No characters matched, return an empty substring based on the input substring.
                substring (substr.String, 0, 0)
            | Some index ->
                substr.[..index]

    /// <summary>Removes all leading and trailing occurrences of characters not satisfying the given predicate from a substring.</summary>
    /// <param name="predicate"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("TrimWith")>]
    let trimWith (predicate : char -> bool) (substr : substring) =
        // Preconditions
        // (None)

        trimStartWith predicate (trimEndWith predicate substr)

    /// <summary>Removes all leading occurrences of the specified set of characters from a substring.</summary>
    /// <param name="chars"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("TrimStart")>]
    let trimStart (chars : char[]) (substr : substring) =
        // Preconditions
        checkNonNull "chars" chars

        // OPTIMIZATION : If the substring is empty, return immediately.
        if substr.IsEmpty then substr
        else
            trimStartWith (fun c -> Array.exists ((=) c) chars) substr

    /// <summary>Removes all trailing occurrences of the specified set of characters from a substring.</summary>
    /// <param name="chars"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("TrimEnd")>]
    let trimEnd (chars : char[]) (substr : substring) =
        // Preconditions
        checkNonNull "chars" chars
        
        // OPTIMIZATION : If the substring is empty, return immediately.
        if substr.IsEmpty then substr
        else
            trimEndWith (fun c -> Array.exists ((=) c) chars) substr

    /// <summary>Removes all leading and trailing occurrences of the specified characters from a substring.</summary>
    /// <param name="chars"></param>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("Trim")>]
    let trim (chars : char[]) (substr : substring) =
        // Preconditions
        checkNonNull "chars" chars

        // OPTIMIZATION : If the substring is empty, return immediately.
        if substr.IsEmpty then substr
        else
            trimWith (fun c -> Array.exists ((=) c) chars) substr

    /// Substring-splitting functions.
    /// These functions are analagous to calling the String.Split method with StringSplitOptions.None,
    /// but are faster because they avoid creating the intermediate array of substrings.
    [<RequireQualifiedAccess>]
    module Split =
        open System

        // OPTIMIZE : The functions below could be modified to include optimized cases
        // for when the separator array contains just one or two characters.

        //
        let private iterDefault (action : substring -> unit) (substr : substring) : unit =
            /// The length of the input substring.
            let len = substr.Length

            // The offset and length of the current substring.
            let mutable offset = 0
            let mutable length = 0

            for i = 0 to len - 1 do
                // Is the current character a separator?
                if Char.IsWhiteSpace substr.[i] then
                    // Apply the function to the current substring.
                    action <| subUnsafe substr offset length

                    // Update the offset to just past the end of this substring
                    // and reset the length to zero to begin a new substring.
                    offset <- i + 1
                    length <- 0

                else
                    // "Add" this character to the current substring by
                    // incrementing the substring length.
                    length <- length + 1

            // If the length is nonzero, then the last substring is still "in-progress"
            // so apply the function to it.
            if length > 0 then
                action <| subUnsafe substr offset length

        //
        let private iterSeparators (separator : char[]) (action : substring -> unit) (substr : substring) : unit =
            /// The length of the input substring.
            let len = substr.Length

            /// A sorted copy of the separator array. Used with Array.BinarySearch
            /// to quickly determine if a given character is a separator.
            let sortedSeparators = Array.sort separator

            // The offset and length of the current substring.
            let mutable offset = 0
            let mutable length = 0

            for i = 0 to len - 1 do
                // Is the current character a separator?
                if Array.BinarySearch (sortedSeparators, substr.[i]) >= 0 then
                    // Apply the function to the current substring.
                    action <| subUnsafe substr offset length

                    // Update the offset to just past the end of this substring
                    // and reset the length to zero to begin a new substring.
                    offset <- i + 1
                    length <- 0

                else
                    // "Add" this character to the current substring by
                    // incrementing the substring length.
                    length <- length + 1

            // If the length is nonzero, then the last substring is still "in-progress"
            // so apply the function to it.
            if length > 0 then
                action <| subUnsafe substr offset length

        /// <summary>
        /// Applies the given function to each of the substrings in the input string that are
        /// delimited by elements of a specified Unicode character array.
        /// </summary>
        /// <param name="separator"></param>
        /// <param name="action"></param>
        /// <param name="substr"></param>
        /// <returns></returns>
        [<CompiledName("Iterate")>]
        let iter (separator : char[]) (action : substring -> unit) (substr : substring) : unit =
            // Preconditions
            // (None)

            // OPTIMIZATION : If the input substring is empty, return immediately.
            if isEmpty substr then ()
            elif isNull separator || Array.isEmpty separator then
                // The case where the separator array is null or empty needs special handling
                // to maintain drop-in compatibilty with String.Split; in this case, any whitespace
                // character is treated as a separator.
                iterDefault action substr
            else
                iterSeparators separator action substr
                
        //
        let private iteriDefault (action : FSharpFunc<_,_,_>) (substr : substring) : unit =
            /// The length of the input substring.
            let len = substr.Length

            // The offset and length of the current substring.
            let mutable offset = 0
            let mutable length = 0

            /// The current substring index (amongst the delimited substrings in the input string).
            let mutable substringIndex = 0

            for i = 0 to len - 1 do
                // Is the current character a separator?
                if Char.IsWhiteSpace substr.[i] then
                    // Apply the function to the current substring.
                    action.Invoke (substringIndex, subUnsafe substr offset length)

                    // Update the offset to just past the end of this substring
                    // and reset the length to zero to begin a new substring.
                    offset <- i + 1
                    length <- 0

                    // Increment the substring index.
                    substringIndex <- substringIndex + 1

                else
                    // "Add" this character to the current substring by
                    // incrementing the substring length.
                    length <- length + 1

            // If the length is nonzero, then the last substring is still "in-progress"
            // so apply the function to it.
            if length > 0 then
                action.Invoke (substringIndex, subUnsafe substr offset length)

        //
        let private iteriSeparators (separator : char[]) (action : FSharpFunc<_,_,_>) (substr : substring) : unit =
            /// The length of the input substring.
            let len = substr.Length

            /// A sorted copy of the separator array. Used with Array.BinarySearch
            /// to quickly determine if a given character is a separator.
            let sortedSeparators = Array.sort separator

            // The offset and length of the current substring.
            let mutable offset = 0
            let mutable length = 0

            /// The current substring index (amongst the delimited substrings in the input string).
            let mutable substringIndex = 0

            for i = 0 to len - 1 do
                // Is the current character a separator?
                if Array.BinarySearch (sortedSeparators, substr.[i]) >= 0 then
                    // Apply the function to the current substring.
                    action.Invoke (substringIndex, subUnsafe substr offset length)

                    // Update the offset to just past the end of this substring
                    // and reset the length to zero to begin a new substring.
                    offset <- i + 1
                    length <- 0

                    // Increment the substring index.
                    substringIndex <- substringIndex + 1

                else
                    // "Add" this character to the current substring by
                    // incrementing the substring length.
                    length <- length + 1

            // If the length is nonzero, then the last substring is still "in-progress"
            // so apply the function to it.
            if length > 0 then
                action.Invoke (substringIndex, subUnsafe substr offset length)

        /// <summary>
        /// Applies the given function to each of the substrings in the input string that are delimited by elements of a specified
        /// Unicode character array. The integer index applied to the function is the index of the substring within the virtual array
        /// of substrings in the input string. For example, if the newline character (\n) is used as the separator, the index of each
        /// substring would be the line number.
        /// </summary>
        /// <param name="separator"></param>
        /// <param name="action"></param>
        /// <param name="substr"></param>
        /// <returns></returns>
        [<CompiledName("IterateIndexed")>]
        let iteri (separator : char[]) (action : int -> substring -> unit) (substr : substring) : unit =
            // Preconditions
            // (None)

            // OPTIMIZATION : If the input substring is empty, call the action with an empty substring and return.
            if isEmpty substr then
                action 0 substr
            else
                let action = FSharpFunc<_,_,_>.Adapt action
                if isNull separator || Array.isEmpty separator then
                    // The case where the separator array is null or empty needs special handling
                    // to maintain drop-in compatibilty with String.Split; in this case, any whitespace
                    // character is treated as a separator.
                    iteriDefault action substr
                else
                    iteriSeparators separator action substr

        //
        let private foldDefault (folder : FSharpFunc<_,_,_>) (state : 'State) (substr : substring) : 'State =
            /// The length of the input substring.
            let len = substr.Length

            // The offset and length of the current substring.
            let mutable offset = 0
            let mutable length = 0

            /// The current state value.
            let mutable state = state

            for i = 0 to len - 1 do
                // Is the current character a separator?
                if Char.IsWhiteSpace substr.[i] then
                    // Apply the function to the current substring.
                    state <- folder.Invoke (state, subUnsafe substr offset length)

                    // Update the offset to just past the end of this substring
                    // and reset the length to zero to begin a new substring.
                    offset <- i + 1
                    length <- 0

                else
                    // "Add" this character to the current substring by
                    // incrementing the substring length.
                    length <- length + 1

            // If the length is nonzero, then the last substring is still "in-progress"
            // so apply the function to it.
            if length > 0 then
                state <- folder.Invoke (state, subUnsafe substr offset length)
            state

        //
        let private foldSeparators (separator : char[]) (folder : FSharpFunc<_,_,_>) (state : 'State) (substr : substring) : 'State =
            /// The length of the input substring.
            let len = substr.Length

            /// A sorted copy of the separator array. Used with Array.BinarySearch
            /// to quickly determine if a given character is a separator.
            let sortedSeparators = Array.sort separator

            // The offset and length of the current substring.
            let mutable offset = 0
            let mutable length = 0

            /// The current state value.
            let mutable state = state

            for i = 0 to len - 1 do
                // Is the current character a separator?
                if Array.BinarySearch (sortedSeparators, substr.[i]) >= 0 then
                    // Apply the function to the current substring.
                    state <- folder.Invoke (state, subUnsafe substr offset length)

                    // Update the offset to just past the end of this substring
                    // and reset the length to zero to begin a new substring.
                    offset <- i + 1
                    length <- 0

                else
                    // "Add" this character to the current substring by
                    // incrementing the substring length.
                    length <- length + 1

            // If the length is nonzero, then the last substring is still "in-progress"
            // so apply the function to it.
            if length > 0 then
                state <- folder.Invoke (state, subUnsafe substr offset length)
            state

        /// <summary>
        /// Applies the given function to each of the substrings in the input string that are delimited by elements of a specified
        /// Unicode character array, threading an accumulator argument through the computation.
        /// </summary>
        /// <param name="separator"></param>
        /// <param name="folder"></param>
        /// <param name="state"></param>
        /// <param name="substr"></param>
        /// <returns></returns>
        [<CompiledName("Fold")>]
        let fold (separator : char[]) (folder : 'State -> substring -> 'State) (state : 'State) (substr : substring) : 'State =
            // Preconditions
            // (None)

            // OPTIMIZATION : If the input substring is empty, just call the folder with an empty substring then return.
            if isEmpty substr then
                folder state substr
            else
                let folder = FSharpFunc<_,_,_>.Adapt folder

                // The case where the separator array is null or empty needs special handling
                // to maintain drop-in compatibilty with String.Split; in this case, any whitespace
                // character is treated as a separator.
                if isNull separator || Array.isEmpty separator then
                    foldDefault folder state substr
                else
                    foldSeparators separator folder state substr

        let private foldiDefault (folder : FSharpFunc<_,_,_,_>) (state : 'State) (substr : substring) : 'State =
            /// The length of the input substring.
            let len = substr.Length

            // The offset and length of the current substring.
            let mutable offset = 0
            let mutable length = 0

            /// The current substring index (amongst the delimited substrings in the input string).
            let mutable substringIndex = 0

            /// The current state value.
            let mutable state = state

            for i = 0 to len - 1 do
                // Is the current character a separator?
                if Char.IsWhiteSpace substr.[i] then
                    // Apply the function to the current substring.
                    state <- folder.Invoke (state, substringIndex, subUnsafe substr offset length)

                    // Update the offset to just past the end of this substring
                    // and reset the length to zero to begin a new substring.
                    offset <- i + 1
                    length <- 0

                    // Increment the substring index.
                    substringIndex <- substringIndex + 1

                else
                    // "Add" this character to the current substring by
                    // incrementing the substring length.
                    length <- length + 1

            // If the length is nonzero, then the last substring is still "in-progress"
            // so apply the function to it.
            if length > 0 then
                state <- folder.Invoke (state, substringIndex, subUnsafe substr offset length)
            state

        let private foldiSeparators (separator : char[]) (folder : FSharpFunc<_,_,_,_>) (state : 'State) (substr : substring) : 'State =
            /// The length of the input substring.
            let len = substr.Length

            /// A sorted copy of the separator array. Used with Array.BinarySearch
            /// to quickly determine if a given character is a separator.
            let sortedSeparators = Array.sort separator

            // The offset and length of the current substring.
            let mutable offset = 0
            let mutable length = 0

            /// The current substring index (amongst the delimited substrings in the input string).
            let mutable substringIndex = 0

            /// The current state value.
            let mutable state = state

            for i = 0 to len - 1 do
                // Is the current character a separator?
                if Array.BinarySearch (sortedSeparators, substr.[i]) >= 0 then
                    // Apply the function to the current substring.
                    state <- folder.Invoke (state, substringIndex, subUnsafe substr offset length)

                    // Update the offset to just past the end of this substring
                    // and reset the length to zero to begin a new substring.
                    offset <- i + 1
                    length <- 0

                    // Increment the substring index.
                    substringIndex <- substringIndex + 1

                else
                    // "Add" this character to the current substring by
                    // incrementing the substring length.
                    length <- length + 1

            // If the length is nonzero, then the last substring is still "in-progress"
            // so apply the function to it.
            if length > 0 then
                state <- folder.Invoke (state, substringIndex, subUnsafe substr offset length)
            state

        /// <summary>
        /// Applies the given function to each of the substrings in the input string that are delimited by elements of a specified
        /// Unicode character array, threading an accumulator argument through the computation. The integer index applied to the function
        /// is the index of the substring within the virtual array of substrings in the input string. For example, if the newline character
        /// (\n) is used as the separator, the index of each substring would be the line number.
        /// </summary>
        /// <param name="separator"></param>
        /// <param name="folder"></param>
        /// <param name="state"></param>
        /// <param name="substr"></param>
        /// <returns></returns>
        [<CompiledName("FoldIndexed")>]
        let foldi (separator : char[]) (folder : 'State -> int -> substring -> 'State) (state : 'State) (substr : substring) : 'State =
            // Preconditions
            // (None)

            // OPTIMIZATION : If the input substring is empty, just call the folder with an empty substring then return.
            if isEmpty substr then
                folder state 0 substr
            else
                let folder = FSharpFunc<_,_,_,_>.Adapt folder

                // The case where the separator array is null or empty needs special handling
                // to maintain drop-in compatibilty with String.Split; in this case, any whitespace
                // character is treated as a separator.
                if isNull separator || Array.isEmpty separator then
                    foldiDefault folder state substr
                else
                    foldiSeparators separator folder state substr

        (*
        //
        [<CompiledName("Filter")>]
        let filter (separator : char[]) (predicate : substring -> bool) (str : string) : seq<substring> =
            // Preconditions
            checkNonNull "str" str

            // OPTIMIZATION : If the input string is empty, return immediately.
            if isEmpty str then Array.empty
            else
                notImpl "String.Split.filter"

        //
        [<CompiledName("Choose")>]
        let choose (separator : char[]) (chooser : substring -> 'T option) (str : string) : seq<'T> =
            // Preconditions
            checkNonNull "str" str

            // OPTIMIZATION : If the input string is empty, return immediately.
            if isEmpty str then Array.empty
            else
                notImpl "String.Split.choose"
        *)


/// <summary>
/// Extension methods for <see cref="System.String"/> and <see cref="System.Text.StringBuilder"/>
/// which provide integration with the substring type.
/// </summary>
module SubstringExtensions =
    type System.String with
        /// <summary>Returns a new substring created from this string and the given starting and ending indices.</summary>
        /// <param name="startIndex"></param>
        /// <param name="finishIndex"></param>
        /// <returns></returns>
        member this.GetSlice (startIndex, finishIndex) : substring =
            let startIndex = defaultArg startIndex 0 
            let finishIndex = defaultArg finishIndex this.Length
            substring (this, startIndex, finishIndex - startIndex + 1)

    type System.Text.StringBuilder with
        /// <summary>Appends a copy of the specified substring to this instance.</summary>
        /// <param name="substr"></param>
        /// <returns></returns>
        member this.Append (value : substring) : System.Text.StringBuilder =
            // OPTIMIZATION : If the substring is empty, return immediately.
            if value.IsEmpty then this
            else
                this.Append (value.String, value.Offset, value.Length)
            
        /// <summary>Appends a copy of the specified substring to this instance, appending a newline.</summary>
        /// <param name="value"></param>
        /// <returns></returns>
        member this.AppendLine (value : substring) : System.Text.StringBuilder =
            let this = this.Append value
            this.AppendLine ()

    type System.Text.RegularExpressions.Regex with
        /// <summary>Searches the input substring for the first occurrence of a regular expression.</summary>
        /// <param name="value"></param>
        /// <returns></returns>
        member this.Match (value : substring) =
            this.Match (value.String, value.Offset, value.Length)
