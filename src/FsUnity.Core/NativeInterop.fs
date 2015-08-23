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

namespace FsUnity.NativeInterop

open Microsoft.FSharp.NativeInterop

#nowarn "9"     // Disable warning about unverifiable IL


/// Functions for working with native pointers.
[<RequireQualifiedAccess>]
module NativePtr =
    /// The null pointer.
    //[<Literal>]
    [<CompiledName("Zero")>]
    let zero<'T when 'T : unmanaged> : nativeptr<'T> =
        NativePtr.ofNativeInt 0n

    /// Determines if a pointer is null.
    [<CompiledName("IsNull")>]
    let [<NoDynamicInvocation>] inline isNull (ptr : nativeptr<'T>) =
        0n = NativePtr.toNativeInt ptr


