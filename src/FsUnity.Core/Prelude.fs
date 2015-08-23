namespace FsUnity

open System
open System.Text

[<AutoOpen>]
module Prelude =


    /// Special isNull operator that works on Unity3d fake 'nulls'
    let inline isNull x = 
        let y = box x // In case of value types
        obj.ReferenceEquals (y, Unchecked.defaultof<_>) || y.Equals(Unchecked.defaultof<_>) 


[<AutoOpen>]
module Extensions = 

    type String with 
        
        static member IsNullOrWhiteSpace (value:string) =
            if isNull value then true else 
                let rec loop cnt =
                    if cnt = value.Length then true 
                    elif not (Char.IsWhiteSpace value.[cnt]) then false
                    else loop (cnt+1)
                loop 0

    type StringBuilder with
        /// Convenience method for sb.Length <- 0
        member self.Clear() =
            self.Length <- 0
            self

    type System.Lazy<'T> with

        [<CompiledName("Create")>] // give the extension member a 'nice', unmangled compiled name, unique within this module
        static member Create(f : unit -> 'T) : System.Lazy<'T> =
            let creator = new System.Func<'T>(f)
            System.Lazy<'T>(creator, true)

        [<CompiledName("CreateFromValue")>] // give the extension member a 'nice', unmangled compiled name, unique within this module
        static member CreateFromValue(value : 'T) : System.Lazy<'T> =
            System.Lazy<'T>.Create(fun () -> value)

        [<CompiledName("IsDelayedDeprecated")>] // give the extension member a 'nice', unmangled compiled name, unique within this module
        member x.IsDelayed = not(x.IsValueCreated)

        [<CompiledName("IsForcedDeprecated")>] // give the extension member a 'nice', unmangled compiled name, unique within this module
        member x.IsForced = x.IsValueCreated

        [<CompiledName("Force")>] // give the extension member a 'nice', unmangled compiled name, unique within this module
        member x.Force() :'T= x.Value

        [<CompiledName("SynchronizedForceDeprecated")>] // give the extension member a 'nice', unmangled compiled name, unique within this module
        member x.SynchronizedForce() = x.Value

        [<CompiledName("UnsynchronizedForceDeprecated")>] // give the extension member a 'nice', unmangled compiled name, unique within this module
        member x.UnsynchronizedForce() = x.Value


    // Questionable Implementation based on http://referencesource.microsoft.com/#mscorlib/system/enum.cs,ac39bb09f05b3300
    type System.Enum with 
        
        member self.HasFlag (flag:Enum) =
            if isNull flag then 
                raise <| ArgumentNullException(sprintf "Flag is null - %A" flag)
            elif not  <| (self.GetType() = flag.GetType()) then
                raise <| ArgumentException(sprintf "Enum type does not match. Flag is %A , expected a %A " (flag.GetType()) (self.GetType()))
            else
                true



