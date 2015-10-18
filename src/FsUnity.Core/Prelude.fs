namespace FsUnity

open System
open System.Text

[<AutoOpen>]
module Prelude =


    /// Special isNull operator that works on Unity3d fake 'nulls'
    let inline isNull x = 
        let y = box x // In case of value types
        obj.ReferenceEquals (y, Unchecked.defaultof<_>) || y.Equals(Unchecked.defaultof<_>) 


    module TypeMaps =

        type Currier = Currier with
            static member inline (?<-) (_,_,fn) = fun a b                         -> fn (a,b)
            static member inline (?<-) (_,_,fn) = fun a b c                       -> fn (a,b,c)
            static member inline (?<-) (_,_,fn) = fun a b c d                     -> fn (a,b,c,d)
            static member inline (?<-) (_,_,fn) = fun a b c d e                   -> fn (a,b,c,d,e)
            static member inline (?<-) (_,_,fn) = fun a b c d e f                 -> fn (a,b,c,d,e,f)
            static member inline (?<-) (_,_,fn) = fun a b c d e f g               -> fn (a,b,c,d,e,f,g)
            static member inline (?<-) (_,_,fn) = fun a b c d e f g h             -> fn (a,b,c,d,e,f,g,h)
            static member inline (?<-) (_,_,fn) = fun a b c d e f g h i           -> fn (a,b,c,d,e,f,g,h,i)
            static member inline (?<-) (_,_,fn) = fun a b c d e f g h i j         -> fn (a,b,c,d,e,f,g,h,i,j)
            static member inline (?<-) (_,_,fn) = fun a b c d e f g h i j k       -> fn (a,b,c,d,e,f,g,h,i,j,k)
            static member inline (?<-) (_,_,fn) = fun a b c d e f g h i j k l     -> fn (a,b,c,d,e,f,g,h,i,j,k,l)
            static member inline (?<-) (_,_,fn) = fun a b c d e f g h i j k l m   -> fn (a,b,c,d,e,f,g,h,i,j,k,l,m)
            static member inline (?<-) (_,_,fn) = fun a b c d e f g h i j k l m n -> fn (a,b,c,d,e,f,g,h,i,j,k,l,m,n)

        type RevCurrier = RevCurrier with
            static member inline (?<-) (_,_,fn) = fun a b                         -> fn (b,a                        )
            static member inline (?<-) (_,_,fn) = fun a b c                       -> fn (c,b,a                      )
            static member inline (?<-) (_,_,fn) = fun a b c d                     -> fn (d,c,b,a                    )
            static member inline (?<-) (_,_,fn) = fun a b c d e                   -> fn (e,d,c,b,a                  )
            static member inline (?<-) (_,_,fn) = fun a b c d e f                 -> fn (f,e,d,c,b,a                )
            static member inline (?<-) (_,_,fn) = fun a b c d e f g               -> fn (g,f,e,d,c,b,a              )
            static member inline (?<-) (_,_,fn) = fun a b c d e f g h             -> fn (h,g,f,e,d,c,b,a            )
            static member inline (?<-) (_,_,fn) = fun a b c d e f g h i           -> fn (i,h,g,f,e,d,c,b,a          )
            static member inline (?<-) (_,_,fn) = fun a b c d e f g h i j         -> fn (j,i,h,g,f,e,d,c,b,a        )
            static member inline (?<-) (_,_,fn) = fun a b c d e f g h i j k       -> fn (k,j,i,h,g,f,e,d,c,b,a      )
            static member inline (?<-) (_,_,fn) = fun a b c d e f g h i j k l     -> fn (l,k,j,i,h,g,f,e,d,c,b,a    )
            static member inline (?<-) (_,_,fn) = fun a b c d e f g h i j k l m   -> fn (m,l,k,j,i,h,g,f,e,d,c,b,a  )
            static member inline (?<-) (_,_,fn) = fun a b c d e f g h i j k l m n -> fn (n,m,l,k,j,i,h,g,f,e,d,c,b,a)


    open TypeMaps

    /// Curry Prefix operator, take any function that takes a tupled arg of size 2-14 and convert it into
    /// a function that takes the same number of args in the same order in curried form
    let inline ( !| ) fn  = (?<-) Currier () fn

    /// ReverseCurry Prefix operator, take any function that takes a tupled arg of size 2-14 and convert it into
    /// a function that takes the same number of args in reverse order in curried form
    let inline ( !|~) fn  = (?<-) RevCurrier () fn

    /// Lambda Curry, take any function that takes a tupled arg of size 2-14 and convert it into
    /// a function that takes the same number of args in the same order in curried form
    let inline    λ   fn  = (?<-) Currier () fn

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



