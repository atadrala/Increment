namespace Increment 

open Graph
open Lens

module Routing = 
    type RouteParser<'T> = string -> ('T*string) option

    type Route<'State> = Lens<'State, string option>

    type Router<'State when 'State: equality> ( state: IMutableNode<'State>, routes: Route<'State> seq) = 
        let update (route: string) (state: 'State) =
           routes 
           |> Seq.map (fun (_, update) -> update (Some route) state)
           |> Seq.tryFind ( (<>) state)
           |> Option.defaultValue state

        let lens: Lens<'State, string> = ((fun (x:'State) -> ""), update)

        let route = new ZoomNode<'State, string>(state, lens) :> IMutableNode<_>

        member this.Route : IMutableNode<string> = route

    let flip f a b = f b a

    let runParser (parser:RouteParser<'T>) (input:string) : 'T option = 
        match parser input with
        | Some (result, "") -> Some result
        | _ -> None

    let route (parser:RouteParser<'T>) ((prj, update):Lens<'State,'T option>) : Route<'State> = 
        (fun _ -> None), (fun newValue state ->  newValue |> Option.bind (runParser parser) |> (flip update state))

    let pConst (pattern:string) = 
        fun  (input:string) ->
            if input.StartsWith(pattern) 
              then Some (pattern, input.Substring(pattern.Length))
              else None

    let pInt : RouteParser<int> =
        fun (input:string) ->
            let digits = input |> Seq.takeWhile (fun x -> '0' <= x && x <= '9') |> Seq.toArray
            match digits with
            | [||] -> None
            | _ -> Some (int <| new string(digits), input.Substring(digits.Length))

    let pStr :RouteParser<string> = 
        fun (input:string) ->
            let chars = input |> Seq.takeWhile ( (<>) '/') |> Seq.toArray
            match chars with
            | [||] -> None
            | _ -> Some (new string(chars), input.Substring(chars.Length))

    let (<|>)(a:RouteParser<'T1>) (f: 'T1 -> 'T2) :RouteParser<'T2> = 
        a >> Option.map( function (v,input) ->  (f v, input))

    let (>>>) (a:RouteParser<'T1>) (b:RouteParser<'T2>) : RouteParser<'T1*'T2>=
        fun (input: string) -> 
            match a input with
            | Some (leftResult, input) -> 
                 b input
                 |> Option.map( fun (rightResult, input) -> ((leftResult, rightResult), input))
            | None -> None
        
    let (.>>>) a b = a >>> b <|> fst
    let (>>>.) a b = a >>> b <|> snd

    let inline (</>) (l:RouteParser<'T1>) (r:RouteParser<'T2>) = l .>>> pConst "/" >>> r

    type U =
        static member Unwind(((a,b),c): (('a*'b)*'c)) = (a,b,c)
        static member Unwind((((a,b),c),d)) = (a,b,c,d) 
        static member Unwind(((((a,b),c),d),e)) = (a,b,c,d,e)
        static member Unwind((((((a,b),c),d),e),f)) = (a,b,c,d,e,f) 
        static member Unwind(((((((a,b),c),d),e),f),g)) = (a,b,c,d,e,f,g)
        static member Unwind((((((((a,b),c),d),e),f),g),h)) = (a,b,c,d,e,f,g,h) 