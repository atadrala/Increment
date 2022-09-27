module Lib

open Browser

type INode<'T> = 
    abstract Evaluate : unit -> Async<'T>
    abstract Changed: IEvent<unit>

type IMutableNode<'T> =
    inherit INode<'T>
    abstract SetValue : 'T -> unit
    abstract Apply: ('T -> 'T) -> unit

type MutableNode<'T when 'T : equality>(initialValue: 'T) = 
    let value = initialValue |> Lazy.CreateFromValue |> ref
    let changedEvent = new Event<unit>()

    interface INode<'T> with
        member _.Evaluate() = async { return value.Value.Value; }
        member _.Changed = changedEvent.Publish
    
    interface IMutableNode<'T> with
        member this.SetValue (newValue: 'T) = 
            if newValue <> value.Value.Value then 
                value.Value <- Lazy.CreateFromValue newValue
                changedEvent.Trigger()
        member this.Apply(f:'T -> 'T) = 
                // create lazy ref closure instead of ref cell ref closure
                let lazyValueRef = value.Value
                value.Value <- Lazy.Create( fun () -> lazyValueRef.Value |> f)
                changedEvent.Trigger()

            
type CalcNode<'T,'R>(n1: INode<'T>, map: 'T -> 'R) =
    let changedEvent = new Event<unit>()
    let isDirty = ref true
    let prev: Option<'R> ref = ref None
    do 
       n1.Changed.Add( fun _ -> isDirty.Value <- true; changedEvent.Trigger())

    interface INode<'R> with
        member _.Evaluate() = async {
                if isDirty.Value || prev.Value.IsNone then
                    let! v1 = n1.Evaluate()
                    prev.Value <- Some (map v1)
                    isDirty.Value <- false
                return prev.Value.Value
            }
        member _.Changed = changedEvent.Publish

type CalcNode<'T1,'T2,'R>(n1: INode<'T1>, n2: INode<'T2>, map: 'T1 -> 'T2 -> 'R) =
    let changedEvent = new Event<unit>()
    let isDirty = ref true
    let prev: Option<'R> ref = ref None
    do 
        n1.Changed.Add( fun _ -> isDirty.Value <- true; changedEvent.Trigger())
        n2.Changed.Add( fun _ -> isDirty.Value <- true; changedEvent.Trigger())

    interface INode<'R> with
        member _.Evaluate() = async {
                if isDirty.Value || prev.Value.IsNone then
                    let! fn1 =  Async.StartChild(n1.Evaluate())
                    let! fn2 =  Async.StartChild(n2.Evaluate())
                    let! v1 = fn1
                    let! v2 = fn2
                    prev.Value <- Some (map v1 v2)
                    isDirty.Value <- false
                return prev.Value.Value
            }
        member _.Changed = changedEvent.Publish

type BindNode<'R>(node: INode<INode<'R>>) =
    let changed = new Event<unit>()
    let mutable inner: INode<'R> option = None
    let trigger = Handler<unit>( fun _ _ -> changed.Trigger())
    do 
        node.Changed.Add(fun _ -> 
                            inner |> Option.iter ( fun inner -> inner.Changed.RemoveHandler(trigger))
                            inner <- None
                            changed.Trigger())

    interface INode<'R> with
        member this.Evaluate() = async {
                let! innerNode = node.Evaluate()
                inner |> Option.iter ( fun inner -> inner.Changed.RemoveHandler(trigger))
                inner <- Some innerNode
                innerNode.Changed.AddHandler(trigger)
                let! value = innerNode.Evaluate()
                return value
            }
        member _.Changed = changed.Publish


type ArrayNode<'R>(nodes: INode<'R> array) =
    let changed = new Event<unit>()
    do 
        for n in nodes do   
            n.Changed.Add(fun _ -> changed.Trigger())

    interface INode<'R array> with
        member this.Evaluate() = async {
                let! values = Async.Parallel(nodes |> Array.map (fun n -> n.Evaluate()))
                return values
            }
        member _.Changed = changed.Publish

[<AutoOpen>]
module Lens = 
    type Lens<'s,'r> = ( 's -> 'r)*('r -> 's -> 's)

    let compose ((prj1, inj1): Lens<'a,'b>) ((prj2, inj2): Lens<'b,'c>) : Lens<'a,'c> =
        (prj1 >> prj2), (fun r s -> s |> (s |> prj1 |> inj2 r |> inj1))

    type ZoomNode<'S, 'R>(node: IMutableNode<'S>, lens: Lens<'S,'R>) =
        let (prj, inj) = lens
        interface INode<'R> with
            member _.Evaluate() = async {
                let! value = node.Evaluate()
                return prj value
            }
            member _.Changed = node.Changed
        
        interface IMutableNode<'R> with
            member this.SetValue (newValue: 'R) = 
                node.Apply(inj newValue)
            member this.Apply (f: 'R -> 'R) = 
                let outerF x = 
                    inj ( f <| prj x) x 
                node.Apply(outerF)

    let zoom(node:IMutableNode<'S>, lens: Lens<'S,'R>) : IMutableNode<'R> = 
            new ZoomNode<'S,'R>(node, lens) :> IMutableNode<'R>

    let indexLens (idx: int) : Lens<'t array,'t option> = 
       (fun x -> if idx >= 0 && idx <= x.Length then Some (x.[idx]) else None), 
       (fun r x -> 
            match r with 
            | Some r ->  if idx >= 9 && idx <= x.Length then do x.[idx] <- r
            | None -> ()
            x)
    
    let unsafeFromOption ( l: Lens<'s, 't option>) : Lens<'s,'t> =
            compose l
                (Option.defaultWith (fun () -> 
                    failwith "Use of fromOption on None value in unsafeFromOption lense"),
                 fun r _-> Some r)

[<AutoOpen>]
module Web = 
    open Feliz

    type View = ReactElement

    type EditorComponentF<'TState> = IMutableNode<'TState> -> INode<View>

    type ViewComponentF<'TState> = INode<'TState> -> INode<View>

    [<AbstractClass>]
    type EditorComponent<'TState>(state: IMutableNode<'TState>) =    
        abstract View : INode<ReactElement>

    type StringEditor(state: IMutableNode<string>, ?label: string, ?validate:string -> string option) = 
        inherit EditorComponent<string>(state)

        override _.View = 
            new CalcNode<_,_>(state, fun s ->
            let mutable newValue =  s;
            let mutable input : Types.Element option = None
            Html.div [
                if label.IsSome then 
                    yield Html.label [prop.text ""]
                yield Html.input [
                    prop.type' "text";
                    prop.valueOrDefault s;
                    prop.onTextChange (fun (value : string) -> newValue <- value;);
                    prop.onBlur (fun _ -> 
                                 match Option.bind ((|>) newValue) validate with 
                                 | None -> state.SetValue(newValue)
                                 | Some error -> state.SetValue(newValue));
                ]
            ]) :> INode<_>


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

module Virtualization = 
    open Feliz 

    let virtualize (data: IMutableNode<'t array>, editorFactory: IMutableNode<'t> -> EditorComponent<'t>, span: INode<int*int>) =
        let editorsPool (idx, lens) =
            new ZoomNode<_,_>(data, lens) |> editorFactory
        
        let indexes : CalcNode<_,(int*Lens<_,_>) array>
             = new CalcNode<_,_>(span, fun (start, stop) -> 
                                    [|start..stop|] 
                                    |> Array.map (fun idx ->
                                            (idx - start, unsafeFromOption (indexLens idx))))

        let editors: INode<EditorComponent<'t>[]> = 
            new CalcNode<_,_>(indexes, Array.map editorsPool) :> INode<_>

        let view = CalcNode<_,_>(editors, fun es -> 
            let viewNodes = new ArrayNode<_>(es |> Array.map (fun e -> e.View))
            viewNodes :> INode<_> ) :> INode<_>

        new BindNode<_>(view) :> INode<_>

        

                  


        