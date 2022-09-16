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

[<AutoOpen>]
module Lens = 
    type Lens<'s,'r> = ( 's -> 'r)*('r -> 's -> 's)

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
