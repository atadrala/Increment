namespace Increment

module Graph = 

    type INode<'value> = 
        abstract Evaluate : unit -> Async<'value>
        abstract Changed: IEvent<unit>

    type IMutableNode<'value> =
        inherit INode<'value>
        abstract SetValue : 'value -> Async<unit>

    type ConstNode<'value>(value: 'value) =
        let changedEvent = new Event<unit>()     
        interface INode<'value> with
            member _.Evaluate() = async { return value }
            member _.Changed = changedEvent.Publish

    type MutableNode<'value when 'value : equality>(initialValue: 'value) = 
        let value = initialValue |> ref
        let changedEvent = new Event<unit>()

        interface INode<'value> with
            member _.Evaluate() = async { return value.Value; }
            member _.Changed = changedEvent.Publish
    
        interface IMutableNode<'value> with
            member this.SetValue (newValue: 'value) = 
                if newValue <> value.Value then 
                    value.Value <- newValue
                    changedEvent.Trigger()
                async { return () }
            
    type CalcNode<'input, 'value when 'input : equality>(n1: INode<'input>, map: 'input -> 'value) =
        let changedEvent = new Event<unit>()
        let isDirty = ref true
        let prevValue: Option<'value> ref = ref None
        let prevDep: Option<'input> ref = ref None
        do 
           n1.Changed.Add( fun _ -> isDirty.Value <- true; changedEvent.Trigger())

        interface INode<'value> with
            member _.Evaluate() = async {
                    if isDirty.Value || prevValue.Value.IsNone then
                        let! v1 = n1.Evaluate()
                        do if prevDep.Value.IsSome && prevDep.Value.Value <> v1 || prevDep.Value.IsNone
                           then 
                                prevValue.Value <- Some (map v1)
                                prevDep.Value <- Some v1    
                        isDirty.Value <- false
                    return prevValue.Value.Value
                }
            member _.Changed = changedEvent.Publish

    type CalcNode<'input1,'input2,'value when 'input1 : equality and 'input2 : equality>(n1: INode<'input1>, n2: INode<'input2>, map: 'input1 -> 'input2 -> 'value) =
        let changedEvent = new Event<unit>()
        let isDirty = ref true
        let prevValue: Option<'value> ref = ref None
        let prevDep1: Option<'input1> ref = ref None
        let prevDep2: Option<'input2> ref = ref None

        do 
            n1.Changed.Add( fun _ -> isDirty.Value <- true; changedEvent.Trigger())
            n2.Changed.Add( fun _ -> isDirty.Value <- true; changedEvent.Trigger())

        interface INode<'value> with
            member _.Evaluate() = async {
                    if isDirty.Value || prevValue.Value.IsNone then
                        let! fn1 =  Async.StartChild(n1.Evaluate())
                        let! fn2 =  Async.StartChild(n2.Evaluate())
                        let! v1 = fn1
                        let! v2 = fn2
                        do if (prevDep1.Value.IsSome && prevDep1.Value.Value <> v1) || prevDep1.Value.IsNone
                           || (prevDep2.Value.IsSome && prevDep2.Value.Value <> v2) || prevDep2.Value.IsNone
                           then 
                                prevDep1.Value <- Some v1
                                prevDep2.Value <- Some v2
                                prevValue.Value <- Some (map v1 v2)
                        isDirty.Value <- false
                    return prevValue.Value.Value
                }
            member _.Changed = changedEvent.Publish

    type CalcNode<'input1,'input2,'input3,'value when 'input1 : equality and 'input2 : equality  and 'input3 : equality>
                (n1: INode<'input1>, n2: INode<'input2>, n3: INode<'input3>, map: 'input1 -> 'input2 -> 'input3 -> 'value) =
        let changedEvent = new Event<unit>()
        let isDirty = ref true
        let prevValue: Option<'value> ref = ref None
        let prevDep1: Option<'input1> ref = ref None
        let prevDep2: Option<'input2> ref = ref None
        let prevDep3: Option<'input3> ref = ref None
        do 
            n1.Changed.Add( fun _ -> isDirty.Value <- true; changedEvent.Trigger())
            n2.Changed.Add( fun _ -> isDirty.Value <- true; changedEvent.Trigger())
            n3.Changed.Add( fun _ -> isDirty.Value <- true; changedEvent.Trigger())

        interface INode<'value> with
            member _.Evaluate() = async {
                    if isDirty.Value || prevValue.Value.IsNone then
                        let! fn1 =  Async.StartChild(n1.Evaluate())
                        let! fn2 =  Async.StartChild(n2.Evaluate())
                        let! fn3 =  Async.StartChild(n3.Evaluate())

                        let! v1 = fn1
                        let! v2 = fn2
                        let! v3 = fn3
                        do if (prevDep1.Value.IsSome && prevDep1.Value.Value <> v1) || prevDep1.Value.IsNone
                           || (prevDep2.Value.IsSome && prevDep2.Value.Value <> v2) || prevDep2.Value.IsNone
                           || (prevDep3.Value.IsSome && prevDep3.Value.Value <> v3) || prevDep3.Value.IsNone
                           then 
                                prevDep1.Value <- Some v1
                                prevDep2.Value <- Some v2
                                prevDep3.Value <- Some v3
                                prevValue.Value <- Some (map v1 v2 v3)
                        isDirty.Value <- false
                    return prevValue.Value.Value
                }
            member _.Changed = changedEvent.Publish

    type BindNode<'value>(node: INode<INode<'value>>) =
        let changed = new Event<unit>()
        let mutable inner: INode<'value> option = None
        let trigger = Handler<unit>( fun _ _ -> changed.Trigger())
        do 
            node.Changed.Add(fun _ -> 
                                inner |> Option.iter ( fun inner -> inner.Changed.RemoveHandler(trigger))
                                inner <- None
                                changed.Trigger())

        interface INode<'value> with
            member this.Evaluate() = async {
                    let! innerNode = node.Evaluate()
                    inner |> Option.iter ( fun inner -> inner.Changed.RemoveHandler(trigger))
                    inner <- Some innerNode
                    innerNode.Changed.AddHandler(trigger)
                    let! value = innerNode.Evaluate()
                    return value
                }
            member _.Changed = changed.Publish

    type ArrayNode<'value>(nodes: INode<'value> array) =
        let changed = new Event<unit>()
        do 
            for n in nodes do   
                n.Changed.Add(fun _ -> changed.Trigger())

        interface INode<'value array> with
            member this.Evaluate() = async {
                    let! values = Async.Parallel(nodes |> Array.map (fun n -> n.Evaluate()))
                    return values
                }
            member _.Changed = changed.Publish


