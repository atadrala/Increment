namespace Increment

open Graph 
open Lens

type Inc = 
    class end
    with
        static member Const(value:'a) = new ConstNode<_>(value) :> INode<_>
        static member Input(initialValue: 'a) = new MutableNode<'a>(initialValue) :> IMutableNode<'a>
        static member Calc(n1: INode<'T>, f: 'T -> 'R) = new CalcNode<'T,'R>(n1, f) :> INode<_>
        static member Calc(n1: INode<'T1>, n2: INode<'T2>, f: 'T1 -> 'T2 -> 'R) = new CalcNode<'T1,'T2,'R>(n1,n2, f) :> INode<_>
        static member Calc(n1: INode<'T1>, n2: INode<'T2>, n3: INode<'T3>,  f: 'T1 -> 'T2 -> 'T3 -> 'R) = new CalcNode<'T1,'T2,'T3,'R>(n1, n2, n3, f) :> INode<_>
        static member Bind(n: INode<INode<_>>) = new BindNode<_>(n) :> INode<_>
        static member Array(n: INode<_> array) = new ArrayNode<_>(n) :> INode<_>
        static member Zoom(mn: IMutableNode<_>, lens:Lens<_,_>) = new ZoomNode<_,_>(mn, lens) :> IMutableNode<_>

module Components = 

    type EditorComponentF<'TState,'TView> =  IMutableNode<'TState> -> INode<'TView>

    type ViewComponentF<'TState,'TView> = INode<'TState> -> INode<'TView>


module Virtualization =
    open Components

    // Replace with bind on MutableNode if possible.
    type ProxyMutableNode<'t> (node : IMutableNode<'t>) =
        let mutable node = node
        let changedEvent = new Event<unit>()
        let triggerChange = Handler<unit>(fun _ _ -> changedEvent.Trigger())
        do node.Changed.AddHandler triggerChange
   
        interface IMutableNode<'t> with
            member this.Evaluate() = node.Evaluate()
            member this.Changed = changedEvent.Publish
            member this.SetValue v = node.SetValue v
            member this.Apply f = node.Apply f

        member this.SetNode (newNode: IMutableNode<'t>) = 
            node.Changed.RemoveHandler triggerChange
            node <- newNode
            node.Changed.AddHandler triggerChange
            changedEvent.Trigger()


    let virtualize (data: IMutableNode<'t array>, editor: EditorComponentF<'t, 'View>, span: INode<int*int>) =
        let mutable editors : (ProxyMutableNode<'t>*INode<'View>) [] = [||]

        let editorsPool (lens: Lens<_,_> []) =
            let reusedEditors = 
                    Seq.zip lens editors
                    |> Seq.map (fun (lens, (proxy, editor)) -> proxy.SetNode (Inc.Zoom(data, lens));  (proxy,editor))
                
            let newEditors = 
                seq {
                  for l in lens |> Seq.skip editors.Length do
                    let proxy = new ProxyMutableNode<_>(Inc.Zoom(data, l))
                    yield proxy, (editor proxy)
                }

            editors <- [| yield! reusedEditors; yield! newEditors |]
            editors |> Array.map snd |> Inc.Array
           
        let editorsViews = Inc.Calc(span, fun (start, stop) -> 
                                     [|start..stop|] 
                                     |> Array.map (indexLens >> unsafeFromOption)
                                     |> editorsPool)

        Inc.Bind(editorsViews)