namespace Increment

open Graph 
open Lens

type Inc = 
    class end
    with
        static member Const(value:'a) = new ConstNode<'a>(value) :> INode<'a>
        static member Var(initialValue: 'a) = new MutableNode<'a>(initialValue) :> IMutableNode<'a>
        static member Map(n1: INode<'a>, f: 'a -> 'b) = new CalcNode<'a,'b>(n1, f) :> INode<'b>
        static member Map(n1: INode<'a>, n2: INode<'b>, f: 'a -> 'b -> 'c) = new CalcNode<'a,'b,'c>(n1, n2, f) :> INode<'c>
        static member Map(n1: INode<'a>, n2: INode<'b>, n3: INode<'c>,  f: 'a -> 'b -> 'c -> 'd) = new CalcNode<'a,'b,'c,'d>(n1, n2, n3, f) :> INode<'d>
        static member Bind(n: INode<INode<'a>>) = new BindNode<'a>(n) :> INode<'a>
        //static member Split( n: INode< 't array>) : INode<'t> array
        static member Join(n: INode<'a> array) = new ArrayNode<'a>(n) :> INode<'a array>
        static member Zoom(mn: IMutableNode<'a>, lens:Lens<'a,'b>) = new ZoomNode<'a,'b>(mn, lens) :> IMutableNode<'b>

module Components = 
    type EditorComponentF<'state,'view> =  IMutableNode<'state> -> INode<'view>
    type ViewComponentF<'state,'view> = INode<'state> -> INode<'view>

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
            editors |> Array.map snd |> Inc.Join
           
        let editorsViews = Inc.Map(span, fun (start, stop) -> 
                                     [|start..stop|] 
                                     |> Array.map (indexLens >> unsafeFromOption)
                                     |> editorsPool)
        Inc.Bind(editorsViews)