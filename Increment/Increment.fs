namespace Increment

open Graph 

module Inc = 

    type EditorComponentF<'TState,'TView> =  IMutableNode<'TState> -> INode<'TView>

    type ViewComponentF<'TState,'TView> = INode<'TState> -> INode<'TView>


module Virtualization =

    open Inc
    open Lens

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
                    |> Seq.map (fun (lens, (proxy, editor)) -> proxy.SetNode (new ZoomNode<_,_>(data, lens));  (proxy,editor))
                
            let newEditors = 
                seq {
                  for l in lens |> Seq.skip editors.Length do
                    let proxy = new ProxyMutableNode<_>(new ZoomNode<_,_>(data, l))
                    yield proxy, (editor proxy)
                }

            editors <- [| yield! reusedEditors; yield! newEditors |]
            editors |> Array.map snd
           
        let editorsViews : INode<INode<_>>
             = new CalcNode<_,_>(span, fun (start, stop) -> 
                                     [|start..stop|] 
                                     |> Array.map (indexLens >> unsafeFromOption)
                                     |> editorsPool
                                     |> ArrayNode<_>
                                     :> INode<_>) :> INode<_>

        new BindNode<_>(editorsViews) :> INode<_>