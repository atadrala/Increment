namespace Increment 

open Graph
open Inc
open Lens

module Combinatorics = 
    type ComposableView<'V> = | V of 'V list
        with
        member this.GetViews = 
                match this with
                | V v -> v

        static member Compose view1 view2 = 
               match view1, view2 with
               | V v1, V v2 -> V [ yield! v1; yield! v2]

    let (>>>>)  (editorLeft :EditorComponentF<'s1,ComposableView<'view>>) (editorRight :EditorComponentF<'s2, ComposableView<'view>>) :EditorComponentF<'s1*'s2, ComposableView<'view>> = 
        fun (state: IMutableNode<'s1*'s2>) -> 
            new CalcNode<_,_,_>( zoom(state, fstLens) |> editorLeft , zoom(state, sndLens) |> editorRight, ComposableView<'view>.Compose) :> INode<_>

    let (|^) (editor: EditorComponentF<'s,'view>) (lens: Lens<'t,'s>) :  EditorComponentF<'t,'view> = 
        fun state -> (editor (zoom(state, lens)))

    let ( |>> ) (editor: EditorComponentF<'s,ComposableView<'view>>)  (f: ComposableView<'view> -> ComposableView<'view>) : EditorComponentF<'s,ComposableView<'view>>= 
        editor >> (fun view -> new CalcNode<_,_>(view, f) :> INode<_>)