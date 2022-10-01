namespace Increment 

open Graph
open Components
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
            Inc.Calc( Inc.Zoom(state, fstLens) |> editorLeft , Inc.Zoom(state, sndLens) |> editorRight, ComposableView<'view>.Compose)

    let (|^) (editor: EditorComponentF<'s,'view>) (lens: Lens<'t,'s>) :  EditorComponentF<'t,'view> = 
        fun state -> (editor (Inc.Zoom(state, lens)))

    let (^|) (lens: Lens<'t,'s>) (editor: EditorComponentF<'s,'view>) = editor |^ lens 

    let ( |>> ) (editor: EditorComponentF<'s,'view>)  (f: 'view -> 'viewOut) : EditorComponentF<'s,'viewOut>= 
        editor >> (fun view -> Inc.Calc(view, f))
