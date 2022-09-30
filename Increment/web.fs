namespace Increment

open Graph
open Browser
open Combinatorics

[<AutoOpen>]
module Web = 
    open Feliz
    open Inc

    type View = ReactElement

    type StringEditor(state: IMutableNode<string>, ?label: string, ?validate:string -> string option) = 
        member  _.View = 
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

        static member f: EditorComponentF<string,View> = fun state -> (new StringEditor(state)).View

        static member f': EditorComponentF<string,ComposableView<View>> = fun state -> new CalcNode<_,_>((new StringEditor(state)).View, List.singleton >> V) :> INode<_>

      
    let wrapWithFlexDiv (v:ComposableView<View>) = 
        V <| [ Html.div [
                prop.style [
                    Feliz.style.display.flex;
                ]
                prop.children v.GetViews
             ]]
             
    let show (view:ComposableView<View>) = 
        match view with 
        | V [view] -> view
        | V views -> Html.div views
        


        

                  