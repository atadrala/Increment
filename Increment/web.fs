namespace Increment

open Graph
open Browser
open Combinatorics

[<AutoOpen>]
module Web = 
    open Feliz
    open Inc
    open Lens

    type View = ReactElement

    type StringEditor<'t>(state: IMutableNode<'t>, lens:Lens<'t, string>, ?label: string, ?validate:'t -> string option) = 
        let (prj,inj) = lens
        member  _.View = 
            new CalcNode<_,_>(state, fun s ->
            let mutable newValue =  s;
            let mutable input : Types.Element option = None
            Html.div [
                if label.IsSome then 
                    yield Html.label [prop.text ""]
                yield Html.input [
                    prop.type' "text";
                    prop.valueOrDefault (prj s);
                    prop.onTextChange (fun (value : string) -> newValue <- inj value newValue;);
                    prop.onBlur (fun _ -> 
                                 match Option.bind ((|>) newValue) validate with 
                                 | None -> state.SetValue(newValue)
                                 | Some error -> state.SetValue(newValue));
                ]
            ]) :> INode<_>

        static member f(state) = (new StringEditor<string>(state, (id, fun nv _ -> nv))).View

        static member f'(state) = new CalcNode<_,_>((new StringEditor<string>(state,(id, fun nv _ -> nv))).View, List.singleton >> V) :> INode<_>

        static member f(state: IMutableNode<int>) = (new StringEditor<int>(state, (string, fun nv _ -> System.Int32.Parse nv))).View

        static member f'(state: IMutableNode<int>) = new CalcNode<_,_>((new StringEditor<_>(state,(string, fun nv _ -> System.Int32.Parse nv))).View, List.singleton >> V) :> INode<_>

      
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


    type VirtualizedGrid<'elem>(state: IMutableNode<'elem[]>, editor: EditorComponentF<'elem, View>) = 

        let scrollPosition = new MutableNode<float>(0.) :> IMutableNode<_>

        let bufferSize = 4
        let elementHeight = 20
        let viewHeight = 100
        let count = 100

        let viewSpan = new CalcNode<_,_>(scrollPosition :> INode<_>, fun scroll -> 
                                let start = max  0 (int scroll / elementHeight - bufferSize)
                                let end_ = start + (viewHeight / elementHeight) + 2 * bufferSize
                                scroll, start, end_
                            )

        let span = new CalcNode<_,_>(viewSpan, fun (_,start,stop) -> (start, stop))

        let editors = Virtualization.virtualize(state, editor, span)

        member _.View = new CalcNode<_,_,_>(
                            viewSpan, editors, fun (scroll, start, stop) editors -> 
                            Html.div [
                                prop.ref <| fun (el:Browser.Types.Element) -> 
                                                    if el <> null then (el :?> Browser.Types.HTMLElement).scrollTop <- scroll
                                prop.style [
                                    Feliz.style.backgroundColor "green";
                                    Feliz.style.height 100;
                                    Feliz.style.overflow.auto;
                                  ];
                                prop.onScroll ( fun se -> scrollPosition.SetValue((se.target :?> Browser.Types.HTMLElement).scrollTop));
                                prop.children  [
                                    yield Html.div [ 
                                        prop.style [ Feliz.style.height (start*elementHeight) ]
                                        ]
                                    for x in editors do
                                      yield Html.div [
                                                prop.children x;
                                                prop.style [ 
                                                    Feliz.style.height elementHeight;
                                            ]
                                    ]

                                    yield Html.div [ 
                                        prop.style [ Feliz.style.height ((count - stop)*elementHeight) ]
                                    ]
                                 ]  
                            ]
                            
                            ) :> INode<_>

        static member f(editor: EditorComponentF<'elem, View>) = 
            fun (state: IMutableNode<'elem []>) -> (new VirtualizedGrid<_>(state, editor)).View
        

                  