namespace Increment

open Graph
open Browser
open Combinatorics

[<AutoOpen>]
module Web = 
    open Feliz
    open Components
    open Lens

    type View = ReactElement

    type Editor<'t when 't: equality>(state: IMutableNode<'t>, lens:Lens<'t, string>, ?label: string, ?validate:'t -> string option) = 
        let (prj,inj) = lens
        let validationResults =Inc.Var("")
        let mutable input : string option = None
        do console.log ("editor created")
                     
        member  _.View = 
            Inc.Map(state, validationResults, fun s validation ->
                let mutable newValue = input|> Option.map(fun i -> inj i s) |> Option.defaultValue s;
                console.log (string input)
                Html.div [
                    if label.IsSome then 
                        yield Html.label [prop.text ""]
                    yield Html.input [
                        prop.type' "text";
                        prop.valueOrDefault (input |> Option.defaultValue (prj s));
                        prop.style <| if validation = ""
                            then [] 
                            else [
                                    Feliz.style.border (2,borderStyle.solid, "red");
                                    Feliz.style.backgroundColor "red";
                                ]
                        prop.onTextChange (fun (value : string) ->
                                                    input <- Some value;
                                                    newValue <- inj value newValue;);
                        prop.onBlur (fun e ->
                                     match Option.bind ((|>) newValue) validate with 
                                     | None -> 
                                            input <- None
                                            async {
                                                do! validationResults.SetValue("")
                                                do! state.SetValue(newValue)
                                            } |> Async.Start
                                     | Some error ->
                                            validationResults.SetValue(error) |> Async.Start );
                    ]
                ])

        static member String(state) = (new Editor<string>(state, (id, fun nv _ -> nv))).View

        static member String'(state) = Inc.Map(Editor<_>.String(state), ComposableView<_>.singleton)

        static member Int(state: IMutableNode<int>) = (new Editor<int>(state, (string, fun nv _ -> System.Int32.Parse nv))).View

        static member Int'(state: IMutableNode<int>) = Inc.Map(Editor<_>.Int(state), ComposableView<_>.singleton)

        static member Double(state: IMutableNode<double>) = (new Editor<double>(state, (string, fun nv _ -> System.Double.Parse nv))).View 
      
        static member Double'(state: IMutableNode<double>) = Inc.Map(Editor<_>.Double(state), ComposableView<_>.singleton)

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

        let scrollPosition = Inc.Var(0.)

        let bufferSize = 4
        let elementHeight = 20
        let viewHeight = 100
        let count = 100

        let viewSpan = Inc.Map(scrollPosition :> INode<_>, fun scroll -> 
                                let start = max  0 (int scroll / elementHeight - bufferSize)
                                let end_ = start + (viewHeight / elementHeight) + 2 * bufferSize
                                scroll, start, end_)

        let span = Inc.Map(viewSpan, fun (_,start,stop) -> (start, stop))

        let editors = Virtualization.virtualize(state, editor, span)

        member _.View = Inc.Map(
                            viewSpan, editors, fun (scroll, start, stop) editors -> 
                            Html.div [
                                prop.ref <| fun (el:Browser.Types.Element) -> 
                                                    if el <> null then (el :?> Browser.Types.HTMLElement).scrollTop <- scroll
                                prop.style [
                                    Feliz.style.backgroundColor "green";
                                    Feliz.style.height 100;
                                    Feliz.style.overflow.auto;
                                  ];
                                prop.onScroll ( fun se -> scrollPosition.SetValue((se.target :?> Browser.Types.HTMLElement).scrollTop) |> Async.Start);
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
                            ])

        static member f(editor: EditorComponentF<'elem, View>) = 
            fun (state: IMutableNode<'elem []>) -> (new VirtualizedGrid<_>(state, editor)).View
   
   
    type CacheNode<'elem>(node: IMutableNode<'elem>) = 
        let mutable v: option<'elem> =  None

        interface IMutableNode<'elem> with
                member this.Evaluate() = async {
                        if v.IsSome 
                            then return v.Value
                            else 
                                let! nodeValue = node.Evaluate()
                                return nodeValue
                    }
                member this.Changed = node.Changed

                member this.SetValue newValue = async { v <- Some newValue }

        member this.Flush() = 
            async { 
                if v.IsSome then 
                    do! node.SetValue v.Value
                    v <- None
            }

          
    type Confirmation<'elem>(state: IMutableNode<'elem>, editor: EditorComponentF<'elem, View>) =
        let editCache = new CacheNode<_>(state)

        member this.View = Inc.Map(editor editCache, fun v -> 
        
                Html.div [
                    prop.children  [
                        v  
                        Html.button [
                            prop.text "Save"
                            prop.onClick (fun x -> editCache.Flush() |> Async.Start)
                            ]
                        ]
                    ])
            
