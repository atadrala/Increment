module App

open Feliz
open Browser.Dom
open Fable.React
open Lib

// Mutable variable to count the number of times we clicked the button

let myAppElement = document.querySelector(".my-app") :?> Browser.Types.HTMLElement

type State = string*string

type Routes<'TState> = List<string * Lens<'TState, string option>>

(*      
    route: lens state <-> string option
    path -> string option -> 
 *)

let routes : Routes<State>  = 
        [   
            "/string/:string", (snd >> Option.Some, fun newRoute (s,route) -> (s, newRoute |> Option.defaultValue route))      
        ]

type Router<'TState>(state: IMutableNode<'TState>, routes: Routes<'TState>, initalRoute: string)  =
    let x = 
        routes |> List.iter (fun (path, lens) ->  
                                let path = zoom(state, lens)
                                path.Changed.Add( fun _ -> 
                                        async {
                                            let! route = path.Evaluate()
                                            match route with 
                                            | Some route -> window.location.pathname <- route 
                                            | None -> ()
                                        } |> Async.Start
                                ))

type MyApp(state: IMutableNode<string*string>) = 
    inherit EditorComponent<string*string>(state) 

    let reverse x = new string( x |> Array.ofSeq |> Array.rev)

    let lens: Lens<string*string,string> = fst >> reverse, (fun x (str, route) -> (reverse x, route))
    let lensFst : Lens<string*string, string> = fst, (fun x  (_,route) -> x, route)

    let strEditor = StringEditor(zoom(state, lensFst), "String:", function | "Artur" -> Some "Cannot be Artur" | _ -> None )
    let revEditor = StringEditor(zoom(state, lens), "Reversed:")

    let scrollPosition = new MutableNode<float>(400.) :> IMutableNode<_>

    let bufferSize = 4
    let elementHeight = 20
    let viewHeight = 100

    let viewSpan = new CalcNode<_,_>(scrollPosition :> INode<_>, fun scroll -> 
                            let start = max  0 (int scroll / elementHeight - bufferSize)
                            let end_ = start + (viewHeight / elementHeight) + 2 * bufferSize
                            scroll, start, end_
                        )

    let data = [1..100] |> List.map (fun x -> "Artur " + string x)
    let count = 100

    override _.View = new CalcNode<_,_>( //strEditor.View, revEditor.View,  
                        //fun strEdit revEdit -> 
                            // Html.div [
                            //     strEdit;
                            //     revEdit;
                            // ]
                            viewSpan, fun (scroll, start, stop) -> 

                            Html.div [
                                prop.ref <| fun (el:Browser.Types.Element) -> 
                                                    if el <> null then (el :?> Browser.Types.HTMLElement).scrollTop <- scroll
                                prop.style [
                                    Feliz.style.backgroundColor "green";
                                    Feliz.style.height 100;
                                    Feliz.style.overflow.auto;
                                  ];
                                prop.custom ("scrollTop", scroll)
                                prop.onScroll ( fun se -> scrollPosition.SetValue((se.target :?> Browser.Types.HTMLElement).scrollTop));
                                prop.children  [
                                    yield Html.div [ 
                                        prop.style [ Feliz.style.height (start*elementHeight) ]
                                        ]
                                    for x in start..stop do
                                      yield Html.div [
                                            prop.text (data.[x])
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

    new() = MyApp(new MutableNode<string*string>( window.location.pathname, window.location.pathname) :> IMutableNode<_>)


let app =  new MyApp()

async { 
        let! v = app.View.Evaluate()
        console.log("react rendered")
        ReactDom.render(v, myAppElement) } |> Async.Start
      
app.View.Changed.Add( fun v -> async { 
        let! v = app.View.Evaluate()
        console.log("react rendered")
        ReactDom.render(v, myAppElement)} |> Async.Start
        )



