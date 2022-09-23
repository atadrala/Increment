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

    override _.View = new CalcNode<_,_,_>( strEditor.View, revEditor.View, 
                        fun strEdit revEdit -> 
                            Html.div [
                                strEdit;
                                revEdit;
                            ]) :> INode<_>

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



