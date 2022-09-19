module App

open Feliz
open Browser.Dom
open Fable.React
open Lib

// Mutable variable to count the number of times we clicked the button

let myAppElement = document.querySelector(".my-app") :?> Browser.Types.HTMLElement

type MyApp(state) = 
    inherit EditorComponent<string>(state) 

    let reverse x = new string( x |> Array.ofSeq |> Array.rev)
    let lens: Lens<string,string> = reverse, (fun x r -> reverse x)

    let strEditor = StringEditor(state, "String:")
    let revEditor = StringEditor(zoom(state, lens), "Reversed:")

    override _.View = new CalcNode<_,_,_>( strEditor.View, revEditor.View, 
                        fun strEdit revEdit -> 
                            Html.div [
                                strEdit;
                                revEdit;
                            ]) :> INode<_>

    new() = MyApp(new MutableNode<string>("") :> IMutableNode<_>)

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



