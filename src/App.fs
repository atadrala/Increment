module App

open Browser.Dom
open Lib

// Mutable variable to count the number of times we clicked the button
let count  = new MutableNode<_>(0) :> IMutableNode<_>

let square = new CalcNode<_,_>(count, (fun x -> x*x)) :> INode<_>

let square2 = new CalcNode<_,_>(count, (fun x -> x*x)) :> INode<_>

let result = new CalcNode<_,_,_>(square, square2, (+))

let str = new MutableNode<string>("") :> IMutableNode<_>

let reverse x = new string( x |> Array.ofSeq |> Array.rev)
let lens: Lens<string,string> =
    reverse, (fun x r -> reverse x)

let rev = zoom(str, lens)

// Get a reference to our button and cast the Element to an HTMLButtonElement
let myButton = document.querySelector(".my-button") :?> Browser.Types.HTMLButtonElement
let myText = document.querySelector(".my-text") :?> Browser.Types.HTMLInputElement
let myRevText = document.querySelector(".my-rev-text") :?> Browser.Types.HTMLInputElement


// Register our listener
myButton.onclick <- fun _ -> (async {
            let! oldValue = (rev :> INode<_>).Evaluate()
            rev.SetValue( reverse oldValue )
            myButton.innerText <- sprintf "clicked squared: %s" oldValue
        } |> Async.Start)

myText.onchange <- fun value -> (
    str.SetValue(myText.value))

myRevText.onchange <- fun value -> (
    rev.SetValue(myRevText.value))

rev.Changed.Add( fun x -> (async {
        let! v = rev.Evaluate()
        if v <> myRevText.value then
            myRevText.value <- v } |> Async.Start ))

str.Changed.Add( fun x -> (async {
        let! v = str.Evaluate()
        if v <> myText.value then
            myText.value <- v } |> Async.Start ))



