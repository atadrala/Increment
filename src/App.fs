module App

open Feliz
open Browser.Dom
open Fable.React
open System

open Increment.Graph
open Increment.Lens
open Increment.Inc
open Increment.Virtualization
open Increment.Web
open Increment.Combinatorics


// Mutable variable to count the number of times we clicked the button

let myAppElement = document.querySelector(".my-app") :?> Browser.Types.HTMLElement

type Person = { 
        Name:string; 
        LastName:string; 
        DateOfBirth: DateTime;
        PlaceOfBirth: string; 
        Age: int;
    }
    with 
        static member NameLens : Lens<Person, string> = (fun p -> p.Name), (fun name p -> {p with Name = name})
        static member LastLens : Lens<Person, string> = (fun p -> p.LastName), (fun lastName p -> {p with LastName = lastName})
        static member DateofBirthLens : Lens<Person, DateTime> = (fun p -> p.DateOfBirth), (fun dob p -> {p with DateOfBirth = dob})
        static member PlaceOfBirthLens : Lens<Person, string> = (fun p -> p.PlaceOfBirth), (fun pob p -> {p with PlaceOfBirth = pob})
        static member TupleLens : Lens<Person, ((string*string)*string)*int> = 
                            (fun p -> (((p.Name, p.LastName), p.PlaceOfBirth)), p.Age), 
                            (fun (((n,ln), pob),age) p -> { p with Name=n; LastName=ln; PlaceOfBirth= pob; Age = age})

let createPerson (n:int) = 
    {
        Name="Name " + (string n);
        LastName= "LastName " + (string n);
        PlaceOfBirth = "City " + (string n);
        DateOfBirth = DateTime.Now;
        Age = n;
    }


let PersonEditorF : EditorComponentF<_, _> =  Person.TupleLens ^| (StringEditor<_>.f' >>>> StringEditor<_>.f' >>>> StringEditor<_>.f' >>>> StringEditor<_>.f')
let PersonEditorF2 : EditorComponentF<_, _> = PersonEditorF |>> wrapWithFlexDiv |>> (function | ComposableView.V x ->  x.Head)

let grid : EditorComponentF<_, _> = PersonEditorF2 |> VirtualizedGrid<Person>.f

let appView = 
    let state = new MutableNode<Person[]>( [|0..100|] |> Array.map createPerson)
    grid state


async { 
        let! v = appView.Evaluate()
        ReactDom.render(v, myAppElement) } |> Async.Start
      
appView.Changed.Add( fun v -> async { 
        let! v = appView.Evaluate()
        ReactDom.render(v, myAppElement)} |> Async.Start
        )



