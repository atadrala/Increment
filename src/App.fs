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
    }
    with 
        static member NameLens : Lens<Person, string> = (fun p -> p.Name), (fun name p -> {p with Name = name})
        static member LastLens : Lens<Person, string> = (fun p -> p.LastName), (fun lastName p -> {p with LastName = lastName})
        static member DateofBirthLens : Lens<Person, DateTime> = (fun p -> p.DateOfBirth), (fun dob p -> {p with DateOfBirth = dob})
        static member PlaceOfBirthLens : Lens<Person, string> = (fun p -> p.PlaceOfBirth), (fun pob p -> {p with PlaceOfBirth = pob})
        static member TupleLens : Lens<Person, (string*string)*string> = 
                            (fun p -> ((p.Name, p.LastName), p.PlaceOfBirth)), 
                            (fun ((n,ln), pob) p -> { p with Name=n; LastName=ln; PlaceOfBirth= pob })

let createPerson (n:int) = 
    {
        Name="Name " + (string n);
        LastName= "LastName " + (string n);
        PlaceOfBirth = "City " + (string n);
        DateOfBirth = DateTime.Now;
    }


let PersonEditorF : EditorComponentF<_, _> = StringEditor.f'  >>>> StringEditor.f'  >>>> StringEditor.f' |^ Person.TupleLens
let PersonEditorF2 : EditorComponentF<_, _> = PersonEditorF |>> wrapWithFlexDiv


type MyApp(state: IMutableNode<Person[]>) = 

    let scrollPosition = new MutableNode<float>(400.) :> IMutableNode<_>

    let bufferSize = 4
    let elementHeight = 20
    let viewHeight = 100

    let viewSpan = new CalcNode<_,_>(scrollPosition :> INode<_>, fun scroll -> 
                            let start = max  0 (int scroll / elementHeight - bufferSize)
                            let end_ = start + (viewHeight / elementHeight) + 2 * bufferSize
                            scroll, start, end_
                        )

    let span = new CalcNode<_,_>(viewSpan, fun (_,start,stop) -> (start, stop))

    let count = 100

    let editors = virtualize(state, PersonEditorF2, span)

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
                                                prop.children x.GetViews;
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

    new() = MyApp(new MutableNode<Person[]>( [|0..100|] |> Array.map createPerson ) :> IMutableNode<_>)


let app =  new MyApp()

async { 
        let! v = app.View.Evaluate()
        ReactDom.render(v, myAppElement) } |> Async.Start
      
app.View.Changed.Add( fun v -> async { 
        let! v = app.View.Evaluate()
        ReactDom.render(v, myAppElement)} |> Async.Start
        )



