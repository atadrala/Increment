module lib.VirtualizationTest

open NUnit.Framework
open Increment.Graph
open Increment.Lens
open Increment.Components
open Increment.Virtualization
open Increment.Web
open Increment

type TestView(state: IMutableNode<string>, value:string) =
    member this.Value = value
    member this.SetValue v = state.SetValue v
    interface Feliz.ReactElement


type TestEditor(state: IMutableNode<string>) = 

    let view = Inc.Map(state, fun x ->  new TestView(state, x))
    member this.View = view :> INode<_> 
    member this.SetValue str = state.SetValue str

    static member f state = (new TestEditor(state)).View

[<Test>]
let EditorIsNotConstructedWhenViewIsNotEvaluated() = 
    let data = Inc.Var( [|1..100|] |> Array.map string)
    let span = Inc.Var(5,10)
    let mutable counter = 0
    let virtualizedEditor = 
        virtualize(
            data, 
            (fun mn -> counter <- counter + 1;  (TestEditor.f mn)),
            span) :> INode<_>

    Assert.That(counter, Is.EqualTo(0))    

[<Test>]
let VirtualizedEditorsAreReused () =
    let data = Inc.Var( [|1..100|] |> Array.map string )
    let span = Inc.Var(5,10)
    let mutable counter = 0
    let virtualizedEditor = 
        virtualize(
            data, 
            (fun mn -> counter <- counter + 1; (TestEditor.f mn)),
            span) :> INode<_>

    let view = virtualizedEditor.Evaluate() |> Async.RunSynchronously
    Assert.That(counter, Is.EqualTo(6))

[<Test>]
let WhenSpanShiftedThenEditorsViewPresentShiftedData() = 
    let data =Inc.Var( [|0..100|] |> Array.map string )
    let span =Inc.Var(5,10)
    let virtualizedEditor = 
        virtualize(data, TestEditor.f , span) :> INode<_>

    let view = virtualizedEditor.Evaluate() |> Async.RunSynchronously
    Assert.That(view[0].Value, Is.EqualTo("5"))
    Assert.That(view[5].Value, Is.EqualTo("10"))

    span.SetValue((10,15))
    let viewShifted = virtualizedEditor.Evaluate() |> Async.RunSynchronously
    Assert.That(viewShifted[0].Value, Is.EqualTo("10"))
    Assert.That(viewShifted[5].Value, Is.EqualTo("15"))

[<Test>]
let WhenSpanShiftsEditorsAreReused() =
    let data =Inc.Var( [|1..100|] |> Array.map string)
    let span =Inc.Var(5,10)
    let mutable counter = 0
    let virtualizedEditor = 
        virtualize(
            data, 
            (fun mn -> counter <- counter + 1;  TestEditor.f mn),
            span) :> INode<_>

    virtualizedEditor.Evaluate() |> Async.RunSynchronously |> ignore
    span.SetValue((10,15))
    virtualizedEditor.Evaluate() |> Async.RunSynchronously |> ignore
    span.SetValue((20,25))
    virtualizedEditor.Evaluate() |> Async.RunSynchronously |> ignore
    Assert.That(counter, Is.EqualTo(6))


[<Test>]
let WhenSpanShiftsEditsAffectsUnderlyingState() =
    let data =Inc.Var( [|0..100|] |> Array.map string)
    let span =Inc.Var(0,10) :> IMutableNode<_>
    let mutable counter = 0
    let virtualizedEditor = 
        virtualize(
            data, 
            TestEditor.f,
            span) :> INode<_>

    for i in [0..10..90] do 
        Async.RunSynchronously <| span.SetValue((i,i+10))
        let editors = virtualizedEditor.Evaluate() |> Async.RunSynchronously
        for editor in editors do
           Async.RunSynchronously <| editor.SetValue "Test"

    let values = (data :> INode<_>).Evaluate() |> Async.RunSynchronously 

    Assert.That(values, Is.EqualTo(Seq.replicate 101 "Test" |> Seq.toArray))


//[<Test>]
//let WhenSpanShiftsReusedEditorsShouldNotChangeState() = // When index span intersection is not empty before and after shift then editors should reused the same indexed.
//    let data =Inc.Input( [|1..100|] |> Array.map string )
//    let span =Inc.Input(50,60) :> IMutableNode<_>
//    let mutable counter = 0
//    let virtualizedEditor = 
//        Virtualization.virtualize(
//            data, 
//            (fun mn -> mn.Changed.Subscribe(fun () -> counter <- counter + 1); new TestEditor(mn)),
//            span) :> INode<_>

//    virtualizedEditor.Evaluate() |> Async.RunSynchronously |> ignore
//    span.SetValue((60,70))
//    virtualizedEditor.Evaluate() |> Async.RunSynchronously |> ignore
//    Assert.That(counter, Is.EqualTo(11))