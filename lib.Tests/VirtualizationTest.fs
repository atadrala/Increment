module lib.VirtualizationTest

open NUnit.Framework
open Lib
open Routing

type TestView( value:string) =
    member this.Value = value
    interface Feliz.ReactElement

type TestEditor(state: IMutableNode<string>) = 
    inherit EditorComponent<string>(state)
    let view = new CalcNode<_,_>(state, fun x -> new TestView(x) :> Feliz.ReactElement)
    override this.View = view :> INode<_> 

[<Test>]
let EditorIsNotConstructedWhenViewIsNotEvaluated() = 
    let data = new MutableNode<string[]>( [|1..100|] |> Array.map string )
    let span = new MutableNode<int*int>(5,10)
    let mutable counter = 0
    let virtualizedEditor = 
        Virtualization.virtualize(
            data, 
            (fun mn -> counter <- counter + 1; new TestEditor(mn)),
            span) :> INode<_>

    Assert.That(counter, Is.EqualTo(0))    

[<Test>]
let VirtualizedEditorsAreReused () =
    let data = new MutableNode<string[]>( [|1..100|] |> Array.map string )
    let span = new MutableNode<int*int>(5,10)
    let mutable counter = 0
    let virtualizedEditor = 
        Virtualization.virtualize(
            data, 
            (fun mn -> counter <- counter + 1; new TestEditor(mn)),
            span) :> INode<_>

    let view = virtualizedEditor.Evaluate() |> Async.RunSynchronously
    Assert.That(counter, Is.EqualTo(6))

[<Test>]
let WhenSpanShiftedThenEditorsViewPresentShiftedData() = 
    let data = new MutableNode<string[]>( [|0..100|] |> Array.map string )
    let span = new MutableNode<int*int>(5,10) :> IMutableNode<_>
    let virtualizedEditor = 
        Virtualization.virtualize(data, (fun mn -> new TestEditor(mn)), span) :> INode<_>

    let view = virtualizedEditor.Evaluate() |> Async.RunSynchronously
    Assert.That((view[0] :?> TestView).Value, Is.EqualTo("5"))
    Assert.That((view[5] :?> TestView).Value, Is.EqualTo("10"))

    span.SetValue((10,15))
    let viewShifted = virtualizedEditor.Evaluate() |> Async.RunSynchronously
    Assert.That((viewShifted[0] :?> TestView).Value, Is.EqualTo("10"))
    Assert.That((viewShifted[5] :?> TestView).Value, Is.EqualTo("15"))

[<Test>]
let WhenSpanShiftsEditorsAreReused() =
    let data = new MutableNode<string[]>( [|1..100|] |> Array.map string )
    let span = new MutableNode<int*int>(5,10) :> IMutableNode<_>
    let mutable counter = 0
    let virtualizedEditor = 
        Virtualization.virtualize(
            data, 
            (fun mn -> counter <- counter + 1; new TestEditor(mn)),
            span) :> INode<_>

    let view = virtualizedEditor.Evaluate() |> Async.RunSynchronously
    span.SetValue((10,15))

    let viewShifted = virtualizedEditor.Evaluate() |> Async.RunSynchronously
    Assert.That(counter, Is.EqualTo(6))
