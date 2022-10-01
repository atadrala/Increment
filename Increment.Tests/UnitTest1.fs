module lib.Tests

open NUnit.Framework
open Increment.Graph
open Increment.Lens
open Increment.Components
open Increment.Virtualization
open Increment.Web
open Increment.Combinatorics
open Increment

[<SetUp>]
let Setup () =
    ()

[<Test>]
let CalcNode () =
    let input = Inc.Input(0)
    let mutable changed = false
    let mutable calculated: bool = false
    let calcNode = Inc.Calc(input, fun x -> calculated <- true; x + x)

    calcNode.Changed.Add( fun _ -> changed <- true)
    do input.SetValue 2
    Assert.That(changed, Is.True) 
    Assert.That(calculated, Is.False) 

    let result = Async.RunSynchronously <| calcNode.Evaluate()
    Assert.That(result, Is.EqualTo(4))
    Assert.That(calculated, Is.True) 

[<Test>]
let LensPropageteInputChange () =
    let input = Inc.Input(0)
    let mutable changed = false
    let mutable projected: bool = false
    let lens : Lens<int,int> = (fun x -> projected <- true; x + 1), (fun x r -> x - 1) 
    let zoomed = Inc.Zoom(input, lens)

    zoomed.Changed.Add( fun _ -> changed <- true)
    do input.SetValue 2
    Assert.That(changed, Is.True) 
    Assert.That(projected, Is.False) 

    let result = Async.RunSynchronously <| zoomed.Evaluate()
    Assert.That(result, Is.EqualTo(3))
    Assert.That(projected, Is.True)

[<Test>]
let LensPropageteZoomedChange () =
    let input = Inc.Input(0)
    let mutable changed = false
    let mutable injected: bool = false
    let lens : Lens<int,int> = (fun x -> x + 1), (fun x r -> injected <- true; x - 1) 
    let zoomed = Inc.Zoom(input, lens)

    input.Changed.Add( fun _ -> changed <- true)
    do zoomed.SetValue 3
    Assert.That(changed, Is.True) 
    Assert.That(injected, Is.False) 

    let result = Async.RunSynchronously <| input.Evaluate()
    Assert.That(result, Is.EqualTo(2))
    Assert.That(injected, Is.True)

[<Test>]
let BindNodeEvaluateOnlyNeededDeps() =
    let boolinput = Inc.Input(false)
    let mutable falseCalculated = false
    let mutable trueCalculated = false
    let mutable changed = false

    let trueCase = Inc.Calc(Inc.Input(1), fun x -> trueCalculated <- true; x)
    let falseCase = Inc.Calc(Inc.Input(0), fun x -> falseCalculated <- true; x)

    let bindNode: INode<int> = new BindNode<int>( Inc.Calc(boolinput, function |true -> trueCase |false -> falseCase))
    bindNode.Changed.Add( fun _ -> changed <- true)
    let result = Async.RunSynchronously <| bindNode.Evaluate()
    Assert.That(result, Is.EqualTo(0))
    Assert.That(falseCalculated, Is.True)
    Assert.That(trueCalculated, Is.False)

    falseCalculated <- false
    trueCalculated <- false

    boolinput.SetValue(true)
    Assert.That(changed, Is.True)
    let result = Async.RunSynchronously <| bindNode.Evaluate()
    Assert.That(result, Is.EqualTo(1))
    Assert.That(falseCalculated, Is.False)
    Assert.That(trueCalculated, Is.True)

[<Test>]
let BindNodePropagatesNestedChanged() =
    let inner = Inc.Input(10)
    let outer = Inc.Input(0)
    let mutable changed = false;

    let bindNode = Inc.Bind( Inc.Calc(outer, function _ -> inner))

    bindNode.Changed.Add( fun _ -> changed <- true)
    let result = Async.RunSynchronously <| bindNode.Evaluate()
    Assert.That(result, Is.EqualTo(10))

    inner.SetValue(20)
    Assert.That(changed, Is.True)

[<Test>]
let BindNodeDoesNotPropagatesNestedChangedIfUpdated() =
    let inner1 = Inc.Input(10)
    let inner2 = Inc.Input(0)
    let outer = Inc.Input(inner1 :> INode<_>)
    let mutable changed = false
  
    let bindNode: INode<int> = Inc.Bind(outer)
    bindNode.Changed.Add( fun _ -> changed <- true)

    let result = Async.RunSynchronously <| bindNode.Evaluate()
 
    inner1.SetValue(20)
    Assert.That(changed, Is.True)
    changed <- false
    inner2.SetValue(30)
    Assert.That(changed, Is.False)
    outer.SetValue(inner2)

    changed <- false
    inner1.SetValue(40)
    Assert.That(changed, Is.False)
    changed <- false
    let result = Async.RunSynchronously <| bindNode.Evaluate()
    inner2.SetValue(50)
    Assert.That(changed, Is.True)




