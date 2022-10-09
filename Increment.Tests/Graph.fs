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
    let input = Inc.Var(0)
    let mutable changed = false
    let mutable calculated: bool = false
    let calcNode = Inc.Map(input, fun x -> calculated <- true; x + x)

    calcNode.Changed.Add( fun _ -> changed <- true)
    Async.RunSynchronously <| input.SetValue 2
    Assert.That(changed, Is.True) 
    Assert.That(calculated, Is.False) 

    let result = Async.RunSynchronously <| calcNode.Evaluate()
    Assert.That(result, Is.EqualTo(4))
    Assert.That(calculated, Is.True) 

[<Test>]
let LensPropageteInputChange () =
    let input = Inc.Var(0)
    let mutable changed = false
    let mutable projected: bool = false
    let lens : Lens<int,int> = (fun x -> projected <- true; x + 1), (fun x r -> x - 1) 
    let zoomed = Inc.Zoom(input, lens)

    zoomed.Changed.Add( fun _ -> changed <- true)
    Async.RunSynchronously <| input.SetValue 2
    Assert.That(changed, Is.True) 
    Assert.That(projected, Is.False) 

    let result = Async.RunSynchronously <| zoomed.Evaluate()
    Assert.That(result, Is.EqualTo(3))
    Assert.That(projected, Is.True)

[<Test>]
let LensPropageteZoomedChange () =
    let input = Inc.Var(0)
    let mutable changed = false
    let mutable injected: bool = false
    let lens : Lens<int,int> = (fun x -> x + 1), (fun x r -> injected <- true; x - 1) 
    let zoomed = Inc.Zoom(input, lens)

    input.Changed.Add( fun _ -> changed <- true)
    Async.RunSynchronously <| zoomed.SetValue 3
    Assert.That(changed, Is.True) 
    Assert.That(injected, Is.True) 

    let result = Async.RunSynchronously <| input.Evaluate()
    Assert.That(result, Is.EqualTo(2))
    Assert.That(injected, Is.True)

[<Test>]
let BindNodeEvaluateOnlyNeededDeps() =
    let boolinput = Inc.Var(false)
    let mutable falseCalculated = false
    let mutable trueCalculated = false
    let mutable changed = false

    let trueCase = Inc.Map(Inc.Var(1), fun x -> trueCalculated <- true; x)
    let falseCase = Inc.Map(Inc.Var(0), fun x -> falseCalculated <- true; x)

    let bindNode: INode<int> = new BindNode<int>( Inc.Map(boolinput, function |true -> trueCase |false -> falseCase))
    bindNode.Changed.Add( fun _ -> changed <- true)
    let result = Async.RunSynchronously <| bindNode.Evaluate()
    Assert.That(result, Is.EqualTo(0))
    Assert.That(falseCalculated, Is.True)
    Assert.That(trueCalculated, Is.False)

    falseCalculated <- false
    trueCalculated <- false

    Async.RunSynchronously <| boolinput.SetValue(true)
    Assert.That(changed, Is.True)
    let result = Async.RunSynchronously <| bindNode.Evaluate()
    Assert.That(result, Is.EqualTo(1))
    Assert.That(falseCalculated, Is.False)
    Assert.That(trueCalculated, Is.True)

[<Test>]
let BindNodePropagatesNestedChanged() =
    let inner = Inc.Var(10)
    let outer = Inc.Var(0)
    let mutable changed = false;

    let bindNode = Inc.Bind( Inc.Map(outer, function _ -> inner))

    bindNode.Changed.Add( fun _ -> changed <- true)
    let result = Async.RunSynchronously <| bindNode.Evaluate()
    Assert.That(result, Is.EqualTo(10))

    Async.RunSynchronously <| inner.SetValue(20)
    Assert.That(changed, Is.True)

[<Test>]
let BindNodeDoesNotPropagatesNestedChangedIfUpdated() =
    let inner1 = Inc.Var(10)
    let inner2 = Inc.Var(0)
    let outer = Inc.Var(inner1 :> INode<_>)
    let mutable changed = false
  
    let bindNode: INode<int> = Inc.Bind(outer)
    bindNode.Changed.Add( fun _ -> changed <- true)

    let result = Async.RunSynchronously <| bindNode.Evaluate()
 
    Async.RunSynchronously <| inner1.SetValue(20)
    Assert.That(changed, Is.True)
    changed <- false
    Async.RunSynchronously <| inner2.SetValue(30)
    Assert.That(changed, Is.False)
    Async.RunSynchronously <|outer.SetValue(inner2)

    changed <- false
    Async.RunSynchronously <| inner1.SetValue(40)
    Assert.That(changed, Is.False)
    changed <- false
    let result = Async.RunSynchronously <| bindNode.Evaluate()
    Async.RunSynchronously <| inner2.SetValue(50)
    Assert.That(changed, Is.True)

[<Test>]
let CalcNodesDoesNotCallFunctionIfDepsEvaluateToPrevValue() = 
    let mutable recalced = false
    let mut = Inc.Var(1)
    let map1 = Inc.Map(mut, fun x -> x % 2)
    let map2 = Inc.Map(map1, fun x -> recalced <- true; x+1)

    let r1, r2 = 
        async {
            let! result1 = map2.Evaluate()
            recalced <- false
            do! mut.SetValue(3)
            let! result2 = map2.Evaluate()
            return result1, result2
        } |> Async.RunSynchronously
    Assert.That(r1, Is.EqualTo(r2))
    Assert.That(recalced, Is.False)

[<Test>]
let CalcNode2DoesNotCallFunctionIfDepsEvaluateToPrevValue() = 
    let mutable recalced = false
    let mut1 = Inc.Var(0)

    let map1 = Inc.Map(mut1, fun x -> x % 2)
    let map2 = Inc.Map(map1, map1, fun x _ -> recalced <- true; x+1)

    let r1, r2 = 
        async {
            let! result1 = map2.Evaluate()
            recalced <- false
            do! mut1.SetValue(2)
            let! result2 = map2.Evaluate()
            return result1, result2
        } |> Async.RunSynchronously
    Assert.That(r1, Is.EqualTo(r2))
    Assert.That(recalced, Is.False)

[<Test>]
let CalcNode3DoesNotCallFunctionIfDepsEvaluateToPrevValue() = 
    let mutable recalced = false
    let mut1 = Inc.Var(1)

    let map1 = Inc.Map(mut1, fun x -> x % 2)
    let map2 = Inc.Map(map1, map1, map1, fun x _ _ -> recalced <- true; x+1)

    let r1, r2 = 
        async {
            let! result1 = map2.Evaluate()
            recalced <- false
            do! mut1.SetValue(3)
            let! result2 = map2.Evaluate()
            return result1, result2
        } |> Async.RunSynchronously
    Assert.That(r1, Is.EqualTo(r2))
    Assert.That(recalced, Is.False)


