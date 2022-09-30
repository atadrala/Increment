module lib.Tests

open NUnit.Framework
open Increment.Graph
open Increment.Lens
open Increment.Inc
open Increment.Virtualization
open Increment.Web
open Increment.Combinatorics

[<SetUp>]
let Setup () =
    ()

[<Test>]
let CalcNode () =
    let mutableNode = new MutableNode<_>(0)
    let mutable changed = false
    let mutable calculated: bool = false
    let calcNode = new CalcNode<_,_>(mutableNode, fun x -> calculated <- true; x + x) :> INode<_>

    calcNode.Changed.Add( fun _ -> changed <- true)
    do (mutableNode :> IMutableNode<_>).SetValue 2
    Assert.That(changed, Is.True) 
    Assert.That(calculated, Is.False) 

    let result = Async.RunSynchronously <| calcNode.Evaluate()
    Assert.That(result, Is.EqualTo(4))
    Assert.That(calculated, Is.True) 

[<Test>]
let LensPropageteInputChange () =
    let mutableNode = new MutableNode<_>(0) :> IMutableNode<_>
    let mutable changed = false
    let mutable projected: bool = false
    let lens : Lens<int,int> = (fun x -> projected <- true; x + 1), (fun x r -> x - 1) 
    let zoomed = zoom (mutableNode, lens) :> IMutableNode<_>

    zoomed.Changed.Add( fun _ -> changed <- true)
    do mutableNode.SetValue 2
    Assert.That(changed, Is.True) 
    Assert.That(projected, Is.False) 

    let result = Async.RunSynchronously <| zoomed.Evaluate()
    Assert.That(result, Is.EqualTo(3))
    Assert.That(projected, Is.True)

[<Test>]
let LensPropageteZoomedChange () =
    let mutableNode = new MutableNode<_>(0) :> IMutableNode<_>
    let mutable changed = false
    let mutable injected: bool = false
    let lens : Lens<int,int> = (fun x -> x + 1), (fun x r -> injected <- true; x - 1) 
    let zoomed = zoom (mutableNode, lens) :> IMutableNode<_>

    mutableNode.Changed.Add( fun _ -> changed <- true)
    do zoomed.SetValue 3
    Assert.That(changed, Is.True) 
    Assert.That(injected, Is.False) 

    let result = Async.RunSynchronously <| mutableNode.Evaluate()
    Assert.That(result, Is.EqualTo(2))
    Assert.That(injected, Is.True)

[<Test>]
let BindNodeEvaluateOnlyNeededDeps() =
    let bool = new MutableNode<_>(false) :> IMutableNode<_>
    let mutable falseCalculated = false
    let mutable trueCalculated = false
    let mutable changed = false

    let trueCase = new CalcNode<_,_>(new MutableNode<_>(1), fun x -> trueCalculated <- true; x) :> INode<_>
    let falseCase = new CalcNode<_,_>(new MutableNode<_>(0), fun x -> falseCalculated <- true; x) :> INode<_>

    let bindNode: INode<int> = new BindNode<int>( new CalcNode<_,_>(bool, function |true -> trueCase |false -> falseCase)) :> INode<_>
    bindNode.Changed.Add( fun _ -> changed <- true)
    let result = Async.RunSynchronously <| bindNode.Evaluate()
    Assert.That(result, Is.EqualTo(0))
    Assert.That(falseCalculated, Is.True)
    Assert.That(trueCalculated, Is.False)

    falseCalculated <- false
    trueCalculated <- false

    bool.SetValue(true)
    Assert.That(changed, Is.True)
    let result = Async.RunSynchronously <| bindNode.Evaluate()
    Assert.That(result, Is.EqualTo(1))
    Assert.That(falseCalculated, Is.False)
    Assert.That(trueCalculated, Is.True)

[<Test>]
let BindNodePropagatesNestedChanged() =
    let inner: IMutableNode<int> = new MutableNode<_>(10) :> IMutableNode<_>
    let outer: IMutableNode<int> = new MutableNode<_>(0) :> IMutableNode<_>
    let mutable changed = false;

    let bindNode: INode<int> = new BindNode<int>( new CalcNode<_,_>(outer, function _ -> inner)) :> INode<_>

    bindNode.Changed.Add( fun _ -> changed <- true)
    let result = Async.RunSynchronously <| bindNode.Evaluate()
    Assert.That(result, Is.EqualTo(10))

    inner.SetValue(20)
    Assert.That(changed, Is.True)

[<Test>]
let BindNodeDoesNotPropagatesNestedChangedIfUpdated() =
    let inner1: IMutableNode<int> = new MutableNode<_>(10) :> IMutableNode<_>
    let inner2: IMutableNode<int> = new MutableNode<_>(0) :> IMutableNode<_>
    let outer = new MutableNode<_>(inner1 :> INode<_>) :> IMutableNode<_>
    let mutable changed = false
  
    let bindNode: INode<int> = new BindNode<int>(outer :> INode<_>) :> INode<_>
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




