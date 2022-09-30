namespace Increment

open Graph

module Lens = 
    type Lens<'s,'r> = ( 's -> 'r)*('r -> 's -> 's)

    let compose ((prj1, inj1): Lens<'a,'b>) ((prj2, inj2): Lens<'b,'c>) : Lens<'a,'c> =
        (prj1 >> prj2), (fun r s -> s |> (s |> prj1 |> inj2 r |> inj1))

    type ZoomNode<'S, 'R>(node: IMutableNode<'S>, lens: Lens<'S,'R>) =
        let (prj, inj) = lens
        interface INode<'R> with
            member _.Evaluate() = async {
                let! value = node.Evaluate()
                return prj value
            }
            member _.Changed = node.Changed
        
        interface IMutableNode<'R> with
            member this.SetValue (newValue: 'R) = 
                node.Apply(inj newValue)
            member this.Apply (f: 'R -> 'R) = 
                let outerF x = 
                    inj ( f <| prj x) x 
                node.Apply(outerF)

    let zoom(node:IMutableNode<'S>, lens: Lens<'S,'R>) : IMutableNode<'R> = 
            new ZoomNode<'S,'R>(node, lens) :> IMutableNode<'R>

    let indexLens (idx: int) : Lens<'t array,'t option> = 
       (fun x -> if idx >= 0 && idx <= x.Length then Some (x.[idx]) else None), 
       (fun r x -> 
            match r with 
            | Some r ->  if idx >= 0 && idx <= x.Length then do x.[idx] <- r
            | None -> ()
            x)

    let fstLens : Lens<'f*'s, 'f> = (fun (a,_) -> a), (fun n (_,b) -> (n,b))
    let sndLens : Lens<'f*'s, 's> = (fun (_,b) -> b), (fun n (a,_) -> (a,n))

    let unsafeFromOption ( l: Lens<'s, 't option>) : Lens<'s,'t> =
            compose l
                (Option.defaultWith (fun () -> 
                    failwith "Use of fromOption on None value in unsafeFromOption lense"),
                 fun r _-> Some r)