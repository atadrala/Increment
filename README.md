# Combinatorial lib for UI and state management

Some fun with lenses, computational graphs, Fable and react.
## Inspiration 
- [Tech Mesh 2012 - Eden: An F#/WPF Framework for building GUI Tools - Tobias Gedell](https://www.youtube.com/watch?v=BsOtAXV_URI)
- [Seven Implementations of Incremental](https://www.youtube.com/watch?v=G6a5G5i4gQU)
- [Introduction to Incr_dom: Writing Dynamic Web Apps in OCaml](https://www.youtube.com/watch?v=h_e5pPKI0K4)

## The idea
Lets represents components as functions (`state -> view`). The function is lifted into calculation graph to `IMutableNode<state> -> ICalc<view>`. We can compose those function by applying lances (pair of geter and setter) on the IMutableState and composing resulting.  

Combinatorial operators would allow to declare UI in similar fasion static markup is written. 

For example:
```
let personGridEditor : EditorComponent<Person>
    = Html.div 
        Html.span <<|
            (StringEditor >>>> StringEditor >>>> IntEditor >>>> DateEditor  ^|> Person.TupleLens |> VirtualizedGrid)
```

 where  
 - `>>>>` is operator that tuples state paramerer and stacks views results on component function. 
 - `^|>` is operator that applies lens on the right side of the operator to the input parameter of component function on the left side of the operator.
 - `<<|` maps resulting view through a function.