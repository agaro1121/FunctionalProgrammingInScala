#Function Literal Examples:

```scala  
println(formatResult("absolute value", -42, abs))
println(formatResult("factorial", 7, factorial))
println(formatResult("increment", 7, (x: Int) => x + 1))
println(formatResult("increment2", 7, (x) => x + 1))
println(formatResult("increment3", 7, x => x + 1))
println(formatResult("increment4", 7, _ + 1))
println(formatResult("increment5", 7, x => { val r = x + 1; r }))
```

