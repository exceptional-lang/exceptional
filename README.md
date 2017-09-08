# Exeptional

## Preface

Exceptional is a toy language and VM. It's original idea was the result of a discussion on using exceptions for control-flow. I joked that if _I_ were to create a language, the programmer would not have to choose between control-flow structures and exceptions; there would be no control-flow structures.

Exceptional's VM is a terrible VM. Almost every line of code can be carbon-dated as I tried to learn Rust the hard way. The bytecode is probably garbage. If only Robert Nystrom had finished writing [his book](http://www.craftinginterpreters.com/contents.html).

## Language

Exceptional is a language where control-flow is only done through exceptions. Rescue blocks pattern match exceptions to achieve that goal. 

Exceptional has a few other peculiarities:
- Unhandled exceptions are ignored
- Functions never return

Here's an example of how one can implement fibonacci.

```
fibonacci = fn(x) do
  rescue({ "m" => m, "k" => 0 }) do
    raise({ "result" => m })
  end
  rescue({ "m" => m, "n" => n, "k" => k }) do
    raise({ "m" => n, "n" => m + n, "k" => k - 1 })
  end
  raise({ "m" => 0, "n" => 1, "k" => k })
end
```

## Acknowledgments

I want to thank everyone that helped me with this language: 
- @sebb for the initial idea
- @sgnr for his invaluable help designing and implementing the language
- @alexsnaps for teaching me computers
