
__precompile__()

module Laziness

import Base: convert, show

export @delay, force

type Promise
    value
    evaluated::Bool
end

Promise(a::Any) = Promise(a, true)

Promise(f::Function) = Promise(f, false)

"""
    @delay(ex)

Introduce laziness to julia.

# Examples

```jldoctest
julia> function foo(x)
           @delay x+1
       end
foo (generic function with 1 method)

julia> p = foo(2)
Promise
value → #[...]

julia> force(p)
3

julia> p
Evaluated promise
value → 3
```

"""

macro delay(ex)
    quote Promise(()->$(esc(ex))) end
end

"""
    force(p::Promise) -> Any

To 'force' a promise means to run its code.
The results will be saved, which means forcing twice will run only once.

See also: `@delay`
"""

force(p::Promise) = begin
    if p.evaluated
        p.value
    else
        p.value = p.value()
        p.evaluated = true
        p.value
    end
end

convert(::Type{T}, p::Promise) where{T} = convert(T, force(p))

Base.show(io::IO, p::Promise) = begin
    println(io, p.evaluated ? "Evaluated promise" : "Promise")
    print(io, "value → $(p.value)")
end

end #module
