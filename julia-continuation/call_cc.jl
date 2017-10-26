#!/usr/bin/env julia

# This attempts to implement call/cc feature in julia
# However

type Continuation_thrown{T}
    value :: T
end

macro call_cc(name::Symbol, content)
    esc( # the esc(...) is useless in this 
    quote
        $name = v -> throw(Continuation_thrown(v))
        try
            $content
        catch e
            if isa(e, Continuation_thrown)
                e.value
            end
        end
    end)
end

