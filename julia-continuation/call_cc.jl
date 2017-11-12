#!/usr/bin/env julia

# This attempts to implement call/cc feature in julia


type Continuation_box{name}
    value
end


macro call_cc(name::Symbol, content)
    quote
        u = Symbol(randstring())
        $name = v -> throw(Continuation_thrown{u}(v))
        try
            $content
        catch e
            if e isa Continuation_box{u}
                e.value
            end
        end
    end
end
