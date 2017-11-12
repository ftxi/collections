#!usr/bin/env julia

include("call_cc.jl")

@show @call_cc cc begin
    5*6+cc(7)
end # ⇒ 9


block(content::Function, except::Function) =
    (@call_cc success begin
        except(@call_cc fail begin
            success(content(fail))
        end)
    end)

block(
    raise -> begin
        function safe_div(a, b)
            if b == 0
                raise("divide by zero")
            else
                div(a, b)
            end
        end
        @show safe_div(3/4)
        @show safe_div(9/6)
        @show safe_div(2/0)
    end,
    errmsg -> begin
        print_with_color(:red, errmsg)
    end
)
