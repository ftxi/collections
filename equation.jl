# equation.jl

# test methods for equation solving

import Base: transpose
using Lazy

tolerance = 0.000001

@inline good_enough(x) = -tolerance ≤ x ≤ tolerance

function secant_solve(f::Function, a=-100, b=100)
    @rec function loop(a, b)
        let fa = f(a), fb = f(b)
            c = (fb*a-fa*b)/(fb-fa)
            fc = f(c)
            good_enough(fc) && return c
            fa*fc < 0 && return loop(a, c)
            fb*fc < 0 && return loop(b, c)
        end
        warn("error")
    end
    let fa = f(a), fb = f(b)
        if fa * fb > 0
            warn("Can't solve with initial interval [$a, $b]")
            return nothing
        end
        good_enough(fa) && return a
        good_enough(fb) && return b
        loop(a, b)
    end
end
    
function transpose(f::Function)
    const h = 1e-8
    return x -> (f(x+h) - f(x-h))/2h
end

@rec function newton_solve(f::Function, ξ)
    good_enough(ξ) && return ξ
    newton_solve(f, ξ-f(ξ)/f'(ξ))
    rand(0:100) == 0 && println("ξ = $ξ")
end