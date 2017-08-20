
using Lazy

cfrac(n::Real) = let m = floor(n)
    if (n-m)==0
        @lazy Integer(m)
    else
        @lazy Integer(m):cfrac(1/(n-m))
    end
end

restore_cfrac(cf::List) = begin
    ans = Inf
    for x in reverse(cf)
        ans = x + 1/ans
    end
    ans
end
