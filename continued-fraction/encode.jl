
#!/usr/local/bin/julia

include("cfrac.jl")

if length(ARGS) != 1
    print_with_color(:blue, "encode: \n")
    println("encode a file to continued fraction")
    println("usage: julia encode.jl ≪filename≫")
    exit(0)
end

#=
str = """
This is a sentence to be shadowed.
This sentence may be very long.
And it may contain unicode characters.
∫e^x ∂x = e^x+C
∀x(human(x)⇒mortal(x))
"""
=#

filename = ARGS[1]
inputfile = open(filename, "r")

str = join(readlines(inputfile), "\n")


arraytype{T,N}(::Array{T,N}) = T

acc = big(0)
arr = transcode(Cwchar_t, str)
u = typemax(arraytype(arr))
for x in arr
    acc *= big(u) + 1
    acc += x
end

k = Int(round(log2(acc))) + 21
g = Int(floor(log10(acc)))


setprecision(BigFloat, k) do
    v = BigFloat(acc)
    v /= big(10)^g
    
    m = k÷4
    infl = cfrac(v)
    l = take(m, infl)
    #=
    outputfile = open("[encoded]"*filename, "w")
    info("first iteration")
    println(outputfile, "continued-fraction encoded file with ", g, " decimal precision")
    println(l)
    close(outputfile)
    =#
    
    info("trying to decrease...")
    outputfile = open("[encoded]"*filename, "w")
    v′ = restore_cfrac(l)
    while v != v′
        m += g÷50
        l = take(m, infl)
        v′ = restore_cfrac(l)
        info("iter: trying ", m, " numbers")
    end
    @show str
    @show acc
    @show k, m, g
    @show v
    @show l
    @show v′
    println(outputfile, "continued-fraction encoded file with ", g, " decimal precision")
    println(l)
end



