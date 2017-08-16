#!usr/bin/env julia

include("chemical_parse.jl")

str = "CH4 K3[Fe(CN)6] C(NH2)3CH(CH3)2"
str = "CH4 O2 CO2 H2O"
str = "HCl Na2CO3 NaCl H2O CO2"

function lindep{T}(args::Vector{T}...)
    length(args) > length(args[1]) && false
    rank(hcat(args...)) == length(args)
end

## function chemical_solve(str::AbstractString)
M, row, column = chemical_parse(str)
rk = rank(M)
if rk == length(row)
    return "No solution."
else
    
end
## end



    

