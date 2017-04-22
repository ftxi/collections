#!usr/bin/env julia

str = "CH4 K3[Fe(CN)6] C(NH2)3CH(CH3)2"

function lindep{T}(args::Vector{T}...)
    length(args) > length(args[1]) && false
    rank(hcat(args...)) == length(args)
end

elems = Dict{String, Integer}()

for elemdata in split(str)
    flag = true
    while flag
        flag = false
        m = match(r".*?\((.*?)\)([0-9]*)", elemdata)
        m != nothing && ((flag=true; k = m[2]=="" ? 1 : parse(Int, m[2]));
            elemdata = replace(elemdata, r"(\(.*?\)[0-9]*)",  m[1]^k, 1))
        m = match(r".*?\[(.*?)\]([0-9]*)", elemdata)
        m != nothing && ((flag=true; k = m[2]=="" ? 1 : parse(Int, m[2]));
            elemdata = replace(elemdata, r"(\[.*?\][0-9]*)",  m[1]^k, 1))
        m = match(r".*?\{(.*?)\}([0-9]*)", elemdata)
        m != nothing && ((flag=true; k = m[2]=="" ? 1 : parse(Int, m[2]));
            elemdata = replace(elemdata, r"(\{.*?\}[0-9]*)",  m[1]^k, 1))
    end
    
    println(elemdata)
end
