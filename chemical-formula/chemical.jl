#!usr/bin/env julia

str = "CH4 K3[Fe(CN)6] C(NH2)3CH(CH3)2"
str = "CH4 O2 CO2 H2O"
str = "HCl Na2CO3 NaCl H2O CO2"

function lindep{T}(args::Vector{T}...)
    length(args) > length(args[1]) && false
    rank(hcat(args...)) == length(args)
end

elems = Dict{String, Integer}()

eldt = [
    begin 
        ## remove parentheses and brackets
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
        ## arrange elements' names
        
        println(elemdata)
        
        len = length(elemdata)
        for i in 1:len
            '0' ≤ elemdata[i] ≤ '9' && continue
            if 'A' ≤ elemdata[i] ≤ 'Z'
                name = i < len && 'a' ≤ elemdata[i+1] ≤ 'z' ?
                    elemdata[i:i+1] : elemdata[i:i]
                haskey(elems, name) || (elems[name] = length(elems)+1)
            end
        end
        elemdata
    end
for elemdata in split(str)]

arr = zeros(Int, length(elems), length(eldt))
## arr[<element>, <molecule>]
for (k, elemdata) in enumerate(eldt)
    len = length(elemdata)
    i = 1
    while i ≤ len
        if 'A' ≤ elemdata[i] ≤ 'Z'
            name = 
                if i < len && 'a' ≤ elemdata[i+1] ≤ 'z'
                    i += 1
                    elemdata[i-1:i]
                else
                    elemdata[i:i]
                end
            i += 1
            amount_str = ""
            while i ≤ len && '0' ≤ elemdata[i] ≤ '9'
                amount_str *= elemdata[i:i]
                i += 1
            end
            arr[elems[name], k] = amount_str=="" ? 1 : parse(Int, amount_str)
        else
            warn("something wrong: ",elemdata,"  ", elemdata[i:end])
        end
    end
end


