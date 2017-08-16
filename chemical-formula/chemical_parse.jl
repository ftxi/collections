

"""
return an table of the amount of each element in every molecule
as well as what the number of the rows and columns stand for.
"""

function chemical_parse(str::String)
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
            @assert  'A' ≤ elemdata[i] ≤ 'Z'
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
        end
    end
    row = split(str)
    revelems = Dict(v=>k for (k, v) in elems)
    column = [revelems[i] for i in 1:length(revelems)]
    return arr, row, column
end
