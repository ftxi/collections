

type Nil end

type List{T}
    ar :: Union{T, List{T}, Nil}
    dr :: Union{T, List{T}, Nil}
end

cons = List
car(l) = l.ar
cdr(l) = l.dr

