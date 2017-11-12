#!usr/bin/env julia
#testing version: 0.6.0

importall Base

struct Physical{T, m, kg, s, A, K, mol, cd}
    value :: T
end

Physical(x, m, kg, s, A, K, mol, cd) = Physical{typeof(x), m, kg, s, A, K, mol, cd}(x)
Physical(x; m=0, kg=0, s=0, A=0, K=0, mol=0, cd=0) = Physical{typeof(x), m, kg, s, A, K, mol, cd}(x)

@inline character_lift(x) = Dict(
    '0' => '⁰',
    '1' => '¹',
    '2' => '²',
    '3' => '³',
    '4' => '⁴',
    '5' => '⁵',
    '6' => '⁶',
    '7' => '⁷',
    '8' => '⁸',
    '9' => '⁹',
)[x]

function show(io::IO, x::Physical{T, m, kg, s, A, K, mol, cd}) where {T, m, kg, s, A, K, mol, cd}
    print(io, "$(x.value)")
    for (u, v) in [(kg, :kg), (A, :A), (m, :m), (K, :K), (mol, :mol), (cd, :cd), (s, :s)]
        if u != 0
            print(io, "⋅", v)
            if u < 0
                print(io, '⁻')
            end
            if u != 1
                print(io, map(character_lift, collect("$(abs(u))")))
            end
        end
    end
end


#addition
(+)(x::Physical{T1, m, kg, s, A, K, mol, cd}, y::Physical{T2, m, kg, s, A, K, mol, cd}) where {T1, T2, m, kg, s, A, K, mol, cd} =
    Physical(x.value + y.value, m, kg, s, A, K, mol, cd)

#negate
(-)(x::Physical{T, m, kg, s, A, K, mol, cd}) where {T, m, kg, s, A, K, mol, cd} =
    Physical(x.value - y.value, m, kg, s, A, K, mol, cd)

#substration
(-)(x::Physical{T1, m, kg, s, A, K, mol, cd}, y::Physical{T2, m, kg, s, A, K, mol, cd}) where {T1, T2, m, kg, s, A, K, mol, cd} =
    Physical(x.value - y.value, m, kg, s, A, K, mol, cd)

#multiplication with pure number
(*)(k, x::Physical{T, m, kg, s, A, K, mol, cd}) where {T, m, kg, s, A, K, mol, cd} =
    Physical{T, m, kg, s, A, K, mol, cd}(k * x.value)

(*)(x::Physical{T, m, kg, s, A, K, mol, cd}, k) where {T, m, kg, s, A, K, mol, cd} =
    Physical{T, m, kg, s, A, K, mol, cd}(x.value * k)

#general multiplication
(*)(x::Physical{T1, m1, kg1, s1, A1, K1, mol1, cd1}, y::Physical{T2, m2, kg2, s2, A2, K2, mol2, cd2}) where {T1, T2, m1, kg1, s1, A1, K1, mol1, cd1, m2, kg2, s2, A2, K2, mol2, cd2} =
    Physical(x.value * y.value, m1+m2, kg1+kg2, s1+s2, A1+A2, K1+K2, mol1+mol2, cd1+cd2)

#division with a pure number
(/)(k, x::Physical{T, m, kg, s, A, K, mol, cd}) where {T, m, kg, s, A, K, mol, cd} =
    Physical{T, -m, -kg, -s, -A, -K, -mol, -cd}(k / x.value)

(/)(x::Physical{T, m, kg, s, A, K, mol, cd}, k) where {T, m, kg, s, A, K, mol, cd} =
    Physical{T, m, kg, s, A, K, mol, cd}(x.value * k)

#general division
(/)(x::Physical{T1, m1, kg1, s1, A1, K1, mol1, cd1}, y::Physical{T2, m2, kg2, s2, A2, K2, mol2, cd2}) where {T1, T2, m1, kg1, s1, A1, K1, mol1, cd1, m2, kg2, s2, A2, K2, mol2, cd2} =
    Physical(x.value / y.value, m1-m2, kg1-kg2, s1-s2, A1-A2, K1-K2, mol1-mol2, cd1-cd2)

#power and square root
(^)(x::Physical{T, m, kg, s, A, K, mol, cd}, k) where {T, m, kg, s, A, K, mol, cd} =
    Physical(x.value ^ k, m*k, kg*k, s*k, A*k, K*k, mol*k, cd*k)

sqrt(x::Physical{T, m, kg, s, A, K, mol, cd}) where {T, m, kg, s, A, K, mol, cd} = let k = 1//2
    Physical(sqrt(x.value), m*k, kg*k, s*k, A*k, K*k, mol*k, cd*k)
end

#exponent and logrithm
#warning: all units are dropped. Probably I should come up with a better solution later
(^)(x, y::Physical) = x.value ^ k
(^)(x::Physical, y::Physical) = x ^ y.value

log(x::Physical) = log(x.value)
log2(x::Physical) = log2(x.value)
log10(x::Physical) = log10(x.value)
log(b, m) = log(m) - log(b)

#handling ambigious
(^)(x::Physical{T, m, kg, s, A, K, mol, cd}, k::Integer) where {T, m, kg, s, A, K, mol, cd} =
    Physical(x.value ^ k, m*k, kg*k, s*k, A*k, K*k, mol*k, cd*k)

(+)(x::Physical, y::Physical) = begin
    warn("Type mismatch while adding")
    show(x)
    print(" and ")
    show(y)
    x.value + y.value
end

meter = Physical(1, m=1)
kilogram = Physical(1, kg=1)
second = Physical(1, s=1)
ampere = Physical(1, A=1)
kelvin = Physical(1, K=1)
mole = Physical(1, mol=1)
candela = Physical(1, cd=1)
hertz = 1/second
newton = kilogram*meter/second^2
pascal = newton/meter^2
joule = newton*meter
watt = joule/second
coulomb = ampere * second
volt = watt/ampere
farad = coulumb/volt
ohm = volt/ampere
weber = volt*second
tesla = weber/meter^2
henry = weber/ampere
lumen = candela
lux = lumen/meter^2

