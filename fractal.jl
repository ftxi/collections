
convert_point(p::Vector, m::Matrix) = (([p, 1]*m)/m[3,3])[1:2]

M(s) = Diagonal([1, 1, 1/s]);

V(Θ) = [cos(Θ)  -sin(Θ) 0;
        sin(Θ)  cos(Θ)  0;
        0       0       1]

P(Δx, Δy) =[1   0   Δx;
            0   1   Δy;
            0   0   1]

size = 300;
models = Set{Matrix}();

function create_image(mvp::Matrix)
    mvp[3, 3] > size && return
    for u in [M(1/2)*V(π/6)*P(0.2, 0.2)]
        create_image(mvp*u)
    end
    push!(models, mvp)
end

function draw()
    for m in models
        
    end
end
