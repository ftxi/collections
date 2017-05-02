
using Luxor, Colors

Drawing(640, 640, "pattern.png")
origin()

pt1 = Point(0, 12)
pt2 = Point(25, 12)
pt3 = Point(25, 37)
pt4 = Point(0, 37)
pt5 = Point(9, 0)

function draw(size)
    size < 5 && return
    randomhue()
    poly([pt1, pt2, pt3, pt4], :fill)
    randomhue()
    poly([pt1, pt5, pt2], :fill)
    axes()
    textcentred(@sprintf "%d" size)
    gsave()
    rotate(atan2(4, -3))
    translate(pt5)
    scale(3/5, 3/5)
    draw(size*3/5)
    grestore()
    gsave()
    rotate(-atan2(3, 4))
    translate(pt2)
    scale(4/5, 4/5)
    draw(size*4/5)
    grestore()
end

scale(10, 10)
draw(10)
finish()
