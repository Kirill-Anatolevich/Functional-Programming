class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __str__(self):
        return f"x = {self.x};\ny = {self.y};\n"

class Line(Point):
    def __init__(self, a, b):
        self.point1 = a
        self.point2 = b
        self.k = (b.y - a.y)/(b.x - a.x)
        self.b = a.y - (b.y - a.y)/(b.x - a.x)*a.x

    def __str__(self):
        return f"A {{\n{self.point1}}};\nB {{\n{self.point2}}};\nk = {self.k};\nb = {self.b};\n"

def line_intersections(line1, line2):
    x = (line2.b - line1.b) / (line1.k - line2.k)
    if (min(line1.point1.x,line1.point2.x) <= x <= max(line1.point1.x,line1.point2.x) and 
            min(line2.point1.x,line2.point1.x) <= max(line2.point1.x,line2.point1.x)):
        print("OK")
    else:
        print("NO")


a = Point(-0.5, 0)
b = Point(0, 2)
l = Line(a, b)
a1 = Point(-1, -1)
b1 = Point(1, 2)
l1= Line(a1, b1)
print(l)
print(l1)
line_intersections(l, l1)
