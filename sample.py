def f(x):
    x = x + 1
    y = x + 2

    return x + 1

class Enum(Colour):
    Red(Colour)
    Green(Colour)
    Blue(Colour)

macro(3)

@macro(enum)
class Colour:
    red
    green
    blue
    _end_

class Preprocessor:
    foo
    x = x + 1
