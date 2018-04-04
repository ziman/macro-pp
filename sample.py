def f(x):
    x = x + 1
    y = x + 2

    return x + 1

@enum
class Model:
    PreorderMaximization(p = PreorderParams, q = bool)
    UndominatedChoice(strict = bool)
    TopTwo
    ChooseSome(models = List[Model])
    pass

class Preprocessor:
    foo
    x = x + 1
