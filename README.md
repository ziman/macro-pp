# macro-pp
Universal preprocessor

1. Write your macro matcher/parser with Parsec
2. Implement conversion of parsed macros to whatever you want
3. Non-matching portions of the input are copied verbatim

## Example

`ExamplePython.hs` defines a *mypy*-supported enum for Python.
This takes the following code...

```python
@enum(deriving=Codec)
class Model:
    PreorderMaximization(p = PreorderParams, q = bool)
    UndominatedChoice(strict = bool)
    TopTwo
    ChooseSome(models = List[Model])
    pass
```

...and generates the following Python source.

```python
class PreorderMaximization(NamedTuple):
    p : PreorderParams
    q : bool
    tag : int = 0
    
class UndominatedChoice(NamedTuple):
    strict : bool
    tag : int = 1
    
class TopTwo(NamedTuple):
    tag : int = 2
    
class ChooseSome(NamedTuple):
    models : List[Model]
    tag : int = 3
    
Model = Union[
    PreorderMaximization,
    UndominatedChoice,
    TopTwo,
    ChooseSome,
]

ModelC = enumC('Model', {
    PreorderMaximization: (PreorderParamsC, boolC,),
    UndominatedChoice: (boolC,),
    TopTwo: (),
    ChooseSome: (listC(ModelC),),
})
```
