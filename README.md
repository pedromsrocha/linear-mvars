# LVars: Linear MVars 

[Haskell MVars]() are the building blocks of several concurrent applications, but writing programs with them can cause deadlocks. 

Use Linear MVars (LVars) instead and all your programs are guaranteed to be deadlock-free by construction 😎

How does it work? Linear types, a restricted form of sharing, and some other ingredients. Check details below 👇🏼

## Build instructions 

Get [Stack](https://docs.haskellstack.org/en/stable/), move to the project folder and input `stack test` in the terminal.

Stack will build the project and run some tests. If everything is ok (🤞) you should see the message 
> Test suite examples passed

## How does it work?

I'm assuming that you have a basic knowledge of Haskell's linear types and the library linear-base. If you don't, please check here. 

First things first: let's see the API exposed by the `LVars` module. There are two methods 

```haskell 
newFull  :: a %1 -> Linear.IO (Full a)

newEmpty ::  Linear.IO (Empty a)
```

which allows us to create either a `Full` or `Empty` LVar. 

Standard Haskell MVars do not distinguish, at the type-level, between a full or empty MVar. However, this distinction is essential if we are aiming for deadlock-freedom. 

Here are the methods to take and put an LVar 
```haskell
takeL :: Full a %1 -> Linear.IO (a, Empty a)
putL  :: Empty a %1-> a %1 -> Linear.IO (Full a)
```

The operation `takeL` shifts an LVar from `Full` to `Empty`, whereas `putL` does the converse. The basic sequential usage of an LVar consists of a strict alternation between the operations `takeL` and `putL`. And when we're done, we can dispose the LVar

```haskell 
class Disposable a where
  dispose :: a %1 -> Linear.IO ()

instance Disposable a => Disposable (Full a) where
    --dispose:: Full a %1 -> Linear.IO
```

You can think of `dispose` as an effectul version of `consume`, as provided by the module `Data.Unrestriced.Linear` of the linear-base libary. In fact, [I've already asked them](https://github.com/tweag/linear-base/issues/436) if there are plans of having something like `dispose` in future releases. 

Notice that we can only release an LVar in its full state, if the LVar is empty we must put, otherwise other concurrent take operations will be left hanging forever, which would cause dealdock. 

[TODO: continue here]

