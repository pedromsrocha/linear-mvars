# LVars: Linear MVars 

[Haskell MVars](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Concurrent-MVar.html) are the building blocks of several concurrent applications, but writing programs with them can cause deadlocks. 

Use Linear MVars (LVars) instead and all your programs are guaranteed to be deadlock-free by construction 😎

How does it work? Basically, by using linear types and exposing a safe API for LVars manipulation, with a controlled form of sharing.  Check details below 👇🏼

## Build instructions 

Get [Stack](https://docs.haskellstack.org/en/stable/), move to this project folder and input `stack test` in the terminal.

Stack will build the project and run some tests. If everything is ok (🤞) you should see the message 
> Test suite examples passed

## LVars API 

I'm assuming that you have a basic knowledge of Haskell's linear types (i.e., you know what a linear arrow  `% 1 ->` means) and the library [linear-base](https://github.com/tweag/linear-base).  

First things first: there are two methods:
```haskell 
newFull  :: a %1 -> Linear.IO (Full a)
newEmpty ::  Linear.IO (Empty a)
```
which allows us to create either a `Full` or `Empty` LVar. 

Here are the methods to take and put an LVar 
```haskell
takeL :: Full a %1 -> Linear.IO (a, Empty a)
putL  :: Empty a %1-> a %1 -> Linear.IO (Full a)
```

The operation `takeL` shifts an LVar from `Full` to `Empty`, whereas `putL` does the converse. The basic sequential usage of an LVar consists of a strict alternation between the operations `takeL` and `putL`. And when we're done, we can dispose the LVar

```haskell 
dispose:: Full a %1 -> Linear.IO
```

You can think of `dispose` as an effectul version of `consume`, as provided by the module `Data.Unrestriced.Linear` of the linear-base libary. In fact, [I've already asked](https://github.com/tweag/linear-base/issues/436) if there are plans of having something like `dispose` in future releases. 

Notice that we can only release an LVar in its full state. If the LVar is empty we must put, otherwise other concurrent take operations will be left hanging forever. 

Finally, to get interesting programs you must be able to share an LVar among concurrent threads, which can be done with the following method
```haskell 
share :: Full a % 1 -> (Full a %1 -> Linear.IO()) %1 -> Linear.IO (Full a, Ur ThreadId)
```
When invoked as `share c f`, it forks the thread  `f c` and returns a tuple containing `c` and the id of the forked thread. Hence, `c` is now shared between the forked thread `f c` and the thread that invoked `share c f`. 

There are two other overloaded `share` operations, which can be applied to an empty LVar 
```haskell 
share :: Empty a % 1 -> (Full a %1 -> Linear.IO()) %1 -> Linear.IO (Empty a, Ur ThreadId)
share :: Empty a %1 -> (Empty a %1 -> Linear.IO()) %1 -> Linear.IO (Full a, Ur ThreadId)
```

These two operations expose the LVar as empty to only one of the threads, which is now responsible to put back some value in the empty LVar. 

## Why do we get deadlock-freedom?

By using the LVars API in Linear Haskell your programs are guaranteed to be deadlock-free, this follows because of linear types and the safe API for LVars manipulation. 

More specifically, the linear type system disallows arbitrary LVars values from being copied and consumed, they can only be shared and disposed with the provided API methods `share` and `dispose`. 

The API guarantees that at most one LVar can be shared among two concurrent threads, guaranteeing that the sharing topologies are always acyclic. If we were allowed to share more than one LVar, we could easily obtain potentially-blocked programs such as 
```haskell
do (c1, c2, Ur _) <- share (c1,c2) (\c1, c2 -> do (v1, c1) <- takeL c1; 
                                                  (v2, c2) <- takeL c2; 
                                                   ...)
                           (v2, c2) <- takeL c2; 
                           (v1, c1) <- takeL c1; 
                           ...
```

The dipose operation can only be applied to a full LVar. In particular, the only possible operations on an empty LVar are `putL` and `share`. Therefore, when an LVar is taken the type system guarantees that a put operation will follow. Hence, potentially-blocked programs like the following 
```haskell
do (c, Ur _) <- share c dispose 
   (v, c) <- take c; 
   ...
```
are excluded.

## Examples 
TODO 

## Some References 
TODO 
