
# Sample Programs from Martin Grabmuller - Monad Transformers Step by Step

http://catamorph.de/documents/Transformers.pdf

The progression of the examples from the paper goes as follows:

  - BasicEvaluator.hs
  - BasicEvaluator.hs
  - MonadicEvaluator.hs
  - GracefulEvaluator.hs
  - EnvironmentDependentEvaluator.hs
  - ProfilingEvaluator.hs
  - LoggingEvaluator.hs
  - IOEvaluator.hs

These samples are based on the paper but refactored a bit for readability, etc.

# Monad Transformer Cheat Sheet

ExceptT - a variant of EitherT - has an inner Either

```haskell
    newtype ExceptT error stackedMonad value = ExceptT (stackedMonad (Either error value))
    runExceptT :: ExceptT error stackedMonad value -> stackedMonad (Either error value)
```

If runM is the extractor for the stacked monad:

```haskell
    runM . runExceptT :: ExceptT error stackedMonad value -> Either error value
```

ReaderT - has an inner monad producer that creates a stacked monad based on the input

```haskell
    newtype ReaderT input stackedMonad value = ReaderT { runReaderT :: input -> stackedMonad value }
    runReaderT ReaderT input stackedMonad value -> (input -> stackedMonad value)
```

StateT - 

```haskell
    newtype StateT state stackedMonad value = StateT { runStateT :: state -> stackedMonad (value, state) }
    runStateT :: StateT state stackedMonad value -> state -> stackedMonad (value, state)
```

WriterT -

```haskell
    newtype WriterT output stackedMonad value = WriterT { runWriterT :: stackedMonad (value, output) }
    runWriterT :: WriterT output stackedMonad value -> stackedMonad(value, output)
```

# Other resources:

http://blog.jakubarnold.cz/2014/07/22/building-monad-transformers-part-1.html
https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md
https://ocharles.org.uk/blog/posts/2016-01-26-transformers-free-monads-mtl-laws.html

