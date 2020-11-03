![Travis CI Badge](https://img.shields.io/travis/typeable/validationt)

# ValidationT

A simple data validation library. The main idea is to provide an easy way to
validate web form data by aggregating errors for each field.

## Usage

Suppose you want to validate some data from the user. Say, that the password adheres to some rules.

You might do something like this:

```haskell
validatePassword :: Monad m => String -> ValidationT [String] m ()
validatePassword password = do
  vLength password
  vAlpha password
  vNum password
  where
    vLength p = when (length p < 8)
      $ vWarning ["The password should be at least 8 characters long."]
    vAlpha p = unless (any isAlpha p)
      $ vWarning ["The password should contain at least one alphabetic character."]
    vNum p = unless (any isDigit p)
      $ vWarning ["The password should contain at least one numeric character."]
```

`ValidationT e m a` essentially just gathers the errors thrown by `vWarning`.

```haskell
vWarning :: (Monad m, Monoid e) => e -> ValidationT e m ()
```

`vWarning` `mappend`s the given `e` to the already collected errors. This is why the warnings are contained inside a list.

There is also `vError`. The only difference between `vWarning` and `vError` is that `vError` stops further execution (and further collection of errors and warnings).

You would use the validation like this:

```haskell
main :: IO ()
main = do
  password <- getLine
  result <- runValidationTEither . validatePassword $ password
  putStrLn $ case result of
    Left errs -> unlines err
    Right () -> "You are fine."
```

You could, of course, do more complicated things like use `vWarningL` and `vErrorL` to add an error to a `mempty` structure, which gets `mappend`ed with other errors.

The library comes with a `MonoidMap` `newtype` wrapper around `Map`, which `mappend`s the values themselves on conflict. This can be useful if you have a multiple points of failure and you want to distinguich between them -- validating a username and a password for example:

```haskell
data Piece
  = Password
  | UserName
  deriving (Eq, Show, Ord)

validatePassword :: Monad m => String -> ValidationT (MonoidMap Piece [String]) m ()
validatePassword password = do
  vLength password
  vAlpha password
  vNum password
  where
    warning = mmSingleton UserName
    vLength p = when (length p < 8)
      $ warning ["The password should be at least 8 characters long."]
    vAlpha p = unless (any isAlpha p)
      $ warning ["The password should contain at least one alphabetic character."]
    vNum p = unless (any isDigit p)
      $ warning ["The password should contain at least one numeric character."]
```

(`mmSingleton` is a covenience initializer for `MonoidMap`.)
