# Proving deadlocks #
We may prove that a program is not terminating by having the `C.run` function return a boolean value wrapped in a monad (rather than the unit type). The boolean value is `True` if the function terminates, i.e. it quits upon an empty action list, and `False` if it quits because it runs out of fuel (i.e. does not terminate). To check this boolean value in a proof, we set `IO = Identity`, i.e. we equate the `IO` monad with the `Identity` monad so we can use `runId` to retrieve the boolean value.

```
record Identity (a : Set) : Set where
  constructor Id
  field
    runId : a
open Identity public
```

However, we then run into the next issue.

## Postulating IO monad ##
Suppose we postulate `IO`. We can also postulate it as a monad instance, i.e. postulate `iMonadIO`. However, if we do this, Agda will not be able to normalize the bind and return operations of expressions involving the `IO` monad. Similarly for `IORef` and its operations.
