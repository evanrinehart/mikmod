[Haskell][1] bindings to [MikMod][2], an Open Source, multi-platform library
for playback of tracker music and sound effects.

For more information about using this library see the haddock API documentation.

```haskell
import Control.Concurrent (threadDelay)
import Control.Monad.Loops (whileM_)
import Sound.MikMod

main = do
  mikmodSetup 16
  tape <- playerLoad "rockin.mod" 64 NotCurious
  playerStart tape
  whileM_ playerActive $ do
    mikmodUpdate
    threadDelay 10000
  playerFree tape
  mikmodExit
```

[1]: http://www.haskell.org/
[2]: http://mikmod.sourceforge.net/
