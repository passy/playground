# Wat

This is supposed to be an experiment trying to break down the problems
I have with *ltxbot*. This means that I want to wrap an existing Monad
inside a Reader so I can access that one in my `act` function.

This should all work with a conduit. While contrived, the result should
be kinda useful.

## Algorithm

This should happen:

- Configure through a State Monad (like TW)
    - Words to be tracked
    - Initialize with count 0
- Read a file
- Split by words
- Count words
    - Fold over configured list of words
    - Save count in StateT

Once the above works:

- Wrap in ReaderT, configured with scoring
- Scoring transformer reads from ReaderT as opposed to StateT
