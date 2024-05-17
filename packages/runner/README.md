This is a very hacky compiler runner / stepper.

The core logic uses typescript's compiler in watch mode and attempts to model the compiler "yielding" information to be printed. This is actually implemented with custom `throw(...)` and throw handlers which keep track of runner step state.