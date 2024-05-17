## (Bootlegged) React Compiler deep dive for ReactConf 2024

`runner` is an interactive compiler runner, which has the following features.

- reads real-time edits to `input.js`
- `debugger mode`, which lets a user step through events and passes compiler (code in `demo`) with source code highlighting.

```sh
# Install deps
yarn

# Build interactive runner
yarn build

# Start interactive runner
yarn start

# Print help for runner
yarn start help

#    s: step to next event in current pass
#    f: finish all event in current pass
#    n: next pass
#    r: reset pass/step state; show compiled output
```

`demo` is the core compiler logic and is a standalone babel plugin.
