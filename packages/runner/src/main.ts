/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import * as BabelCore from "@babel/core";
import watcher from "@parcel/watcher";
import chalk from "chalk";

import { StepState } from "demo-utils";
import { emphasize } from "emphasize";
import fs from "fs/promises";
import { createRequire } from "module";
import path from "path";
import process from "process";
import * as readline from "readline";
import {
  CompileResult,
  clearRequireCache,
  compile,
  watchSrc,
} from "./utils.js";
const require = createRequire(import.meta.url);

const ROOT_PATH = path.dirname(path.dirname(process.cwd()));
const INPUT_PATH = path.join(ROOT_PATH, "input.js");
const COMPILER_PATH = path.join(ROOT_PATH, "packages", "demo");
const COMPILED_PATH = path.join("packages", "demo", "dist");
const COMPILED_MAINFILE = path.join(ROOT_PATH, COMPILED_PATH, "index.js");

function clearConsole() {
  // console.log("\u001Bc");
  console.clear();
}

const DEFAULT_STEPSTATE = {
  pass: 0,
  step: 0,
};

type CompilerWatchState = {
  // Monotonically increasing integer which determines whether we need to
  // clear the require cache (due to compiler source code changing)
  version: number;
  isValid: boolean;
  stepState: StepState | null;
};

function logResult(result: CompileResult) {
  if (result.kind === "Yielded" && result.srcLoc != null) {
    const lines = result.src.split("\n");
    const { start, end } = result.srcLoc;
    const highlighted = lines.map((line, idx_) => {
      const idx = idx_ + 1;
      let before = "";
      let content = "";
      let after = "";
      if (idx == start.line && idx == end.line) {
        before = line.substring(0, start.column);
        content = line.substring(start.column, end.column);
        after = line.substring(end.column);
      } else if (idx == start.line) {
        before = line.substring(0, start.column);
        content = line.substring(start.column);
      } else if (idx == end.line) {
        content = line.substring(0, end.column);
        after = line.substring(end.column);
      } else if (idx > start.line && idx < end.line) {
        content = line;
      } else {
        before = line;
      }
      return chalk.dim(before) + chalk.bgGreen(content) + chalk.dim(after);
    });
    console.log(highlighted.join("\n"));
    console.log(`------- Logs (pass ${result.passDesc}) ---------`);
    console.log(result.output);
  } else if (result.kind === "Ok") {
    console.log(chalk.dim(result.src));
    console.log("------- Output -------");
    console.log(
      emphasize.highlight(
        "jsx",
        result.code,
        // {
        //   'keyword': chalk.magenta.dim,
        //   'number': chalk.green,
        //   'literal': chalk.red,
        //   'property': chalk.blue,
        // }
      ).value,
    );
  } else if (result.kind === "Yielded") {
    console.log(chalk.dim(result.src));
    console.log(`------- Logs (pass ${result.passDesc}) ---------`);
    console.log(result.output);
  } else {
    console.log(chalk.red(result?.msg ?? result));
  }
}

let currentVersion: number | null = null;
// Callback to re-run things after some change
async function onChange(state: CompilerWatchState) {
  if (state.isValid) {
    clearConsole();
    if (currentVersion !== state.version) {
      clearRequireCache(COMPILED_PATH);
      currentVersion = state.version;
    }
    let input: string;
    try {
      input = await fs.readFile(INPUT_PATH, "utf8");
    } catch (e) {
      console.log("couldn't read input");
      return;
    }
    let compilerPlugin: BabelCore.PluginObj<any>;
    try {
      compilerPlugin = require(COMPILED_MAINFILE);
    } catch (e) {
      console.log("couldn't read compiled output.", e);
      return;
    }

    const result = await compile(
      input,
      INPUT_PATH,
      state.stepState,
      compilerPlugin,
    );

    logResult(result);
    console.log(chalk.dim("Waiting for changes..."));
  } else {
    console.error(
      "Found errors in compiler source code, skipping compilation.",
    );
  }
}

/**
 * Runs the compiler in watch or single-execution mode
 */
export async function main(): Promise<void> {
  let state: CompilerWatchState = {
    version: 0,
    isValid: false,
    stepState: null,
  };

  // Run TS in incremental watch mode
  watchSrc(
    COMPILER_PATH,
    () => console.log("\nCompiling..."),
    (isSuccess) => {
      // TODO: should we always reset currentStep on change?
      state = {
        version: isSuccess ? state.version + 1 : state.version,
        isValid: isSuccess,
        stepState: null,
      };
      onChange(state);
    },
  );

  // Watch the input filepath for changes
  watcher.subscribe(path.dirname(INPUT_PATH), async (err, events) => {
    if (err) {
      console.error(err);
      process.exit(1);
    } else if (events.findIndex((event) => event.path === INPUT_PATH) !== -1) {
      state = {
        ...state,
        stepState: null,
      };
      // Input changed, re-run everything
      onChange(state);
    }
  });

  // Basic key event handling
  process.stdin.on("keypress", (_str, key) => {
    // TODO: add more cli stuff here
    if (key.name >= "0" && key.name <= "9") {
      state = {
        ...state,
        stepState: {
          step: 1e6,
          pass: parseInt(key.name),
        },
      };
      onChange(state);
      return;
    }
    switch (key.name) {
      case "q": {
        process.exit(0);
      }
      case "r": {
        // r => reset
        state = {
          ...state,
          stepState: null,
        };
        onChange(state);
        break;
      }
      case "s": {
        // s: next step
        state = {
          ...state,
          stepState:
            state.stepState == null
              ? DEFAULT_STEPSTATE
              : {
                  step: state.stepState.step + 1,
                  pass: state.stepState.pass,
                },
        };
        onChange(state);
        break;
      }
      case "f": {
        // f: finish pass
        state = {
          ...state,
          stepState:
            state.stepState == null
              ? DEFAULT_STEPSTATE
              : {
                  step: 1e6,
                  pass: state.stepState.pass,
                },
        };
        onChange(state);
        break;
      }
      case "backspace": {
        // backspace: rewind pass
        state = {
          ...state,
          stepState:
            state.stepState == null
              ? DEFAULT_STEPSTATE
              : {
                  step: 0,
                  pass: state.stepState.pass,
                },
        };
        onChange(state);
        break;
      }
      case "n":
      case "right": {
        // -> next pass
        state = {
          ...state,
          stepState:
            state.stepState == null
              ? DEFAULT_STEPSTATE
              : {
                  step: 0,
                  pass: state.stepState.pass + 1,
                },
        };
        onChange(state);
        break;
      }
      case "left": {
        // <- prev pass
        state = {
          ...state,
          stepState: state.stepState?.pass
            ? {
                step: 0,
                pass: state.stepState.pass - 1,
              }
            : null,
        };
        onChange(state);
        break;
      }
    }
  });
}

// main script
readline.emitKeypressEvents(process.stdin);
if (process.stdin.isTTY) {
  process.stdin.setRawMode(true);
}

process.stdin.on("keypress", function (_, key) {
  if (key && key.name === "c" && key.ctrl) {
    // handle sigint
    process.exit(-1);
  }
});

main().catch((error) => console.error(error));
