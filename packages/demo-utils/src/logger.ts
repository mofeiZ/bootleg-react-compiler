import * as t from "@babel/types";
import chalk from "chalk";
import invariant from "invariant";
// Very hacky: we want to demo a babel plugin but we also want
// some way of interactively stepping through the input to log
// some debugging info.
// So...
// We'll set 'breakpoints' via cli and just throw an error.
// Relying on compilation being very fast to make this work.
let breakpoint: StepState | null = null;
let state: LoggerState = {
  src: null,
  pass: 0,
  step: 0,
};
const output: Array<string> = [];
export type StepState = {
  pass: number;
  step: number;
};
export type SourceLocation = {
  start: {
    line: number;
    column: number;
  };
  end: {
    line: number;
    column: number;
  };
};
export type YieldInfo = {
  // (optional) source code to display
  src: string | null;
  // (optional) loc in src to highlight
  srcLoc: SourceLocation | null;
  // logs that have been 'yielded' so far
  output: string;
  passDesc: string | number;
};
type YieldedInfoWrapper = YieldInfo & {
  __hackyAttr: true;
};
type LoggerState = StepState & {
  src: string | null;
};

function makeYieldError(
  src: string | null,
  srcLoc: SourceLocation | null,
  passDesc: string | number,
  logs: string,
): YieldedInfoWrapper {
  return {
    src,
    srcLoc,
    output: logs,
    passDesc,
    __hackyAttr: true,
  };
}
export function isYield(o: object): o is YieldedInfoWrapper {
  return o != null && "__hackyAttr" in o && o.__hackyAttr === true;
}

export function setLoggerBreakpoint(newBreakpoint: StepState | null) {
  breakpoint = newBreakpoint;
  state.src = null;
  state.pass = -1;
  state.step = 0;
  output.length = 0;
}

export function collectLogs() {
  return makeYieldError(
    state.src,
    null,
    state.pass,
    output.join("\n") + "\n" + chalk.dim(`( done )`),
  );
}
export function yieldPass(newSrc?: string) {
  if (breakpoint == null) {
    return;
  }
  if (state.pass >= breakpoint.pass) {
    // console.log("logPass: past breakpoint pass=", breakpoint.pass, state.pass);
    throw collectLogs();
  }
  state.pass++;
  state.step = 0;
  // console.log("logPass", `${breakpoint.pass}:${breakpoint.step}`, `${state.pass}:${state.step}`);
  state.src = newSrc ?? null;
  output.length = 0;
}

export function yieldLog(
  msg: string,
  loc?: t.SourceLocation | null | undefined | number,
) {
  if (breakpoint == null) {
    return;
  } else if (state.pass === breakpoint.pass) {
    if (state.step === breakpoint.step) {
      let transformedLoc: SourceLocation | null;
      if (typeof loc === "number") {
        transformedLoc = {
          start: {
            line: loc + 1,
            column: 0,
          },
          end: {
            line: loc + 1,
            column: 1e6,
          },
        };
      } else {
        transformedLoc = loc ?? null;
      }
      output.push(msg);
      throw makeYieldError(
        state.src,
        transformedLoc,
        state.pass,
        output.join("\n") + "\n",
      );
    } else if (state.step < breakpoint.step) {
      output.push(msg);
      state.step++;
    } else {
      invariant(false, "unexpected past");
    }
  }
}
