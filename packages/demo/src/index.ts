import * as Babel from "@babel/core";
import { ValueInfo, codegenJS, print, readInstructions } from "./Transform";
import { InstrId, Instruction, eachValue } from "./LoweredJavaScript";
import { isHookCall, understandAliasing } from "./UnderstandMutability";

function getValuesThatMayChange(func: Map<InstrId, Instruction>): Set<InstrId> {
  const mayChange = new Set<InstrId>();
  for (const [id, instr] of func) {
    if (instr.kind === "Param" || isHookCall(instr, func)) {
      // Parameters and values returned from hooks might change
      mayChange.add(id);
    } else {
      // As can anything calculated off of them
      for (const used of eachValue(instr)) {
        if (mayChange.has(used)) {
          mayChange.add(id);
        }
      }
    }
  }
  return mayChange;
}

// Creating a new object (directly or by calling a function)
// creates a writable value.
// (Note that primitives cannot be modified after being created,
// and neither can params or hook return results)
//
// Note: the result of a hook calls themselves are not writable
// but... calls or objects that use those values might be
// So let's be conservative and say that the results of hook
// calls are writable
function getWritableValues(func: Map<InstrId, Instruction>): Set<InstrId> {
  const maybeWritable = new Set<InstrId>();
  for (const [id, instr] of func) {
    if (["Object", "Call"].includes(instr.kind)) {
      maybeWritable.add(id);
    } else {
      for (const used of eachValue(instr)) {
        if (maybeWritable.has(used)) {
          maybeWritable.add(id);
        }
      }
    }
  }
  return maybeWritable;
}

function analyze(func: Map<InstrId, Instruction>): Map<InstrId, ValueInfo> {
  const result = new Map<InstrId, ValueInfo>();
  // Step 1: Understand which instructions should be memoized
  // and their dependencies
  const mayChange = getValuesThatMayChange(func);
  for (const [id, instr] of func) {
    const dependencies = new Set<number>();
    // Maybe every value an instruction uses should be counted
    // as a dependency?
    for (const used of eachValue(instr)) {
      if (mayChange.has(used)) {
        dependencies.add(used);
      }
    }

    // Only call or object instructions can be expensive or create
    // new objects. Other instructions in our limited set are cheap
    // and read existing values.
    result.set(id, {
      shouldMemo: instr.kind === "Call" || instr.kind === "Object",
      dependencies,
    });
  }
  print(func, result);

  // Step 2: Understand writes!
  // Some instructions might write to (and modify) a value created
  // by an earlier instruction
  const writableValues = getWritableValues(func);
  for (const [id, instr] of func) {
    if (instr.kind === "Call") {
      for (const used of eachValue(instr)) {
        if (writableValues.has(used)) {
          // (Terrible syntax, this creates a new set if instructions
          //  doesn't exist, then add this instruction to it)
          result.get(used)!.instructions ??= new Set();
          result.get(used)!.instructions!.add(id);
        }
      }
    }
  }
  print(func, result);
  return result;
}

export default {
  visitor: {
    FunctionDeclaration(babelFunc) {
      const instrs = readInstructions(babelFunc);
      print(instrs);

      const info = analyze(instrs);
      understandAliasing(instrs, info);

      const generatedBody = codegenJS(instrs, info);
      babelFunc
        .get("body")
        .replaceWith(Babel.types.blockStatement(generatedBody));
    },
  },
} as Babel.PluginObj;
