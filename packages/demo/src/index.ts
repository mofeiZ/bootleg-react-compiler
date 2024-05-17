import * as Babel from "@babel/core";
import { ValueInfo, codegenJS, print, readInstructions } from "./Transform";
import { InstrId, Instruction, eachValue } from "./LoweredJavaScript";
import { isHookCall } from "./UnderstandMutability";

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

function analyze(func: Map<InstrId, Instruction>): Map<InstrId, ValueInfo> {
  const result = new Map<InstrId, ValueInfo>();
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
  return result;
}

export default {
  visitor: {
    FunctionDeclaration(babelFunc) {
      const instrs = readInstructions(babelFunc);
      print(instrs);

      const info = analyze(instrs);

      const generatedBody = codegenJS(instrs, info);
      babelFunc
        .get("body")
        .replaceWith(Babel.types.blockStatement(generatedBody));
    },
  },
} as Babel.PluginObj;
