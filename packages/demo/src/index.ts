import * as Babel from "@babel/core";
import {
  InstrId,
  Instruction,
  eachValue,
} from "./LoweredJavaScript";
import {
  ValueInfo,
  codegenJS,
  print,
  readInstructions,
} from "./Transform";
import { understandAliasing } from "./UnderstandMutability";

function getValuesThatMayChange(
  func: Map<InstrId, Instruction>,
): Set<InstrId> {
  const mayChange = new Set<InstrId>();
  for (const [id, instr] of func) {
    if (instr.kind === "Param") {
      mayChange.add(id);
    } else {
      for (const used of eachValue(instr)) {
        if (mayChange.has(used)) {
          mayChange.add(id);
        }
      }
    }
  }
  return mayChange;
}

function analyze(
  func: Map<InstrId, Instruction>,
): Map<InstrId, ValueInfo> {
  const result = new Map<InstrId, ValueInfo>();
  // Understand dependencies and what values should be memoized
  const mayChange = getValuesThatMayChange(func);
  for (const [id, instr] of func) {
    const dependencies = new Set<number>();
    for (const used of eachValue(instr)) {
      if (mayChange.has(used)) {
        dependencies.add(used);
      }
    }
    result.set(id, {
      shouldMemo: ["Call", "Object"].includes(
        instr.kind,
      ),
      dependencies,
    });
  }
  print(func, result);

  // Understand writing to values produced by previous instructions
  const writableValues = getWritableValues(func);
  for (const [id, instr] of func) {
    if (instr.kind === "Call") {
      for (const used of eachValue(instr)) {
        if (writableValues.has(used)) {
          // This is a maybe-write operation, so record it as such
          result.get(used)!.instructions ??=
            new Set();
          result.get(used)!.instructions?.add(id);
        }
      }
    }
  }
  print(func, result);

  return result;
}
function getWritableValues(
  func: Map<InstrId, Instruction>,
): Set<InstrId> {
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

export default {
  visitor: {
    FunctionDeclaration(babelFunc) {
      const instrs = readInstructions(babelFunc);

      const infos = analyze(instrs);
      understandAliasing(instrs, infos);

      const body = codegenJS(instrs, infos);

      babelFunc
        .get("body")
        .replaceWith(
          Babel.types.blockStatement(body),
        );
    },
  },
} as Babel.PluginObj;
