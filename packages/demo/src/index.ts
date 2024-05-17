import * as Babel from "@babel/core";
import { ValueInfo, codegenJS, print, readInstructions } from "./Transform";
import { InstrId, Instruction } from "./LoweredJavaScript";

function analyze(func: Map<InstrId, Instruction>): Map<InstrId, ValueInfo> {
  const result = new Map<InstrId, ValueInfo>();
  for (const [id, instr] of func) {
    // TODO
    result.set(id, {
      shouldMemo: true,
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
