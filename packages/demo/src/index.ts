import * as Babel from "@babel/core";
import { codegenJS, print, readInstructions } from "./Transform";

export default {
  visitor: {
    FunctionDeclaration(babelFunc) {
      const instrs = readInstructions(babelFunc);
      print(instrs);

      const generatedBody = codegenJS(instrs);
      babelFunc
        .get("body")
        .replaceWith(Babel.types.blockStatement(generatedBody));
    },
  },
} as Babel.PluginObj;
