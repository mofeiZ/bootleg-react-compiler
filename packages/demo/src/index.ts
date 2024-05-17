import * as Babel from "@babel/core";
import { print, readInstructions } from "./Transform";

export default {
  visitor: {
    FunctionDeclaration(babelFunc) {
      const instrs = readInstructions(babelFunc);
      print(instrs);
    },
  },
} as Babel.PluginObj;
