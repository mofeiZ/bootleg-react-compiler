import * as Babel from "@babel/core";

export default {
  visitor: {
    FunctionDeclaration(babelFunc) {
      console.log("found", babelFunc.node.id?.name);
    }
  },
} as Babel.PluginObj;
