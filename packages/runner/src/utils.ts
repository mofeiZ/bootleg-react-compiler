import * as BabelCore from "@babel/core";
import { transformFromAstSync } from "@babel/core";
import * as BabelParser from "@babel/parser";
import type { StepState } from "demo-utils";
import { setLoggerBreakpoint } from "demo-utils";
import ts from "typescript";

import { SourceLocation, collectLogs, isYield } from "demo-utils";
import invariant from "invariant";
import path from "path";
import prettier from "prettier";

import { createRequire } from "module";
const require = createRequire(import.meta.url);

export function watchSrc(
  srcPath: string,
  onStart: () => void,
  onComplete: (isSuccess: boolean) => void,
): ts.WatchOfConfigFile<ts.SemanticDiagnosticsBuilderProgram> {
  const configPath = ts.findConfigFile(
    /*searchPath*/ srcPath,
    ts.sys.fileExists,
    "tsconfig.json",
  );
  if (!configPath) {
    throw new Error("Could not find a valid 'tsconfig.json'.");
  }
  const createProgram = ts.createSemanticDiagnosticsBuilderProgram;
  const host = ts.createWatchCompilerHost(
    configPath,
    {},
    ts.sys,
    createProgram,
    () => {}, // we manually report errors in afterProgramCreate
    () => {}, // we manually report watch status
  );

  const origCreateProgram = host.createProgram;
  host.createProgram = (rootNames, options, host, oldProgram) => {
    onStart();
    return origCreateProgram(rootNames, options, host, oldProgram);
  };
  const origPostProgramCreate = host.afterProgramCreate;
  host.afterProgramCreate = (program) => {
    origPostProgramCreate!(program);

    // syntactic diagnostics refer to javascript syntax
    const errors = program
      .getSyntacticDiagnostics()
      .filter((diag) => diag.category === ts.DiagnosticCategory.Error);
    // semantic diagnostics refer to typescript semantics
    errors.push(
      ...program
        .getSemanticDiagnostics()
        .filter((diag) => diag.category === ts.DiagnosticCategory.Error),
    );

    if (errors.length > 0) {
      for (const diagnostic of errors) {
        let fileLoc: string;
        if (diagnostic.file) {
          // https://github.com/microsoft/TypeScript/blob/ddd5084659c423f4003d2176e12d879b6a5bcf30/src/compiler/program.ts#L663-L674
          const { line, character } = ts.getLineAndCharacterOfPosition(
            diagnostic.file,
            diagnostic.start!,
          );
          const fileName = path.relative(
            ts.sys.getCurrentDirectory(),
            diagnostic.file.fileName,
          );
          fileLoc = `${fileName}:${line + 1}:${character + 1} - `;
        } else {
          fileLoc = "";
        }
        console.error(
          `${fileLoc}error TS${diagnostic.code}:`,
          ts.flattenDiagnosticMessageText(diagnostic.messageText, "\n"),
        );
      }
      console.error(
        `Compilation failed (${errors.length} ${
          errors.length > 1 ? "errors" : "error"
        }).\n`,
      );
    }

    const isSuccess = errors.length === 0;
    onComplete(isSuccess);
  };

  // `createWatchProgram` creates an initial program, watches files, and updates
  // the program over time.
  return ts.createWatchProgram(host);
}

export function clearRequireCache(match: string) {
  Object.keys(require.cache).forEach(function (path) {
    if (path.includes(match)) {
      delete require.cache[path];
    }
  });
}

export type CompileResult =
  | {
      kind: "Ok";
      src: string;
      code: string;
    }
  | {
      kind: "Yielded";
      src: string;
      srcLoc: SourceLocation | null;
      passDesc: number | string;
      output: string;
    }
  | {
      kind: "Error";
      msg: string;
    };

export async function compile(
  text: string,
  filename: string,
  stepNumber: StepState | null,
  compilerPlugin: BabelCore.PluginObj<any>,
): Promise<CompileResult> {
  try {
    const ast = BabelParser.parse(text, {
      sourceFilename: filename,
      plugins: ["typescript", "jsx"],
      sourceType: "module",
    });
    setLoggerBreakpoint(stepNumber);
    const result = transformFromAstSync(ast, text, {
      filename: filename,
      highlightCode: false,
      retainLines: true,
      plugins: [[compilerPlugin, {}]],
      sourceType: "module",
    });
    // final yield to print "done" on last pass
    if (stepNumber != null) {
      throw collectLogs();
    }
    invariant(result?.code != null, "Internal error: expected code!");

    const formattedCode = await prettier.format(result.code, {
      semi: true,
      parser: "babel-ts",
      printWidth: 40,
    });
    return {
      kind: "Ok",
      src: text,
      code: formattedCode,
    };
  } catch (e) {
    if (typeof e === "object" && e != null && isYield(e)) {
      return {
        kind: "Yielded",
        src: e.src ?? text,
        srcLoc: e.srcLoc,
        output: e.output,
        passDesc: e.passDesc,
      };
    } else {
      return {
        kind: "Error",
        msg: (e as any)?.toString(),
      };
    }
  }
}
