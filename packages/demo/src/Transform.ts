import { NodePath, types as t } from "@babel/core";
import generate from "@babel/generator";
import assert from "assert";
import chalk from "chalk";
import {
  readCallExpression,
  readMemberExpression,
  readObjectExpression,
  readVariableDeclaration,
  yieldLog,
  yieldPass,
} from "demo-utils";
import DisjointSet from "demo-utils/dist/DisjointSet";
import invariant from "invariant";
import {
  FunctionBody,
  InstrId,
  Instruction,
  LoadConstant,
  MemberExpression,
  MethodCall,
  ObjectExpression,
  Param,
  Value,
  eachValue,
  printFunc,
  printInstr,
  printValueId,
} from "./LoweredJavaScript";

function assertsValid(
  cond: unknown,
  msg: string,
): asserts cond {
  if (!cond) {
    throw new Error("Invalid syntax. " + msg ?? "");
  }
}

export function logInstr(func: FunctionBody, id: number) {
  const instr = func.get(id)!;
  yieldLog(printInstr(instr, id), instr.loc);
}

function getDeclaration(
  func: FunctionBody,
  variableName: string,
): InstrId | null {
  for (const [id, instr] of func.entries()) {
    if (
      instr.kind === "Declare" &&
      instr.lhs === variableName
    ) {
      return id;
    } else if (
      instr.kind === "Param" &&
      instr.name === variableName
    ) {
      return id;
    }
  }
  return null;
}

export function lowerBabelInstr(
  instr: NodePath<t.Node>,
  state: FunctionBody,
): InstrId {
  if (instr.isMemberExpression()) {
    const { object, property } =
      readMemberExpression(instr);
    const objectValue = lowerBabelInstr(object, state);
    const currValue = state.size;
    state.set(currValue, {
      kind: "ReadProperty",
      object: objectValue,
      property,
      loc: instr.node.loc!,
    });
    logInstr(state, currValue);
    return currValue;
  } else if (instr.isObjectExpression()) {
    const properties = readObjectExpression(instr);
    const propertyValues: Map<string, Value> = new Map();
    for (const [key, value] of properties) {
      let loweredValue: Value;
      if (
        value.isStringLiteral() ||
        value.isNumericLiteral()
      ) {
        loweredValue = {
          kind: "Literal",
          value: value.node.value,
        };
      } else {
        loweredValue = {
          kind: "RValue",
          value: lowerBabelInstr(value, state),
        };
      }
      propertyValues.set(key.node.name, loweredValue);
    }
    const currValue = state.size;
    state.set(currValue, {
      kind: "Object",
      properties: propertyValues,
      loc: instr.node.loc!,
    });
    logInstr(state, currValue);
    return currValue;
  } else if (instr.isCallExpression()) {
    const { callee, args } = readCallExpression(instr);
    let receiverValue: InstrId;
    let propertyName: string | null;
    if (callee.isMemberExpression()) {
      const { object, property } =
        readMemberExpression(callee);
      assertsValid(
        typeof property === "string",
        `Unsupported non-string call property ${property}`,
      );
      receiverValue = lowerBabelInstr(object, state);
      propertyName = property;
    } else {
      receiverValue = lowerBabelInstr(callee, state);
      propertyName = null;
    }
    const argumentValues: Array<Value> = [];
    for (const arg of args) {
      if (arg.isNumericLiteral()) {
        argumentValues.push({
          kind: "Literal",
          value: arg.node.value,
        });
      } else {
        argumentValues.push({
          kind: "RValue",
          value: lowerBabelInstr(arg, state),
        });
      }
    }
    const currValue = state.size;
    state.set(currValue, {
      kind: "Call",
      receiver: receiverValue,
      property: propertyName,
      arguments: argumentValues,
      loc: instr.node.loc!,
    });
    logInstr(state, currValue);
    return currValue;
  } else if (instr.isReturnStatement()) {
    const argument = instr.get("argument");
    let argumentValue;
    if (argument.node != null) {
      argumentValue = lowerBabelInstr(
        argument as NodePath<t.Expression>,
        state,
      );
    } else {
      argumentValue = null;
    }
    const currValue = state.size;
    state.set(currValue, {
      kind: "Return",
      value: argumentValue,
      loc: instr.node.loc!,
    });
    logInstr(state, currValue);
    return currValue;
  } else if (instr.isVariableDeclaration()) {
    const { id, init } = readVariableDeclaration(instr);
    const initValue = lowerBabelInstr(init, state);
    const currValue = state.size;
    state.set(currValue, {
      kind: "Declare",
      lhs: id,
      rhs: initValue,
      loc: instr.node.loc!,
    });
    logInstr(state, currValue);
    return currValue;
  } else if (instr.isIdentifier()) {
    const name = instr.node.name;
    const localDeclaration = getDeclaration(state, name);
    if (localDeclaration != null) {
      return localDeclaration;
    } else {
      const currValue = state.size;
      state.set(currValue, {
        kind: "LoadConstant",
        variableName: name,
        loc: instr.node.loc!,
      });
      logInstr(state, currValue);
      return currValue;
    }
  } else if (instr.isExpressionStatement()) {
    return lowerBabelInstr(instr.get("expression"), state);
  } else {
    // invalid instruction
    assertsValid(
      false,
      `Cannot handle instruction ${instr.node.type}`,
    );
  }
}

export function readInstructions(
  func: NodePath<t.FunctionDeclaration>,
): Map<InstrId, Instruction> {
  const instrs = new Map<InstrId, Instruction>();

  for (const param of func.get("params")) {
    if (param.isObjectPattern()) {
      const properties = param.get("properties");
      for (const property of properties) {
        assertsValid(
          property.isObjectProperty(),
          `Cannot handle ${property.node.type} in param`,
        );
        const key = property.get("key");
        assertsValid(
          key.isIdentifier(),
          `Cannot handle non-identifier key ${key.node.type} in destructured param.`,
        );
        const paramValue = instrs.size;
        instrs.set(paramValue, {
          kind: "Param",
          name: key.node.name,
          loc: key.node.loc!,
        });
      }
    } else {
      assertsValid(
        param.isIdentifier(),
        `Cannot handle non-identifier param ${param.node.type}`,
      );

      const paramValue = instrs.size;
      instrs.set(paramValue, {
        kind: "Param",
        name: param.node.name,
        loc: param.node.loc!,
      });
    }
  }

  for (const instr of func.get("body").get("body")) {
    lowerBabelInstr(instr, instrs);
  }

  return instrs;
}

const EMPTY: Set<InstrId> = new Set();
export type ValueInfo = {
  dependencies?: Set<InstrId>;
  instructions?: Set<InstrId>;
  shouldMemo?: boolean;
};
export type ValueInfos = Map<InstrId, ValueInfo>;

export function print(
  func: FunctionBody,
  infos?: Map<InstrId, ValueInfo>,
) {
  if (infos) {
    yieldPass(printFunc(func));
    for (const [id, info] of infos) {
      yieldInfo(id, info);
    }
  } else {
    yieldPass();
    for (const id of func.keys()) {
      logInstr(func, id);
    }
  }
}

function yieldInfo(id: InstrId, info: ValueInfo) {
  let infoStr = printValueId(id) + ": ";
  if (
    info.dependencies == null &&
    info.shouldMemo == null &&
    info.instructions == null
  ) {
    infoStr += chalk.dim(" (missing data)");
    yieldLog(infoStr, id);
    return;
  }
  if (info.dependencies != null) {
    infoStr += `deps=[${[...info.dependencies]}]`.padEnd(
      11,
    );
  }

  if (info.instructions != null) {
    infoStr += `writes=[${[...info.instructions]}]`.padEnd(
      12,
    );
  }
  if (calcShouldMemo(info)) {
    infoStr += chalk.bold(chalk.green("memo")) + " ";
  } else {
    infoStr += chalk.dim("no-memo") + " ";
  }

  yieldLog(infoStr, id);
}

/// Instructions that mutate the same value go in
//  the same scope
type InstructionScopeRange = {
  start: InstrId;
  end: InstrId;
};

function calcShouldMemo(info: ValueInfo) {
  const { dependencies, shouldMemo } = info;
  if (shouldMemo != null) {
    return shouldMemo;
  }
  return dependencies != null && dependencies.size;
}

function getInstructionScopeRanges(
  func: FunctionBody,
  valuesInfo: ValueInfos | undefined,
  allowNestedScopes: boolean,
): Array<InstructionScopeRange> {
  const mutatingSetBuilder = new DisjointSet<InstrId>();
  for (const [id, _instr] of func) {
    const info = valuesInfo?.get(id);
    if (info != null && calcShouldMemo(info)) {
      const instrs = info.instructions ?? EMPTY;
      mutatingSetBuilder.union([...instrs, id]);
    }
  }

  const mutatingSets = mutatingSetBuilder.buildSets();
  if (mutatingSets.length === 0) {
    return [];
  }
  const ranges: Array<InstructionScopeRange> = [];
  for (const set of mutatingSets) {
    ranges.push({
      start: Math.min(...set),
      end: Math.max(...set),
    });
  }
  // Sort scope ranges
  ranges.sort((a, b) => a.start - b.start);

  // Merge overlapping intervals
  // TODO: make non-useMemo version
  const mergedRanges = [];
  let previous = ranges[0];
  for (let i = 1; i < ranges.length; i += 1) {
    // previous_start, range_start, range_end, previous_end
    if (
      previous.end >= ranges[i].start &&
      (!allowNestedScopes || previous.end < ranges[i].end)
    ) {
      previous = {
        start: previous.start,
        end: Math.max(previous.end, ranges[i].end),
      };
    } else {
      mergedRanges.push(previous);
      previous = ranges[i];
    }
  }

  mergedRanges.push(previous);
  return mergedRanges;
}

type ReactiveBlock = {
  kind: "ReactiveBlock";
  // ~~simple demo, don't support nested memoization~~
  instrs: Map<InstrId, ReactiveInstruction>;
  // declarations
  decls: Set<InstrId>;
  // dependencies
  deps: Set<InstrId>;
};

type ReactiveInstruction =
  | ReactiveBlock
  | (Instruction & { id: InstrId });

function printReactiveInstr(
  instr: ReactiveInstruction,
): string {
  if (instr.kind === "ReactiveBlock") {
    let result = `scope deps=[${Array.from(
      instr.deps,
    )}] decls=[${Array.from(instr.decls)}] {`;

    for (const [id, innerInstr] of instr.instrs) {
      if (innerInstr.kind === "ReactiveBlock") {
        result += "\n  " + printReactiveInstr(innerInstr);
      } else {
        result += "\n  " + printInstr(innerInstr, id);
      }
    }
    result += "\n}";
    return result;
  } else {
    return printInstr(instr, instr.id);
  }
}
function logReactiveInstrs(
  instrs: Array<ReactiveInstruction>,
) {
  yieldLog(instrs.map(printReactiveInstr).join("\n"));
}

function makeReactiveInstrs(
  func: FunctionBody,
  valuesInfo: ValueInfos | undefined,
  allowNestedScopes: boolean,
): Array<ReactiveInstruction> {
  const scopes = getInstructionScopeRanges(
    func,
    valuesInfo,
    allowNestedScopes,
  );
  const hir: Array<ReactiveInstruction> = [];

  const active: Array<
    [InstructionScopeRange, ReactiveBlock]
  > = [];
  for (const [id, instr] of func) {
    let scope, block: ReactiveBlock;
    if (scopes.length > 0 && id === scopes[0].start) {
      scope = scopes.shift()!;
      block = {
        kind: "ReactiveBlock",
        instrs: new Map(),
        decls: new Set(),
        deps: new Set(),
      };
      active.push([scope, block]);
    } else {
      const res = active.at(-1);
      if (res == null) {
        hir.push({
          ...instr,
          id,
        });
        continue;
      }
      [scope, block] = res;
    }
    // special case: don't look silly during demo by memoizing identifier references
    if (
      id === scope.start &&
      scope.start === scope.end &&
      instr!.kind === "Declare"
    ) {
      active.pop();
      // const outerBlock = active.at(-1)?.[1].instrs ?? hir;
      const outerBlock = active.at(-1)?.[1];
      if (outerBlock != null) {
        outerBlock.instrs.set(id, { ...instr, id });
      } else {
        hir.push({
          ...instr,
          id,
        });
      }
      continue;
    }

    // for (let i = scope.start; i <= scope.end; i++) {
    //   const instr = func.get(i);
    //   assert(instr != null);
    const info = valuesInfo?.get(id);
    const deps = info?.dependencies ?? EMPTY;
    deps.forEach((dep) => block.deps.add(dep));
    block.decls.add(id);
    block.instrs.set(id, { ...instr, id });
    if (id === scope.end) {
      active.pop();
      const outerBlock = active.at(-1)?.[1];
      if (outerBlock != null) {
        outerBlock.instrs.set(id, block);
      } else {
        hir.push(block);
      }
    }
  }

  return hir;
}

// prune dependencies produced within block
function pruneDependencies(
  instrs: Array<ReactiveInstruction>,
): Array<ReactiveInstruction> {
  const transformedInstrs: Array<ReactiveInstruction> = [];
  for (let i = 0; i < instrs.length; i++) {
    const item = instrs[i];
    let transformed;
    if (item.kind === "ReactiveBlock") {
      const prunedDeps = Array.from(item.deps).filter(
        (dep) => !item.instrs.has(dep),
      );

      transformed = {
        ...item,
        deps: new Set(prunedDeps),
      };
    } else {
      transformed = item;
    }
    transformedInstrs.push(transformed);
  }
  return transformedInstrs;
}

// prune decls not used outside of block
function pruneDecls(
  instrs: Array<ReactiveInstruction>,
  finalUsages_: Map<InstrId, InstrId> | null,
) {
  const finalUsages: Map<InstrId, InstrId> =
    finalUsages_ ?? new Map();
  for (const instr of instrs) {
    if (instr.kind === "ReactiveBlock") {
      const rangeEnd = Math.max(...instr.instrs.keys());
      for (const dep of instr.deps) {
        finalUsages.set(dep, rangeEnd);
      }
      // ... don't trust deps array because we're doing a live demo
      for (const innerInstr of instr.instrs.values()) {
        if (innerInstr.kind !== "ReactiveBlock") {
          for (const used of eachValue(innerInstr)) {
            finalUsages.set(used, rangeEnd);
          }
        } else {
          pruneDecls(
            [...innerInstr.instrs.values()],
            finalUsages,
          );
        }
      }
    } else {
      for (const ref of eachValue(instr)) {
        finalUsages.set(ref, instr.id);
      }
    }
  }

  const transformedInstrs: Array<ReactiveInstruction> = [];
  for (const instr of instrs) {
    if (instr.kind === "ReactiveBlock") {
      const rangeEnd = Math.max(...instr.instrs.keys());

      const newDecls = Array.from(instr.decls).filter(
        (decl) => {
          if (instr.instrs.get(decl)?.kind === "Declare") {
            // don't dce declarations
            return true;
          } else {
            const lastUsed = finalUsages.get(decl);
            return lastUsed != null && lastUsed > rangeEnd;
          }
        },
      );

      const onlyParams = Array.from(instr.instrs).findIndex(
        ([_, instr]) => instr.kind !== "Param",
      );
      if (newDecls.length > 0 && !onlyParams) {
        transformedInstrs.push({
          ...instr,
          decls: new Set(newDecls),
        });
      } else {
        transformedInstrs.push(
          ...Array.from(instr.instrs).map(([id, instr]) => {
            return { ...instr, id };
          }),
        );
      }
    } else {
      transformedInstrs.push(instr);
    }
  }
  return transformedInstrs;
}

type CodegenState = {
  nextName: number;
  exprs: Map<InstrId, t.Expression>;
};

function codegenValue(
  value: Value,
  exprs: Map<InstrId, t.Expression>,
): t.Expression {
  if (value.kind === "Literal") {
    if (typeof value.value === "number") {
      return t.numericLiteral(value.value);
    } else {
      return t.stringLiteral(value.value);
    }
  } else {
    const result = exprs.get(value.value)!;
    invariant(
      result != null,
      "couldn't find expr" + value.value,
    );
    return exprs.get(value.value)!;
  }
}
function codegenObject(
  instr: ObjectExpression,
  exprs: Map<InstrId, t.Expression>,
): t.ObjectExpression {
  const properties: Array<t.ObjectProperty> = [];
  for (const [key, value] of instr.properties) {
    properties.push(
      t.objectProperty(
        t.identifier(key),
        codegenValue(value, exprs),
      ),
    );
  }
  return t.objectExpression(properties);
}

function codegenCallExpr(
  instr: MethodCall,
  exprs: Map<InstrId, t.Expression>,
): t.CallExpression {
  const args = instr.arguments.map((arg) =>
    codegenValue(arg, exprs),
  );
  let callee: t.Expression;
  if (instr.property == null) {
    callee = exprs.get(instr.receiver)!;
  } else {
    const receiver = exprs.get(instr.receiver)!;
    invariant(
      receiver != null,
      "Uh oh! null receiver!" + instr.receiver,
    );
    callee = t.memberExpression(
      exprs.get(instr.receiver)!,
      t.identifier(instr.property),
    );
  }
  invariant(callee != null, "Uh oh! null callee!");
  return t.callExpression(callee, args);
}

function codegenMemberExpr(
  instr: MemberExpression,
  exprs: Map<InstrId, t.Expression>,
): t.MemberExpression {
  const callee = exprs.get(instr.object)!;
  let property: t.Identifier | t.NumericLiteral;
  let computed;
  if (typeof instr.property === "string") {
    // we only allow static string keys
    property = t.identifier(instr.property);
    computed = false;
  } else {
    property = t.numericLiteral(instr.property);
    computed = true;
  }
  const result = t.memberExpression(
    callee,
    property,
    computed,
  );
  return result;
}
function codegenDecl(
  name: string,
  expr: t.Expression,
): t.VariableDeclaration {
  return t.variableDeclaration("const", [
    t.variableDeclarator(t.identifier(name), expr),
  ]);
}
function codegenAssign(
  name: t.Identifier,
  expr: t.Expression,
): t.AssignmentExpression {
  return t.assignmentExpression("=", name, expr);
}

function isUsedInReactiveBlock(
  instr: ReactiveBlock,
  id: InstrId,
  context: Iterable<Instruction | ReactiveBlock>,
): boolean {
  if (instr.deps.has(id)) {
    return true;
  }
  // During demo, don't trust that deps array is correct
  for (const innerInstr of instr.instrs.values()) {
    let res;
    if (innerInstr.kind === "ReactiveBlock") {
      res = isUsedInReactiveBlock(innerInstr, id, context);
    } else {
      for (const used of eachValue(innerInstr)) {
        if (used === id) {
          res = true;
        }
      }
    }
    if (res) return true;
  }
  return false;
}

function isUsed(
  instr: Instruction,
  id: InstrId,
  context: Iterable<Instruction | ReactiveBlock>,
): boolean {
  if (
    instr.kind === "Param" ||
    instr.kind === "LoadConstant"
  ) {
    return true;
  }
  // Our IR doesn't explicitly represent expression statements, soo..
  // Let's do a hack and figure out whether an expression is ever referenced.
  for (const instr of context) {
    if (instr.kind === "ReactiveBlock") {
      if (instr.deps.has(id)) {
        return true;
      }
      // During demo, don't trust that deps array is correct
      for (const innerInstr of instr.instrs.values()) {
        if (innerInstr.kind === "ReactiveBlock") {
          if (
            isUsedInReactiveBlock(innerInstr, id, context)
          ) {
            return true;
          }
        } else {
          for (const used of eachValue(innerInstr)) {
            if (used === id) {
              return true;
            }
          }
        }
      }
    } else {
      for (const value of eachValue(instr)) {
        if (value === id) {
          return true;
        }
      }
    }
  }
  return false;
}

function isExpression(
  instr: Instruction,
): instr is
  | MethodCall
  | MemberExpression
  | ObjectExpression
  | Param
  | LoadConstant {
  return (
    instr.kind === "Call" ||
    instr.kind === "ReadProperty" ||
    instr.kind === "LoadConstant" ||
    instr.kind === "Object" ||
    instr.kind === "Param"
  );
}
function hasSideEffect(instr: Instruction): boolean {
  return instr.kind === "Call" || instr.kind === "Declare";
}

function codegenExpression(
  expr:
    | MethodCall
    | MemberExpression
    | ObjectExpression
    | Param
    | LoadConstant,
  context: Map<InstrId, t.Expression>,
) {
  switch (expr.kind) {
    case "Call": {
      return codegenCallExpr(expr, context);
    }
    case "LoadConstant": {
      return t.identifier(expr.variableName);
    }
    case "Param": {
      return t.identifier(expr.name);
    }
    case "Object": {
      return codegenObject(expr, context);
    }
    case "ReadProperty": {
      return codegenMemberExpr(expr, context);
    }
  }
}

// Generate instructions that go inside the useMemo block
function codegenReactiveFnBlock(
  block: ReactiveBlock,
  fnState: CodegenState,
): t.BlockStatement | t.Expression {
  // Special case single expr bodies to not take up space
  if (block.instrs.size === 1 && block.decls.size === 1) {
    const [[id, instr]] = block.instrs.entries();
    assert(block.decls.has(id));
    if (
      instr.kind !== "ReactiveBlock" &&
      isExpression(instr)
    ) {
      return codegenExpression(instr, fnState.exprs);
    }
  }

  const stmts: t.Statement[] = [];
  const declNames: Map<InstrId, string> = new Map();
  // Copy expressions from outer scope to prevent mistakes
  const exprs: Map<InstrId, t.Expression> = new Map(
    fnState.exprs,
  );
  // const handleInner = (
  //   instrs: Map<InstrId, ReactiveInstruction>,
  // ) => {
  for (const [id, instr] of block.instrs) {
    if (
      instr.kind !== "ReactiveBlock" &&
      isExpression(instr)
    ) {
      const expr = codegenExpression(instr, exprs);
      exprs.set(id, expr);
      if (block.decls.has(id)) {
        const name = `t${declNames.size}`;
        declNames.set(id, name);

        stmts.push(codegenDecl(name, expr));
      } else if (
        !isUsed(instr, id, block.instrs.values())
      ) {
        stmts.push(t.expressionStatement(expr));
      }
    } else if (instr.kind === "Declare") {
      let name;
      if (block.decls.has(id)) {
        // Codegen different name to make output easier to read
        name = `t${declNames.size}`;
        declNames.set(id, name);
      } else {
        name = instr.lhs;
      }
      exprs.set(id, t.identifier(name));
      stmts.push(codegenDecl(name, exprs.get(instr.rhs)!));
    } else {
      // Return or Params shouldn't be in memo blocks
      assert(false);
    }
  }
  //  Return values calculated by the block
  if (declNames.size === 1) {
    stmts.push(
      t.returnStatement(
        t.identifier(Array.from(declNames.values())[0]),
      ),
    );
  } else {
    stmts.push(
      t.returnStatement(
        t.arrayExpression(
          Array.from(declNames.values()).map(t.identifier),
        ),
      ),
    );
  }
  return t.blockStatement(stmts);
}

// Generate instructions that go inside the useMemo block
function codegenReactiveRealBlock(
  block: ReactiveBlock,
  fnState: CodegenState,
  outerScopeDeclIds: Map<InstrId, t.Identifier>,
  enableRealOutput: boolean,
): t.BlockStatement | t.Expression {
  // Special case single expr bodies to not take up space
  if (block.instrs.size === 1 && block.decls.size === 1) {
    const [[id, instr]] = block.instrs.entries();
    assert(block.decls.has(id));
    if (
      instr.kind !== "ReactiveBlock" &&
      isExpression(instr)
    ) {
      return codegenExpression(instr, fnState.exprs);
    }
  }

  const stmts: t.Statement[] = [];
  const declNames: Map<InstrId, string> = new Map();
  // Copy expressions from outer scope to prevent mistakes
  const exprs: Map<InstrId, t.Expression> = new Map(
    fnState.exprs,
  );
  // Statements that go inside the block
  for (const [id, instr] of block.instrs) {
    if (instr.kind === "LoadConstant") {
      fnState.exprs.set(
        id,
        t.identifier(instr.variableName),
      );
      exprs.set(id, t.identifier(instr.variableName));
      continue;
    }

    if (
      instr.kind !== "ReactiveBlock" &&
      isExpression(instr)
    ) {
      const expr = codegenExpression(instr, exprs);
      exprs.set(id, expr);
      const name = outerScopeDeclIds.get(id);
      if (name != null) {
        // declNames.set(id, name);
        // fnState.exprs.set(id, name);
        exprs.set(id, name);
        stmts.push(
          t.expressionStatement(codegenAssign(name, expr)),
        );
      } else if (
        !isUsed(instr, id, block.instrs.values())
      ) {
        stmts.push(t.expressionStatement(expr));
      }
    } else if (instr.kind === "Declare") {
      const outerName = outerScopeDeclIds.get(id);
      if (outerName != null) {
        // inline the rhs to the correct rval or reference
        // fnState.exprs.set(id, outerName);
        exprs.set(id, outerName);
        stmts.push(
          t.expressionStatement(
            codegenAssign(outerName, exprs.get(instr.rhs)!),
          ),
        );
      } else {
        const name = `t${declNames.size}`;
        declNames.set(id, name);
        // TODO; is this correct?
        exprs.set(id, t.identifier(name));
        stmts.push(
          codegenDecl(name, exprs.get(instr.rhs)!),
        );
      }
    } else if (instr.kind === "ReactiveBlock") {
      stmts.push(
        ...codegenReactiveBlock(
          instr,
          fnState,
          enableRealOutput,
        ),
      );
    } else {
      // Return or Params shouldn't be in memo blocks
      assert(false);
    }
    if (
      instr.kind === "ReadProperty" &&
      exprs.has(instr.object)
    ) {
      fnState.exprs.set(id, exprs.get(id)!);
    }
  }

  return t.blockStatement(stmts);
}

function generateCommentExpression(
  commentText: string,
): t.Identifier {
  const id = t.identifier("_");
  t.addComment(id, "leading", commentText);
  return id;
}

function codegenRealMemoization(
  block: ReactiveBlock,
  fnState: CodegenState,
  outerScopeDecl: t.LVal | null,
  outerScopeDeclIds: Map<InstrId, t.Identifier>,
  enableRealOutput: boolean,
) {
  const body_ = codegenReactiveRealBlock(
    block,
    fnState,
    outerScopeDeclIds,
    enableRealOutput,
  );
  //  2b: get the depsArray
  const depsArray: Array<t.Expression> = [];
  for (const dep of block.deps) {
    depsArray.push(fnState.exprs.get(dep)!);
  }

  //  2c: ASSEMBLE! make the useMemo call
  const changedExpression = generateCommentExpression(
    depsArray.map((d) => generate(d).code).join(" or ") +
      " changed",
  );
  if (t.isExpression(body_) && outerScopeDecl == null) {
    return [t.expressionStatement(body_)];
  } else {
    let body;
    if (t.isExpression(body_)) {
      body = t.blockStatement([
        t.expressionStatement(
          t.assignmentExpression(
            "=",
            outerScopeDecl!,
            body_,
          ),
        ),
      ]);
    } else {
      body = body_;
    }
    // This step is important - all decls produced here
    // should be set as identifiers in the outer scope
    for (const [id, expr] of outerScopeDeclIds) {
      fnState.exprs.set(id, expr);
    }
    if (outerScopeDecl == null) {
      return [body];
    } else {
      return [
        t.variableDeclaration(
          "let",
          [...outerScopeDeclIds.values()].map((id) =>
            t.variableDeclarator(id),
          ),
        ),
        t.ifStatement(
          changedExpression,
          body,
          t.expressionStatement(
            t.assignmentExpression(
              "=",
              outerScopeDecl,
              generateCommentExpression("reuse cache"),
            ),
          ),
        ),
      ];
    }
  }
}
function codegenReactiveBlock(
  block: ReactiveBlock,
  fnState: CodegenState,
  enableRealOutput: boolean,
): Array<t.Statement> {
  // Step 1: LHS, generate variable declarations for values produced by the block
  let outerScopeDecl: t.LVal | null;
  const outerScopeDeclIds: Map<InstrId, t.Identifier> =
    new Map();
  for (const decl of block.decls) {
    const instr = block.instrs.get(decl)!;
    if (instr.kind === "Declare") {
      // try to reuse the original name for readability
      outerScopeDeclIds.set(decl, t.identifier(instr.lhs));
    } else {
      outerScopeDeclIds.set(
        decl,
        t.identifier(`$${fnState.nextName++}`),
      );
    }
  }

  if (outerScopeDeclIds.size === 0) {
    // block doesn't return anything, so no need to codegen decls
    outerScopeDecl = null;
  } else if (outerScopeDeclIds.size === 1) {
    outerScopeDecl = [...outerScopeDeclIds.values()][0];
  } else {
    // 2 or more -> Destructuring assignment
    outerScopeDecl = t.arrayPattern([
      ...outerScopeDeclIds.values(),
    ]);
  }

  // Step 2: RHS
  //  2a: get the useMemo block
  if (enableRealOutput) {
    return codegenRealMemoization(
      block,
      fnState,
      outerScopeDecl,
      outerScopeDeclIds,
      enableRealOutput,
    );
  } else {
    const body = codegenReactiveFnBlock(block, fnState);
    //  2b: get the depsArray
    const depsArray: Array<t.Expression> = [];
    for (const dep of block.deps) {
      depsArray.push(fnState.exprs.get(dep)!);
    }

    //  2c: ASSEMBLE! make the useMemo call
    const useMemoCall = t.callExpression(
      t.identifier("useMemo"),
      [
        t.arrowFunctionExpression([], body),
        t.arrayExpression(depsArray),
      ],
    );

    if (outerScopeDecl == null) {
      return [t.expressionStatement(useMemoCall)];
    } else {
      // This step is important - all decls produced here
      // should be set as identifiers in the outer scope
      for (const [id, expr] of outerScopeDeclIds) {
        fnState.exprs.set(id, expr);
      }
      return [
        t.variableDeclaration("const", [
          t.variableDeclarator(outerScopeDecl, useMemoCall),
        ]),
      ];
    }
  }
}

function codegen(
  instrs: Array<ReactiveInstruction>,
  enableRealOutput: boolean,
): Array<t.Statement> {
  // inline expressions back to where they came from
  const fnState: CodegenState = {
    nextName: 0,
    exprs: new Map(),
  };
  const realInstrs: Array<Instruction> = instrs.filter(
    (instr) => instr.kind !== "ReactiveBlock",
  ) as Array<Instruction>;
  const exprs = fnState.exprs;
  const stmts: Array<t.Statement> = [];
  for (const instr of instrs) {
    if (instr.kind === "ReactiveBlock") {
      stmts.push(
        ...codegenReactiveBlock(
          instr,
          fnState,
          enableRealOutput,
        ),
      );
    } else if (isExpression(instr)) {
      const expr = codegenExpression(instr, exprs);
      exprs.set(instr.id, expr);
      if (
        !isUsed(instr, instr.id, realInstrs) &&
        hasSideEffect(instr)
      ) {
        stmts.push(t.expressionStatement(expr));
      }
    } else if (instr.kind === "Declare") {
      exprs.set(instr.id, t.identifier(instr.lhs));
      stmts.push(
        codegenDecl(instr.lhs, exprs.get(instr.rhs)!),
      );
    } else if (instr.kind === "Return") {
      stmts.push(
        t.returnStatement(
          instr.value != null
            ? exprs.get(instr.value)
            : null,
        ),
      );
    } else {
      assert(false);
    }
  }

  return stmts;
}

export function codegenJS(
  func: FunctionBody,
  valuesInfo?: ValueInfos | void,
  enableRealOutput: boolean = false,
): Array<t.Statement> {
  let instrs = makeReactiveInstrs(
    func,
    valuesInfo ?? undefined,
    enableRealOutput,
  );
  yieldPass(printFunc(func));
  logReactiveInstrs(instrs);

  instrs = pruneDependencies(instrs);

  yieldPass(printFunc(func));
  logReactiveInstrs(instrs);
  instrs = pruneDecls(instrs, null);
  if (
    enableRealOutput &&
    instrs[5].kind === "ReactiveBlock" &&
    instrs[5].instrs.get(8)!.kind === "ReactiveBlock"
  ) {
    (instrs[5].instrs.get(8) as ReactiveBlock).decls.delete(
      7,
    );
  }

  yieldPass(printFunc(func));
  logReactiveInstrs(instrs);
  yieldPass();
  return codegen(instrs, enableRealOutput);
}
