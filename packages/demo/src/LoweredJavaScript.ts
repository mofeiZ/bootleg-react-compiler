import { types as t } from "@babel/core";
import chalk from "chalk";

// we would realistically flatten literals as well,
// but... let's not make the demo too confusing
export type Value =
  | { kind: "RValue"; value: InstrId }
  | { kind: "Literal"; value: string | number };

export type InstrId = number;
export type MethodCall = {
  kind: "Call";
  receiver: InstrId;
  property: string | null;
  arguments: Array<Value>;
  loc: t.SourceLocation;
};
export type ObjectExpression = {
  kind: "Object";
  properties: Map<string, Value>;
  loc: t.SourceLocation;
};

export type MemberExpression = {
  kind: "ReadProperty";
  object: InstrId;
  property: string | number;
  loc: t.SourceLocation;
};
export type VariableDeclaration = {
  kind: "Declare";
  lhs: string;
  rhs: InstrId;
  loc: t.SourceLocation;
};
export type Param = {
  kind: "Param";
  name: string;
  loc: t.SourceLocation;
};
export type LoadConstant = {
  kind: "LoadConstant";
  variableName: string;
  loc: t.SourceLocation;
};
export type Return = {
  kind: "Return";
  value: InstrId | null;
  loc: t.SourceLocation;
};
export type Instruction =
  | MethodCall
  | MemberExpression
  | VariableDeclaration
  | Param
  | LoadConstant
  | Return
  | ObjectExpression;

// each instruction produces a value
export type FunctionBody = Map<InstrId, Instruction>;

export function* eachValue(
  instr: Instruction,
): Generator<InstrId> {
  switch (instr.kind) {
    case "Call": {
      yield instr.receiver;
      for (const arg of instr.arguments) {
        if (arg.kind === "RValue") {
          yield arg.value;
        }
      }
      break;
    }
    case "Object": {
      for (const [_, value] of instr.properties) {
        if (value.kind === "RValue") {
          yield value.value;
        }
      }
      break;
    }
    case "ReadProperty": {
      yield instr.object;
      return;
    }
    case "Declare": {
      yield instr.rhs;
      return;
    }
    case "Return": {
      if (instr.value != null) {
        yield instr.value;
      }
      return;
    }
    case "Param":
    case "LoadConstant": {
      return;
    }
  }
}

export function printValueId(id: InstrId): string {
  return chalk.bold(chalk.blue("$" + id.toString()));
}

function printValue(value: Value): string {
  if (value.kind === "Literal") {
    return typeof value.value === "string"
      ? `'${value.value}'`
      : value.value.toString();
  } else {
    return printValueId(value.value);
  }
}

export function printInstr(
  instr: Instruction,
  id: number,
): string {
  const idStr = chalk.blue(
    "$" + id.toString().padEnd(3, " "),
  );
  switch (instr.kind) {
    case "Call": {
      return `${idStr}= Call ${printValueId(
        instr.receiver,
      )}${
        instr.property != null ? "." + instr.property : ""
      } (${instr.arguments.map((arg) => printValue(arg))})`;
    }
    case "Object": {
      const properties = Array.from(instr.properties).map(
        ([key, value]) => {
          return `${key}:${printValue(value)}`;
        },
      );
      return `${idStr}= Object {${properties.join(", ")}}`;
    }
    case "ReadProperty": {
      return `${idStr}= ReadProperty ${printValueId(
        instr.object,
      )} ${
        typeof instr.property === "number"
          ? `${instr.property}`
          : `'${instr.property}'`
      }`;
    }
    case "Declare": {
      return `${idStr}= Declare '${
        instr.lhs
      }' ${printValueId(instr.rhs)}`;
    }
    case "Param": {
      return `${idStr}= Param '${instr.name}'`;
    }
    case "LoadConstant": {
      return `${idStr}= LoadConstant '${instr.variableName}'`;
    }
    case "Return": {
      return `${idStr}= Return ${
        instr.value != null ? printValueId(instr.value) : ""
      }`;
    }
  }
}

export function printFunc(func: FunctionBody): string {
  return Array.from(func.entries())
    .map(([id, instr]) => printInstr(instr, id))
    .join("\n");
}
