import DisjointSet from "demo-utils/dist/DisjointSet";
import {
  FunctionBody,
  InstrId,
  Instruction,
  eachValue,
} from "./LoweredJavaScript";
import { ValueInfos } from "./Transform";

const HOOK_FUNCTION_NAME = /^use[A-Z].*/;

export function isHookCall(
  instr: Instruction,
  func: FunctionBody,
): boolean {
  if (instr.kind === "Call") {
    const callee = func.get(instr.receiver)!;
    return (
      callee.kind === "LoadConstant" &&
      HOOK_FUNCTION_NAME.test(callee.variableName)
    );
  }
  return false;
}
function getMutableValues(
  func: FunctionBody,
): Set<InstrId> {
  // Step 1: primitive type inference
  //   which values are *actually* mutable?
  const mutableValues: Set<InstrId> = new Set();
  for (const [id, instr] of func) {
    // A value is `maybeMutable`
    let isMutable;
    // 0. if it is NOT the result of a hook call
    if (isHookCall(instr, func)) {
      continue;
    }
    // 1. if it comes from specific instructions
    isMutable = ["Call", "Object"].includes(
      instr.kind,
    );
    // 2. or is computed from other maybeMutable values
    for (const used of eachValue(instr)) {
      isMutable ||= mutableValues.has(used);
    }
    if (isMutable) {
      mutableValues.add(id);
    }
  }
  return mutableValues;
}

export function understandAliasing(
  func: FunctionBody,
  info: ValueInfos,
) {
  // Step 1: primitive type inference
  //   which values are *actually* mutable?
  const mutableValues: Set<InstrId> =
    getMutableValues(func);

  // Step 2: Create map of aliases
  const aliasBuilder = new DisjointSet<InstrId>();
  for (const [id, instr] of func) {
    if (
      [
        "Declare",
        "ReadProperty",
        "Call",
      ].includes(instr.kind)
    ) {
      const mutableAliases = [];
      for (const val of eachValue(instr)) {
        if (mutableValues.has(val)) {
          mutableAliases.push(val);
        }
      }
      if (mutableAliases.length > 0) {
        aliasBuilder.union([
          id,
          ...mutableAliases,
        ]);
      }
    }
  }
  const aliasSets = aliasBuilder.buildSets();
  // Mark all aliases as mutated
  for (const aliases of aliasSets) {
    const allMutatingInstructions =
      new Set<number>(aliases);

    for (const alias of aliases) {
      for (const instr of info.get(alias)!
        .instructions ?? []) {
        allMutatingInstructions.add(instr);
      }
    }

    for (const alias of aliases) {
      info.get(alias)!.instructions =
        allMutatingInstructions;
    }
  }
}
export function understandMutability(
  func: FunctionBody,
  info: ValueInfos,
) {
  // Step 1: primitive type inference
  //   which values are *actually* mutable?
  const mutableValues: Set<InstrId> =
    getMutableValues(func);

  // Step 2:
  // Figure out which instructions might change operands
  // other than its own instrId
  for (const [id, instr] of func) {
    if (instr.kind === "Call") {
      for (const val of eachValue(instr)) {
        if (mutableValues.has(val)) {
          info.get(val)!.instructions ??=
            new Set();
          info.get(val)!.instructions!.add(id);
        }
      }
    }
  }

  // Step 2: Create map of aliases
  const aliasBuilder = new DisjointSet<InstrId>();
  for (const [id, instr] of func) {
    if (
      [
        "Declare",
        "ReadProperty",
        "Call",
      ].includes(instr.kind)
    ) {
      const mutableAliases = [];
      for (const val of eachValue(instr)) {
        if (mutableValues.has(val)) {
          mutableAliases.push(val);
        }
      }
      if (mutableAliases.length > 0) {
        aliasBuilder.union([
          id,
          ...mutableAliases,
        ]);
      }
    }
  }
  const aliasSets = aliasBuilder.buildSets();
  // Mark all aliases as mutated
  for (const aliases of aliasSets) {
    const allMutatingInstructions =
      new Set<number>(aliases);

    for (const alias of aliases) {
      for (const instr of info.get(alias)!
        .instructions ?? []) {
        allMutatingInstructions.add(instr);
      }
    }

    for (const alias of aliases) {
      info.get(alias)!.instructions =
        allMutatingInstructions;
    }
  }
}
