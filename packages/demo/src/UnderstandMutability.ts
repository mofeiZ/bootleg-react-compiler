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
    // 1. if it comes from specific instructions
    let isMutable = ["Call", "Object"].includes(
      instr.kind,
    );
    // 2. or is computed from other maybeMutable values
    for (const used of eachValue(instr)) {
      isMutable ||= mutableValues.has(used);
    }
    // Note: the result of a hook calls themselves are not writable
    // but... calls or objects that use those values might be
    // So let's be conservative and say that the results of hook
    // calls are writable
    if (isMutable) {
      mutableValues.add(id);
    }
  }
  return mutableValues;
}

/**
 * Understand which instruction values might be the same
 * and merge their write sets.
 * Note that this is a very naive implementation, and doesn't
 * cover many optimizations (see `InferMutableRanges` in
 * react/compiler).
 */
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

/**
 * Just in case we don't get to writing this during live demo
 */
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
    if (
      instr.kind === "Call" &&
      !isHookCall(instr, func)
    ) {
      for (const val of eachValue(instr)) {
        if (mutableValues.has(val)) {
          info.get(val)!.instructions ??=
            new Set();
          info.get(val)!.instructions!.add(id);
        }
      }
    }
  }
}
