import * as t from "@babel/types";
import { NodePath } from "@babel/core";
import assert from "assert";

/// Babel helpers
export function readVariableDeclaration(
  node: NodePath<t.VariableDeclaration>,
): {
  id: string;
  init: NodePath<t.Expression>;
} {
  const declarations = node.get("declarations");
  assert(declarations.length === 1);
  const id = declarations[0].get("id");
  assert(id.isIdentifier());
  const init = declarations[0].get("init");
  assert(init.isExpression());
  return {
    id: id.node.name,
    init,
  };
}

export function readMemberExpression(node: NodePath<t.MemberExpression>): {
  object: NodePath<t.Expression>;
  property: string | number;
} {
  const object = node.get("object");
  assert(object.isExpression());
  const property = node.get("property");
  if (property.isIdentifier() && node.node.computed === false) {
    return {
      object,
      property: property.node.name,
    };
  } else if (property.isNumericLiteral()) {
    return {
      object,
      property: property.node.value,
    };
  } else {
    assert(false);
  }
}

export function readCallExpression(node: NodePath<t.CallExpression>): {
  callee: NodePath<t.Expression>;
  args: Array<NodePath<t.Expression>>;
} {
  const callee = node.get("callee");
  assert(callee.isExpression());
  const babelArgs = node.get("arguments");
  const args = [];

  for (const babelArg of babelArgs) {
    assert(babelArg.isExpression());
    args.push(babelArg);
  }
  return {
    callee,
    args,
  };
}

export function readObjectExpression(
  node: NodePath<t.ObjectExpression>,
): Array<[NodePath<t.Identifier>, NodePath<t.Expression>]> {
  const properties = node.get("properties");
  const checkedProperties: Array<
    [NodePath<t.Identifier>, NodePath<t.Expression>]
  > = [];
  for (const property of properties) {
    // Don't handle object methods or spread elements yet
    assert(property.isObjectProperty());
    // Also only handle static keys
    // (this is mainly to keep the IR really simple for the demo)
    assert(!property.node.computed);
    const key = property.get("key");
    assert(key.isIdentifier());
    const value = property.get("value");
    assert(value.isExpression());
    checkedProperties.push([key, value]);
  }
  return checkedProperties;
}

export function isValidInstr(
  node: NodePath<t.Node>,
): node is
  | NodePath<t.VariableDeclaration>
  | NodePath<t.MemberExpression>
  | NodePath<t.CallExpression>
  | NodePath<t.Identifier> {
  return (
    node.node.type in
    ["VariableDeclaration", "MemberExpression", "CallExpression", "Identifier"]
  );
}

/// Too much code to write live
