let parse = () => {
  const tableRows = Array.from($(".parse_table").tBodies.item(0).rows).map(
    row =>
      Array.from(row.cells).map(cell =>
        cell.innerHTML
          .replace(/<\/?terminal>|<\/?nonterm>|<\/?sub>|<\/?term>|\&nbsp;/g, "")
          .replace(/\&amp;epsilon/g, "eps")
          .replace(/dot/g, ".")
          .replace(/semi/g, ";")
          .replace(/colon/g, ":")
          .replace(/sr/g, "::")
          .replace(/comma/g, ",")
          .replace(/plus/g, "+")
          .replace(/minus/g, "-")
          .replace(/mult/g, "*")
          .replace(/div/g, "/")
          .replace(/geq/g, ">=")
          .replace(/leq/g, "<=")
          .replace(/gt/g, ">")
          .replace(/lt/g, "<")
          .replace(/neq/g, "<>")
          .replace(/eqeq/g, "==")
          .replace(/eq/g, "=")
          .replace(/intnum/g, "integerliteral")
          .replace(/floatnum/g, "floatliteral")
          .replace(/lsqbr/g, "[")
          .replace(/rsqbr/g, "]")
          .replace(/lpar/g, "(")
          .replace(/rpar/g, ")")
          .replace(/lcurbr/g, "{")
          .replace(/rcurbr/g, "}")
          .replace(/VARIABLE/g, "VAR")
          .replace(/VARIABLE1/g, "VAR1")
          .replace(/STATEMENT/g, "STAT")
          .replace(/→/g, "->")
      )
  );

  const nRows = tableRows.length - 1;
  const nCols = tableRows[0].length - 1;

  const terminals = [];
  const map = new Map();

  for (let i = 1; i <= nCols; i += 1) {
    const cell = tableRows[0][i];
    terminals.push(cell);
  }

  for (let i = 1; i <= nRows; i += 1) {
    const row = tableRows[i];
    const variable = row[0];
    for (let j = 1; j <= nCols; j += 1) {
      const terminal = terminals[j - 1];
      const production = row[j];
      if (production !== "") {
        const key = `${variable}, ${terminal}`;
        map.set(key, production);
      }
    }
  }

  let parseTableAsString = "";
  for (const [key, production] of map.entries()) {
    parseTableAsString += `(${key}): ${production}\n`;
  }

  const compare = (str1, str2) => {
    const getKeyValuePair = str => {
      if (str === "") {
        return ["", ""];
      }
      const matches = str.match(/^\((\w+), ((?:(?!\)\:).)+)\)\: .*$/);
      return [matches[1], matches[2]];
    };
    const [k1, v1] = getKeyValuePair(str1);
    const [k2, v2] = getKeyValuePair(str2);

    if (k1 < k2) {
      return -1;
    } else if (k1 > k2) {
      return 1;
    } else {
      if (v1 < v2) {
        return -1;
      } else if (v1 > v2) {
        return 1;
      }
    }

    return 0;
  };

  parseTableAsString = parseTableAsString
    .split("\n")
    .sort(compare)
    .join("\n");

  return parseTableAsString;
};
