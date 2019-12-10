open Day03;

[@bs.val] external __dirname: string = "__dirname";

let readInput = () => Node.Fs.readFileAsUtf8Sync(__dirname ++ "/input");

let solutionPart1 = () => {
  let wires = readInput() |> String.split_on_char('\n', _) |> Array.of_list;

  let wiresA = wires[0];
  let wiresB = wires[1];

  Part1.solve((wiresA, wiresB));
};

let solutionPart2 = () => {
  let wires = readInput() |> String.split_on_char('\n', _) |> Array.of_list;

  let wiresA = wires[0];
  let wiresB = wires[1];

  Part2.solve((wiresA, wiresB));
};

Js.log2("day03 part1", solutionPart1());
Js.log2("day03 part2", solutionPart2());
