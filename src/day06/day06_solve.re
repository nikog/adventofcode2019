open Day06;

[@bs.val] external __dirname: string = "__dirname";
let readInput = () => Node.Fs.readFileAsUtf8Sync(__dirname ++ "/input");

let solution = () =>
  readInput() |> Js.String.split("\n") |> Array.to_list |> Part1.make;

Js.log2("day06 part1", solution());

let solutionPart2 = () =>
  readInput() |> Js.String.split("\n") |> Array.to_list |> Part2.make;

Js.log2("day06 part2", solutionPart2());
