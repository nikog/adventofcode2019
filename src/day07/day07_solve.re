open Day07;

[@bs.val] external __dirname: string = "__dirname";
let readInput = () => Node.Fs.readFileAsUtf8Sync(__dirname ++ "/input");

let solutionPart1 = () => {
  readInput()
  |> String.split_on_char(',', _)
  |> Array.of_list
  |> Array.map(int_of_string)
  |> Part1.make;
};

Js.log2("day07 part1", solutionPart1());

let solutionPart2 = () => {
  readInput()
  |> String.split_on_char(',', _)
  |> Array.of_list
  |> Array.map(int_of_string)
  |> Part2.make;
};

Js.log2("day07 part2", solutionPart2());
