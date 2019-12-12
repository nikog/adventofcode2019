open Day10;

[@bs.val] external __dirname: string = "__dirname";
let readInput = () => Node.Fs.readFileAsUtf8Sync(__dirname ++ "/input");

let solutionPart1 = () => {
  let (_, visible) =
    readInput()
    |> String.split_on_char('\n', _)
    |> Array.of_list
    |> Part1.make;
  ();

  visible;
};

Js.log2("day10 part1", solutionPart1());

let solutionPart2 = () => {
  let coords =
    readInput()
    |> String.split_on_char('\n', _)
    |> Array.of_list
    |> Part2.make;
  ();

  let (x, y) = coords[199];

  x * 100 + y;
};

Js.log2("day10 part2", solutionPart2());
