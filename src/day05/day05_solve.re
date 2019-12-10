open Day05;

[@bs.val] external __dirname: string = "__dirname";
let readInput = () => Node.Fs.readFileAsUtf8Sync(__dirname ++ "/input");

let solutionPart1 = () => {
  let (_, output) =
    readInput()
    |> String.split_on_char(',', _)
    |> Array.of_list
    |> Array.map(int_of_string)
    |> Part1.make(~input=1);
  ();

  output[(output |> Array.length) - 1];
};

Js.log2("day05 part1", solutionPart1());

let solutionPart2 = () => {
  let (_, output) =
    readInput()
    |> String.split_on_char(',', _)
    |> Array.of_list
    |> Array.map(int_of_string)
    |> Part1.make(~input=5);
  ();

  output[(output |> Array.length) - 1];
};

Js.log2("day05 part2", solutionPart2());
