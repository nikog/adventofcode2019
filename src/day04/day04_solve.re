open Day04;
open Belt;

[@bs.val] external __dirname: string = "__dirname";

let readInput = () => Node.Fs.readFileAsUtf8Sync(__dirname ++ "/input");

let solutionPart1 = () => {
  let values = readInput() |> Js.String.split("-");

  switch (values[0], values[1]) {
  | (Some(a), Some(b)) => Part1.make(int_of_string(a), int_of_string(b))
  | _ => 0
  };
};

let solutionPart2 = () => {
  let values = readInput() |> Js.String.split("-");

  switch (values[0], values[1]) {
  | (Some(a), Some(b)) => Part2.make(int_of_string(a), int_of_string(b))
  | _ => 0
  };
};

Js.log2("day04 part1", solutionPart1());
Js.log2("day04 part2", solutionPart2());
