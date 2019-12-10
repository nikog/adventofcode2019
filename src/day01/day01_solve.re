open Day01;

[@bs.val] external __dirname: string = "__dirname";

open Js_string;
open Js_array;

let readInput = () => Node.Fs.readFileAsUtf8Sync(__dirname ++ "/input");

let solution = solveFn =>
  readInput()
  |> split("\n")
  |> map(value => value |> int_of_string |> solveFn)
  |> reduce((acc, n) => acc + n, 0);

Js.log(solution(Part1.solve));
Js.log(solution(Part2.solve));
