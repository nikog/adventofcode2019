[@bs.val] external __dirname: string = "__dirname";

open Js_string;
open Js_array;

let readInput = () => Node.Fs.readFileAsUtf8Sync(__dirname ++ "/input");

let solution = solveFn =>
  readInput()
  |> split("\n")
  |> map(value => value |> int_of_string |> solveFn)
  |> reduce((acc, n) => acc + n, 0);

module Part1 = {
  let solve = input => input / 3 - 2;
};

module Part2 = {
  let rec solve = fuel =>
    Part1.solve(fuel) |> (fuel => fuel > 0 ? fuel + solve(fuel) : 0);
};

Js.log(solution(Part1.solve));
Js.log(solution(Part2.solve));
