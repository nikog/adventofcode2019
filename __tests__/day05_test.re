open Jest;
open Expect;

describe("day01", () => {
  testAll(
    "intcode v1 test",
    [
      ([|1, 0, 0, 0, 99|], [|2, 0, 0, 0, 99|]),
      ([|2, 3, 0, 3, 99|], [|2, 3, 0, 6, 99|]),
      ([|2, 4, 4, 5, 99, 0|], [|2, 4, 4, 5, 99, 9801|]),
      ([|1, 1, 1, 4, 99, 5, 6, 0, 99|], [|30, 1, 1, 4, 2, 5, 6, 0, 99|]),
    ],
    ((input, expected)) => {
      let (program, _) = Day05.Part1.make(input, ~input=0);

      program |> expect |> toEqual(expected);
    },
  );

  test("part1 test", () => {
    let input = 10;
    let (_, output) = Day05.Part1.make([|3, 0, 4, 0, 99|], ~input);

    output |> expect |> toEqual([|10|]);
  });

  test("part1 test", () => {
    let (code, _) = Day05.Part1.make([|1002, 4, 3, 4, 33|], ~input=0);

    code |> expect |> toEqual([|1002, 4, 3, 4, 99|]);
  });

  test("part2 test position equals 8", () => {
    let (_, output) =
      Day05.Part1.make([|3, 9, 8, 9, 10, 9, 4, 9, 99, (-1), 8|], ~input=8);

    output |> expect |> toEqual([|1|]);
  });

  test("part2 test position doesnt equal 8", () => {
    let (_, output) =
      Day05.Part1.make([|3, 9, 8, 9, 10, 9, 4, 9, 99, (-1), 8|], ~input=10);

    output |> expect |> toEqual([|0|]);
  });

  test("part2 test immediate equals 8", () => {
    let (_, output) =
      Day05.Part1.make([|3, 3, 1108, (-1), 8, 3, 4, 3, 99|], ~input=8);

    output |> expect |> toEqual([|1|]);
  });

  test("part2 test immediate doesnt equal 8", () => {
    let (_, output) =
      Day05.Part1.make([|3, 3, 1108, (-1), 8, 3, 4, 3, 99|], ~input=10);

    output |> expect |> toEqual([|0|]);
  });
});
