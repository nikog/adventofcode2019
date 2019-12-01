open Jest;
open Expect;

describe("day01", () => {
  testAll(
    "part1 test",
    [(12, 2), (14, 2), (1969, 654), (100756, 33583)],
    ((input, expected)) =>
    expect(Day01.Part1.solve(input)) |> toBe(expected)
  );

  testAll(
    "part2 test",
    [(12, 2), (14, 2), (1969, 966), (100756, 50346)],
    ((input, expected)) =>
    expect(Day01.Part2.solve(input)) |> toBe(expected)
  );
});
