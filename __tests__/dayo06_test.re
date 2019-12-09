open Jest;
open Expect;

describe("day06", () => {
  test("part1 graph test", () => {
    let input = [
      "COM)B",
      "B)C",
      "C)D",
      "D)E",
      "E)F",
      "B)G",
      "G)H",
      "D)I",
      "E)J",
      "J)K",
      "K)L",
    ];

    Day06_graph.Part1.make(input) |> expect |> toBe(42);
  });

  test("part1 test", () => {
    let input = [
      "COM)B",
      "B)C",
      "C)D",
      "D)E",
      "E)F",
      "B)G",
      "G)H",
      "D)I",
      "E)J",
      "J)K",
      "K)L",
    ];

    Day06.Part1.make(input) |> expect |> toBe(42);
  });

  test("part2 test", () => {
    let input = [
      "COM)B",
      "B)C",
      "C)D",
      "D)E",
      "E)F",
      "B)G",
      "G)H",
      "D)I",
      "E)J",
      "J)K",
      "K)L",
      "K)YOU",
      "I)SAN",
    ];

    Day06.Part2.make(input) |> expect |> toBe(4);
  });
});
