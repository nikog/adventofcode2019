open Jest;
open Expect;

describe("day04 part1", () => {
  test("test input 1", () =>
    Day04.Part1.make(111111, 111111) |> expect |> toEqual(1)
  );

  test("test input 2", () =>
    Day04.Part1.make(223450, 223450) |> expect |> toEqual(0)
  );

  test("test input 3", () =>
    Day04.Part1.make(123789, 123789) |> expect |> toEqual(0)
  );
});

describe("day04 part2", () => {
  test("test input 1", () =>
    Day04.Part2.make(112233, 112233) |> expect |> toEqual(1)
  );

  test("test input 2", () =>
    Day04.Part2.make(123444, 123444) |> expect |> toEqual(0)
  );

  test("test input 3", () =>
    Day04.Part2.make(111122, 111122) |> expect |> toEqual(1)
  );
});
