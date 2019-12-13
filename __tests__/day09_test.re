open Jest;
open Expect;

describe("day09", () => {
  testAll(
    "intcode v1 test",
    [
      ([|1, 0, 0, 0, 99|], [|2, 0, 0, 0, 99|]),
      ([|2, 3, 0, 3, 99|], [|2, 3, 0, 6, 99|]),
      ([|2, 4, 4, 5, 99, 0|], [|2, 4, 4, 5, 99, 9801|]),
      ([|1, 1, 1, 4, 99, 5, 6, 0, 99|], [|30, 1, 1, 4, 2, 5, 6, 0, 99|]),
    ],
    ((input, expected)) => {
      let (program, _) = Day09.Part1.make(input, ~input=[|0|]);

      program
      |> expect
      |> toBeSupersetOf(expected |> Array.map(string_of_int));
    },
  );

  test("part1 test", () => {
    let input = [|10|];
    let (_, output) = Day09.Part1.make([|3, 0, 4, 0, 99|], ~input);

    output |> expect |> toEqual([|10|] |> Array.map(string_of_int));
  });

  test("part1 test", () => {
    let (code, _) = Day09.Part1.make([|1002, 4, 3, 4, 33|], ~input=[|0|]);

    code
    |> expect
    |> toBeSupersetOf([|1002, 4, 3, 4, 99|] |> Array.map(string_of_int));
  });

  test("part2 test position equals 8", () => {
    let (_, output) =
      Day09.Part1.make(
        [|3, 9, 8, 9, 10, 9, 4, 9, 99, (-1), 8|],
        ~input=[|8|],
      );

    output |> expect |> toEqual([|1|] |> Array.map(string_of_int));
  });

  test("part2 test position doesnt equal 8", () => {
    let (_, output) =
      Day09.Part1.make(
        [|3, 9, 8, 9, 10, 9, 4, 9, 99, (-1), 8|],
        ~input=[|10|],
      );

    output |> expect |> toEqual([|0|] |> Array.map(string_of_int));
  });

  test("part2 test immediate equals 8", () => {
    let (_, output) =
      Day09.Part1.make([|3, 3, 1108, (-1), 8, 3, 4, 3, 99|], ~input=[|8|]);

    output |> expect |> toEqual([|1|] |> Array.map(string_of_int));
  });

  test("part2 test immediate doesnt equal 8", () => {
    let (_, output) =
      Day09.Part1.make([|3, 3, 1108, (-1), 8, 3, 4, 3, 99|], ~input=[|10|]);

    output |> expect |> toEqual([|0|] |> Array.map(string_of_int));
  });

  test("part 2 test large value", () => {
    let (_, output) =
      Day09.Part1.make(
        [|
          3,
          21,
          1008,
          21,
          8,
          20,
          1005,
          20,
          22,
          107,
          8,
          21,
          20,
          1006,
          20,
          31,
          1106,
          0,
          36,
          98,
          0,
          0,
          1002,
          21,
          125,
          20,
          4,
          20,
          1105,
          1,
          46,
          104,
          999,
          1105,
          1,
          46,
          1101,
          1000,
          1,
          20,
          4,
          20,
          1105,
          1,
          46,
          98,
          99,
        |],
        ~input=[|7|],
      );

    output |> expect |> toEqual([|999|] |> Array.map(string_of_int));
  });

  test("part 2 test large value", () => {
    let (_, output) =
      Day09.Part1.make(
        [|
          3,
          21,
          1008,
          21,
          8,
          20,
          1005,
          20,
          22,
          107,
          8,
          21,
          20,
          1006,
          20,
          31,
          1106,
          0,
          36,
          98,
          0,
          0,
          1002,
          21,
          125,
          20,
          4,
          20,
          1105,
          1,
          46,
          104,
          999,
          1105,
          1,
          46,
          1101,
          1000,
          1,
          20,
          4,
          20,
          1105,
          1,
          46,
          98,
          99,
        |],
        ~input=[|8|],
      );

    output |> expect |> toEqual([|1000|] |> Array.map(string_of_int));
  });

  test("part 2 test large value", () => {
    let (_, output) =
      Day09.Part1.make(
        [|
          3,
          21,
          1008,
          21,
          8,
          20,
          1005,
          20,
          22,
          107,
          8,
          21,
          20,
          1006,
          20,
          31,
          1106,
          0,
          36,
          98,
          0,
          0,
          1002,
          21,
          125,
          20,
          4,
          20,
          1105,
          1,
          46,
          104,
          999,
          1105,
          1,
          46,
          1101,
          1000,
          1,
          20,
          4,
          20,
          1105,
          1,
          46,
          98,
          99,
        |],
        ~input=[|9|],
      );

    output |> expect |> toEqual([|1001|] |> Array.map(string_of_int));
  });

  test("part1 latest", () => {
    let instructions = [|
      109,
      1,
      204,
      (-1),
      1001,
      100,
      1,
      100,
      1008,
      100,
      16,
      101,
      1006,
      101,
      0,
      99,
    |];
    let (_, output) = Day09.Part1.make(instructions, ~input=[|0|]);

    output |> expect |> toEqual(instructions |> Array.map(string_of_int));
  });

  test("part1 large number", () => {
    let instructions = [|104, 1125899906842624, 99|];
    let (_, output) = Day09.Part1.make(instructions, ~input=[|0|]);

    output
    |> expect
    |> toEqual([|1125899906842624|] |> Array.map(string_of_int));
  });

  test("part1 large number 2", () => {
    let instructions = [|1102, 34915192, 34915192, 7, 4, 7, 99, 0|];
    let (_, output) = Day09.Part1.make(instructions, ~input=[|0|]);

    output[0] |> Js.String.split("") |> Array.length |> expect |> toEqual(16);
  });
});
