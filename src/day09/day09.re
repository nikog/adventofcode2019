exception Invalid_instruction(string);
exception Out_of_bounds(string);
type mode =
  | Position
  | Immediate
  | Relative;

let parseOpcode = (instruction: int) => {
  let opcode = instruction mod 100;

  let modes = (instruction - opcode) / 100;

  (
    opcode,
    modes
    ->string_of_int
    ->Js.String.split("", _)
    ->Belt.Array.reverse
    ->Belt.Array.map(code =>
        switch (int_of_string(code)) {
        | 2 => Relative
        | 1 => Immediate
        | 0
        | _ => Position
        }
      ),
  );
};

let getIndex = (instructions: array(int64), modes, pointer, relativeBase, i) =>
  switch (Belt.Array.get(modes, i - 1)) {
  | Some(Relative) =>
    relativeBase + (instructions[pointer + i] |> Int64.to_int)
  | Some(Immediate) => pointer + i
  | Some(Position)
  | None => instructions[pointer + i] |> Int64.to_int
  };

module Part1 = {
  let rec loop = (instructions, pointer, ~input, ~output, ~relativeBase) => {
    let withinBounds = pointer <= (instructions |> Array.length) - 1;

    let (opcode, modes) = parseOpcode(instructions[pointer] |> Int64.to_int);

    let basicGetIndex = getIndex(instructions, modes, pointer, relativeBase);

    switch (withinBounds, opcode) {
    | (true, 1) =>
      let p1 = basicGetIndex(1) |> Array.get(instructions);
      let p2 = basicGetIndex(2) |> Array.get(instructions);
      let pos = basicGetIndex(3);
      instructions[pos] = Int64.add(p1, p2);
      loop(instructions, ~input, ~output, ~relativeBase, pointer + 4);
    | (true, 2) =>
      let p1 = basicGetIndex(1) |> Array.get(instructions);
      let p2 = basicGetIndex(2) |> Array.get(instructions);
      let pos = basicGetIndex(3);
      instructions[pos] = Int64.mul(p1, p2);
      loop(instructions, ~input, ~output, ~relativeBase, pointer + 4);
    | (true, 3) =>
      let pos = basicGetIndex(1);
      instructions[pos] = input;
      loop(instructions, ~input, ~output, ~relativeBase, pointer + 2);
    | (true, 4) =>
      let pos = basicGetIndex(1);

      loop(
        instructions,
        pointer + 2,
        ~input,
        ~output=Array.append(output, [|instructions[pos]|]),
        ~relativeBase,
      );
    | (true, 5) =>
      let jump =
        basicGetIndex(1) |> Array.get(instructions) |> Int64.to_int > 0;
      let newPointer =
        jump
          ? basicGetIndex(2) |> Array.get(instructions) |> Int64.to_int
          : pointer + 3;
      loop(instructions, ~input, ~output, ~relativeBase, newPointer);
    | (true, 6) =>
      let jump =
        basicGetIndex(1) |> Array.get(instructions) |> Int64.to_int == 0;
      let newPointer =
        jump
          ? basicGetIndex(2) |> Array.get(instructions) |> Int64.to_int
          : pointer + 3;
      loop(instructions, ~input, ~output, ~relativeBase, newPointer);
    | (true, 7) =>
      let p1 = basicGetIndex(1) |> Array.get(instructions);
      let p2 = basicGetIndex(2) |> Array.get(instructions);
      let pos = basicGetIndex(3);

      instructions[pos] = (Int64.compare(p1, p2) < 0 ? 1 : 0) |> Int64.of_int;
      loop(instructions, ~input, ~output, ~relativeBase, pointer + 4);
    | (true, 8) =>
      let p1 = basicGetIndex(1) |> Array.get(instructions);
      let p2 = basicGetIndex(2) |> Array.get(instructions);
      let pos = basicGetIndex(3);

      instructions[pos] =
        (Int64.compare(p1, p2) == 0 ? 1 : 0) |> Int64.of_int;
      loop(instructions, ~input, ~output, ~relativeBase, pointer + 4);
    | (true, 9) =>
      let p1 = basicGetIndex(1) |> Array.get(instructions) |> Int64.to_int;

      loop(
        instructions,
        pointer + 2,
        ~input,
        ~output,
        ~relativeBase=relativeBase + p1,
      );

    | (true, 99) => (instructions, output)
    | (true, v) =>
      raise(Invalid_instruction("Invalid instruction " ++ string_of_int(v)))
    | (false, _) =>
      raise(Out_of_bounds("Out of bounds at " ++ string_of_int(pointer)))
    };
  };

  let make = (instructions: array(int), ~input: int) => {
    let memory =
      Array.make(100 * (instructions |> Array.length), 0 |> Int64.of_int);

    let instructionsWithMemory =
      instructions |> Array.map(Int64.of_int) |> Array.append(_, memory);

    let (endstate, output) =
      loop(
        instructionsWithMemory,
        0,
        ~input=input |> Int64.of_int,
        ~output=[||],
        ~relativeBase=0,
      );

    (
      endstate |> Array.map(Int64.to_string),
      output |> Array.map(Int64.to_string),
    );
  };
};
