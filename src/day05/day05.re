type mode =
  | Position
  | Immediate;

let parseOpcode = (instruction: int) => {
  let opcode = instruction mod 10;

  let modes = (instruction - opcode) / 100;

  (
    opcode,
    modes
    ->string_of_int
    ->Js.String.split("", _)
    ->Belt.Array.reverse
    ->Belt.Array.map(code =>
        switch (int_of_string(code)) {
        | 0 => Position
        | 1 => Immediate
        | _ => Position
        }
      ),
  );
};

let getValue = (instructions, modes, pointer, i) =>
  switch (Belt.Array.get(modes, i - 1)) {
  | Some(Immediate) => instructions[pointer + i]
  | Some(Position)
  | None => instructions[instructions[pointer + i]]
  };

module Part1 = {
  let rec loop = (instructions: array(int), pointer, ~input, ~output) => {
    let withinBounds = pointer <= (instructions |> Array.length) - 1;

    let (opcode, modes) = parseOpcode(instructions[pointer]);

    switch (withinBounds, opcode) {
    | (true, 1) =>
      let p1 = getValue(instructions, modes, pointer, 1);
      let p2 = getValue(instructions, modes, pointer, 2);
      let pos = instructions[pointer + 3];
      instructions[pos] = p1 + p2;
      loop(instructions, pointer + 4, ~input, ~output);
    | (true, 2) =>
      let p1 = getValue(instructions, modes, pointer, 1);
      let p2 = getValue(instructions, modes, pointer, 2);
      let pos = instructions[pointer + 3];
      instructions[pos] = p1 * p2;
      loop(instructions, pointer + 4, ~input, ~output);
    | (true, 3) =>
      let pos = instructions[pointer + 1];
      instructions[pos] = input;
      loop(instructions, pointer + 2, ~input, ~output);
    | (true, 4) =>
      let pos = instructions[pointer + 1];

      loop(
        instructions,
        pointer + 2,
        ~input,
        ~output=Array.append(output, [|instructions[pos]|]),
      );
    | (true, 5) =>
      let jump = getValue(instructions, modes, pointer, 1) > 0;
      let newPointer =
        jump ? getValue(instructions, modes, pointer, 2) : pointer + 3;
      loop(instructions, newPointer, ~input, ~output);
    | (true, 6) =>
      let jump = getValue(instructions, modes, pointer, 1) == 0;
      let newPointer =
        jump ? getValue(instructions, modes, pointer, 2) : pointer + 3;
      loop(instructions, newPointer, ~input, ~output);
    | (true, 7) =>
      let p1 = getValue(instructions, modes, pointer, 1);
      let p2 = getValue(instructions, modes, pointer, 2);
      let pos = instructions[pointer + 3];

      instructions[pos] = p1 < p2 ? 1 : 0;
      loop(instructions, pointer + 4, ~input, ~output);
    | (true, 8) =>
      let p1 = getValue(instructions, modes, pointer, 1);
      let p2 = getValue(instructions, modes, pointer, 2);
      let pos = instructions[pointer + 3];

      instructions[pos] = p1 == p2 ? 1 : 0;
      loop(instructions, pointer + 4, ~input, ~output);
    | (true, 99)
    | (true, _)
    | (false, _) => (instructions, output)
    };
  };

  let make = (instructions: array(int), ~input) =>
    loop(instructions, 0, ~input, ~output=[||]);
};
