module Part1 = {
  let rec loop = (i, outputs) => {
    i < Array.length(outputs) - 1
      ? {
        switch (outputs[i], outputs[i + 1], outputs[i + 2]) {
        | (_, _, "2") => 1 + loop(i + 3, outputs)
        | (_, _, _) => loop(i + 3, outputs)
        };
      }
      : 0;
  };

  let make = program => {
    let computer = Day09.Part1.make;
    let (_, outputs) = computer(program, ~input=[||]);

    loop(0, outputs);
  };
};

module Part2 = {
  let rec findTile = (tile, i, outputs) => {
    i > 0
      ? {
        switch (outputs[i - 2], outputs[i - 1], outputs[i]) {
        | (x, y, t) when t == tile => Some((x, y))
        | (_, _, _) => findTile(tile, i - 3, outputs)
        };
      }
      : None;
  };

  let rec findScore = (i, outputs) => {
    i > 0
      ? {
        switch (outputs[i - 2], outputs[i - 1], outputs[i]) {
        | ("-1", "0", score) => Some(score)
        | (_, _, _) => findScore(i - 3, outputs)
        };
      }
      : None;
  };

  let make = program => {
    let computer = Day09.Part1.make;

    let ans = ref("0");
    let break = ref(false);
    let inputs = ref([||]);

    program[0] = 2;

    let pointer = ref(0);
    let relativeBase = ref(0);
    let instructions = ref(program);

    let ball = ref(("0", "0"));
    let paddle = ref(("0", "0"));

    while (! break^) {
      switch (
        computer(
          instructions^,
          ~input=inputs^,
          ~pointer=pointer^,
          ~relativeBase=relativeBase^,
        )
      ) {
      | exception (
                    Day09.Waiting_for_instruction(
                      currentPointer,
                      currentRelativeBase,
                      currentInstructions,
                      output64,
                    )
                  ) =>
        pointer := currentPointer;
        relativeBase := currentRelativeBase;
        instructions := currentInstructions |> Array.map(Int64.to_int);

        let output = output64 |> Array.map(Int64.to_string);

        let currentBall = findTile("4", Array.length(output) - 1, output);
        let currentPaddle = findTile("3", Array.length(output) - 1, output);

        switch (currentBall, currentPaddle) {
        | (Some(b), Some(p)) =>
          ball := b;
          paddle := p;
        | (Some(b), None) => ball := b
        | (None, Some(p)) => paddle := p
        | (_, _) => ()
        };

        let (ballX, _) = ball^;
        let (paddleX, _) = paddle^;

        let joystick =
          switch (compare(ballX |> int_of_string, paddleX |> int_of_string)) {
          | v when v == 0 => 0
          | v when v < 0 => (-1)
          | v when v > 0 => 1
          | _ => 0
          };

        inputs := [|joystick|];

      | (_, outputs) =>
        break := true;

        let score = findScore(Array.length(outputs) - 1, outputs);

        switch (score) {
        | Some(score) => ans := score
        | None => ()
        };
      };
    };

    ans^;
  };
};
