module Part1 = {
  let action = (cmd, p1, p2) =>
    switch (cmd, p1, p2) {
    | (1, Some(p1), Some(p2)) => Some(p1 + p2)
    | (2, Some(p1), Some(p2)) => Some(p1 * p2)
    | _ => None
    };

  let rec loop = (input: array(int), index: int) =>
    (input |> Array.length) - 1 >= index
      ? {
        let cmd = input[index];

        switch (cmd) {
        | 1
        | 2 =>
          let p1 = input[input[index + 1]];
          let p2 = input[input[index + 2]];
          let pos = input[index + 3];
          let value = action(cmd, Some(p1), Some(p2));

          switch (value) {
          | Some(v) => input[pos] = v
          | None => ()
          };

          loop(input, index + 4);
        | 99
        | _ => input
        };
      }
      : input;

  let solve = (input: array(int)) => loop(input, 0);
};
