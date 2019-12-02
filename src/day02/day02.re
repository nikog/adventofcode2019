[@bs.val] external __dirname: string = "__dirname";

let readInput = () => Node.Fs.readFileAsUtf8Sync(__dirname ++ "/input");

module Part1 = {
  let action = (cmd, p1, p2) =>
    switch (cmd, p1, p2) {
    | (1, Some(p1), Some(p2)) => Some(p1 + p2)
    | (2, Some(p1), Some(p2)) => Some(p1 * p2)
    | _ => None
    };

  let rec loop = (input: array(int), index: int) =>
    (input |> Array.length) - 1 >= index ?
      {
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
      } :
      input;

  let solve = (input: array(int)) => loop(input, 0);
};

let solutionPart1 = (~noun, ~verb) =>
  readInput()
  |> Js.String.splitByRe([%re "/,/"])
  |> Array.map(int_of_string)
  |> Array.mapi((index, value) =>
       switch (index) {
       | 1 => noun
       | 2 => verb
       | _ => value
       }
     )
  |> Part1.solve
  |> (arr => arr[0]);

Js.log(solutionPart1(~noun=12, ~verb=2));

let solutionPart2 = () => {
  let nouns = Belt.Array.range(0, 99);
  let verbs = Belt.Array.range(0, 99);

  let res =
    nouns
    |> Array.map(noun =>
         verbs
         |> Array.map(verb =>
              switch (solutionPart1(~verb, ~noun)) {
              | 19690720 => Some(100 * noun + verb)
              | _ => None
              }
            )
       )
    |> Belt.Array.concatMany
    |> Belt.Array.keepMap(_, x => x);

  res->Array.get(0);
};

Js.log(solutionPart2());
