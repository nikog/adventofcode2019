open Day02;

[@bs.val] external __dirname: string = "__dirname";

let readInput = () => Node.Fs.readFileAsUtf8Sync(__dirname ++ "/input");

let solutionPart1 = (~noun, ~verb) =>
  readInput()
  |> String.split_on_char(',', _)
  |> Array.of_list
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
