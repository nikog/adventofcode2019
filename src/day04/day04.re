open Belt;

let hasDoubleDigits = password => {
  let re = [%re "/(\d)\1/"];
  re->Js.Re.test_(password->string_of_int);
};

let hasExactlyDoubleDigits = password => {
  let re = [%re "/(\d)(\1+)/mig"];
  let results = re |> Js.String.match(_, password->string_of_int);

  let captures =
    switch (results) {
    | Some(result) => result
    | None => [||]
    };

  captures
  ->Array.map(capture => capture->String.length)
  ->Array.some(count => count === 2);
};

let hasDecreasingDigits = password => {
  let passwordString = password |> string_of_int;
  let found = ref(false);

  passwordString
  ->Js.String.split("", _)
  ->Array.forEachWithIndex((i, value) => {
      let rest = passwordString |> Js.String.sliceToEnd(~from=i + 1);
      let digit = max(0, int_of_string(value) - 1);

      let re = Js.Re.fromString("[0-" ++ string_of_int(digit) ++ "]");
      let hasDecreasingDigits = re->Js.Re.test_(rest);

      hasDecreasingDigits && ! found^
        ? {
          found := true;
          ();
        }
        : ();
    });

  found^;
};

module Part1 = {
  let make = (lower, upper) => {
    let passwords =
      Array.range(lower, upper)
      ->Array.reduce(0, (acc, input) =>
          hasDoubleDigits(input) && !hasDecreasingDigits(input)
            ? acc + 1 : acc
        );

    passwords;
  };
};

module Part2 = {
  let make = (lower, upper) => {
    let passwords =
      Array.range(lower, upper)
      ->Array.reduce(0, (acc, input) =>
          hasExactlyDoubleDigits(input) && !hasDecreasingDigits(input)
            ? acc + 1 : acc
        );

    passwords;
  };
};

[@bs.val] external __dirname: string = "__dirname";

let readInput = () => Node.Fs.readFileAsUtf8Sync(__dirname ++ "/input");

let solutionPart1 = () => {
  let values = readInput() |> Js.String.split("-");

  switch (values[0], values[1]) {
  | (Some(a), Some(b)) => Part1.make(int_of_string(a), int_of_string(b))
  | _ => 0
  };
};

let solutionPart2 = () => {
  let values = readInput() |> Js.String.split("-");

  switch (values[0], values[1]) {
  | (Some(a), Some(b)) => Part2.make(int_of_string(a), int_of_string(b))
  | _ => 0
  };
};

Js.log2("day04 part1", solutionPart1());
Js.log2("day04 part2", solutionPart2());
