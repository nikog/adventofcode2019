module Part1 = {
  let rec permutations = phases => {
    switch (Belt.Set.Int.size(phases)) {
    | 1 => [Belt.Set.Int.toList(phases)]
    | _ =>
      let ans = ref([]);

      phases->Belt.Set.Int.forEach(phase => {
        let remainingPhases = phases->Belt.Set.Int.remove(phase);
        let innerPermutations = permutations(remainingPhases);

        innerPermutations
        |> List.iter(innerPermutation => {
             let permutation = innerPermutation |> List.append([phase]);

             ans := [permutation, ...ans^];
           });
      });

      ans^;
    };
  };

  let make = program => {
    let computer = Day09.Part1.make(program);
    let phases = Belt.Set.Int.fromArray([|0, 1, 2, 3, 4|]);

    permutations(phases)
    ->Belt.List.reduce(
        0,
        (acc, permutation) => {
          let signal =
            permutation->Belt.List.reduce(
              0,
              (input, phase) => {
                let (_, output) = computer(~input=[|phase, input|]);

                output[0] |> int_of_string;
              },
            );

          signal > acc ? signal : acc;
        },
      );
  };
};

module Part2 = {
  let make = program => {
    let computer = Day09.Part1.make(program);
    let phases = Belt.Set.Int.fromArray([|5, 6, 7, 8, 9|]);

    Part1.permutations(phases)
    ->Belt.List.reduce(
        0,
        (acc, permutation) => {
          let ans = ref(0);
          let break = ref(false);
          let inputs = permutation |> Array.of_list |> Array.map(i => [i]);
          let outputs = Belt.Array.make(5, [||]);

          while (! break^) {
            let signal =
              permutation->Belt.List.reduceWithIndex(
                Some(ans^), (input, _, i) => {
                switch (input) {
                | None => None
                | Some(input) =>
                  inputs[i] = List.append(inputs[i], [input]);

                  let (_, output) =
                    computer(~input=inputs[i]->Array.of_list);

                  if (output |> Array.length != (outputs[i] |> Array.length)) {
                    outputs[i] = output;

                    switch (
                      output->Belt.Array.get(Belt.Array.length(output) - 1)
                    ) {
                    | Some(v) => Some(v |> int_of_string)
                    | None => None
                    };
                  } else {
                    None;
                  };
                }
              });

            switch (signal) {
            | Some(v) => ans := v
            | None => break := true
            };
          };

          ans^ > acc ? ans^ : acc;
        },
      );
  };
};
