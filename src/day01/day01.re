module Part1 = {
  let solve = input => input / 3 - 2;
};

module Part2 = {
  let rec solve = fuel =>
    Part1.solve(fuel) |> (fuel => fuel > 0 ? fuel + solve(fuel) : 0);
};
