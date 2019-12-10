let getValues = string => {
  let split = string |> Js.String.split(")");
  (split[0], split[1]);
};

let findBodyOf = (satellite, map) =>
  List.find(
    item => {
      let (_, itemSatellite) = item |> getValues;
      satellite == itemSatellite;
    },
    map,
  );

let rec getOrbit = (map, satellite, toBody) =>
  switch (findBodyOf(satellite, map)) {
  | exception Not_found => (1, [satellite])
  | nextOrbit =>
    let (nextBody, _) = nextOrbit |> getValues;

    nextBody == toBody
      ? (2, [satellite])
      : {
        let (distance, path) = getOrbit(map, nextBody, toBody);
        (1 + distance, List.append([satellite], path));
      };
  };

let distanceTo = (map, fromSatellite, toBody) =>
  map
  |> List.fold_left(
       ((accDistance, accPath), value) => {
         let (body, satellite) = value |> getValues;

         if (satellite == fromSatellite) {
           let (distance, path) = getOrbit(map, body, toBody);
           (accDistance + distance, List.append(path, accPath));
         } else {
           (accDistance, accPath);
         };
       },
       (0, []),
     );

module Part1 = {
  let make = orbitMap =>
    orbitMap
    |> List.fold_left(
         (acc, value) => {
           let (body, _) = value |> getValues;

           let (orbitDistance, _) = getOrbit(orbitMap, body, "COM");

           acc + orbitDistance;
         },
         0,
       );
};

module Part2 = {
  let make = orbitMap => {
    let (_, youPath) = distanceTo(orbitMap, "YOU", "COM");
    let (_, sanPath) = distanceTo(orbitMap, "SAN", "COM");

    let youSet = Belt.Set.String.fromArray(Array.of_list(youPath));
    let sanSet = Belt.Set.String.fromArray(Array.of_list(sanPath));
    let commonBodies =
      Belt.Set.String.intersect(youSet, sanSet) |> Belt.Set.String.toArray;

    let totalDist =
      commonBodies
      |> Array.fold_left(
           (acc, i) => {
             let (youDist, _) = distanceTo(orbitMap, "YOU", i);
             let (sanDist, _) = distanceTo(orbitMap, "SAN", i);

             let totalDist = youDist + sanDist;

             totalDist < acc ? totalDist : acc;
           },
           max_int,
         );

    totalDist - 2;
  };
};
