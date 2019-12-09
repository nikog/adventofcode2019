type tree =
  | Empty
  | Node(treeNode)
and treeNode = {
  value: string,
  mutable children: list(tree),
};

let rec findNode = (node, value, distance) =>
  switch (node) {
  | Empty => None
  | Node(v) =>
    v.value == value ?
      Some((node, distance)) :
      {
        let nodeRef: ref(tree) = ref(Empty);
        let childDistanceRef = ref(distance);

        v.children
        |> List.iter(item => {
             let node = findNode(item, value, distance + 1);

             switch (node) {
             | Some((Node(v), distance)) =>
               nodeRef := Node(v);
               childDistanceRef := distance;
             | _ => ()
             };
           });

        Some((nodeRef^, childDistanceRef^));
      }
  };

module Part1 = {
  let make = (map: list(string)) => {
    let (_, distance) =
      map
      |> List.fold_left(
           ((acc, accDistance), path) => {
             let jooh = path |> Js.String.split(")");

             let bodyLabel = jooh[0];
             let satelliteLabel = jooh[1];

             let body = findNode(acc, bodyLabel, 0);

             let satellite = Node({value: satelliteLabel, children: []});

             switch (body) {
             | Some((Empty, _))
             | None => (Node({value: bodyLabel, children: [satellite]}), 1)
             | Some((Node(v), distance)) =>
               let satellites = v.children;
               v.children = [satellite, ...satellites];

               (acc, accDistance + distance + 1);
             };
           },
           (Empty, 0),
         );

    distance;
  };
};
