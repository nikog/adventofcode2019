let re = [%re "/([A-Z])(\d+)/"];

module Part1 = {
  let getCapture = (result, i) =>
    switch (result) {
    | Some(result) =>
      let captures = result |> Js.Re.captures;
      captures[i] |> Js.Nullable.toOption;
    | None => None
    };

  let sum = (coord, nextCoord) => {
    let (x0, y0) = coord;
    let (x1, y1) = nextCoord;

    (x0 + x1, y0 + y1);
  };

  let stringToCoords = (wire: string) => {
    let cursor = ref((0, 0));

    let coords =
      wire
      |> String.split_on_char(',', _)
      |> Array.of_list
      |> Array.map(vector => {
           let result = re->Js.Re.exec_(vector);
           let direction = getCapture(result, 1);
           let distance = getCapture(result, 2);

           let nextCoord =
             switch (direction, distance) {
             | (Some("L"), Some(d)) =>
               sum(cursor^, (int_of_string(d) * (-1), 0))
             | (Some("R"), Some(d)) => sum(cursor^, (int_of_string(d), 0))
             | (Some("U"), Some(d)) => sum(cursor^, (0, int_of_string(d)))
             | (Some("D"), Some(d)) =>
               sum(cursor^, (0, int_of_string(d) * (-1)))
             | (Some(_), Some(_)) => (0, 0)
             | (_, _) => (0, 0)
             };

           cursor := nextCoord;

           nextCoord;
         });

    Array.append([|(0, 0)|], coords);
  };

  let intersect2 =
      (
        ((p0_x_i, p0_y_i), (p1_x_i, p1_y_i)),
        ((p2_x_i, p2_y_i), (p3_x_i, p3_y_i)),
      ) => {
    let p0_x = float_of_int(p0_x_i);
    let p0_y = float_of_int(p0_y_i);
    let p1_x = float_of_int(p1_x_i);
    let p1_y = float_of_int(p1_y_i);
    let p2_x = float_of_int(p2_x_i);
    let p2_y = float_of_int(p2_y_i);
    let p3_x = float_of_int(p3_x_i);
    let p3_y = float_of_int(p3_y_i);

    let s1_x = p1_x -. p0_x;
    let s1_y = p1_y -. p0_y;
    let s2_x = p3_x -. p2_x;
    let s2_y = p3_y -. p2_y;

    let jooh = -. s2_x *. s1_y +. s1_x *. s2_y;

    switch (jooh) {
    | 0. => None
    | _ =>
      let s =
        (-. s1_y *. (p0_x -. p2_x) +. s1_x *. (p0_y -. p2_y))
        /. (-. s2_x *. s1_y +. s1_x *. s2_y);
      let t =
        (s2_x *. (p0_y -. p2_y) -. s2_y *. (p0_x -. p2_x))
        /. (-. s2_x *. s1_y +. s1_x *. s2_y);

      s >= 0. && s <= 1. && t >= 0. && t <= 1.
        ? Some((
            int_of_float(p0_x +. t *. s1_x),
            int_of_float(p0_y +. t *. s1_y),
          ))
        : None;
    };
  };

  let findIntersections = (wireA, wireB) => {
    let intersections = ref([]);

    wireA
    |> Array.iteri((ia, a) =>
         switch (ia) {
         | 0 => ()
         | _ =>
           let segmentA = (wireA[ia - 1], a);

           wireB
           |> Array.iteri((ib, b) =>
                switch (ib) {
                | 0 => ()
                | _ =>
                  let segmentB = (wireB[ib - 1], b);
                  let jooh = intersect2(segmentA, segmentB);

                  switch (jooh) {
                  | Some(v) => intersections := [v, ...intersections^]
                  | None => ()
                  };
                }
              );
         }
       );

    intersections^;
  };

  let solve = ((wireA: string, wireB: string)) => {
    let wireACoords = stringToCoords(wireA);
    let wireBCoords = stringToCoords(wireB);

    let intersections = findIntersections(wireACoords, wireBCoords);

    let min =
      intersections
      |> List.map(intersection => {
           let (x, y) = intersection;

           switch (x, y) {
           | (0, 0) => max_int
           | (_, _) => Js.Math.abs_int(x) + Js.Math.abs_int(y)
           };
         })
      |> ListLabels.fold_left(~f=(acc, i) => min(acc, i), ~init=max_int);

    min;
  };
};

module Part2 = {
  let isBetween = ((a_x, a_y), (b_x, b_y), (c_x, c_y)) => {
    let crossproduct =
      (c_y -. a_y) *. (b_x -. a_x) -. (c_x -. a_x) *. (b_y -. a_y);
    let dotproduct =
      (c_x -. a_x) *. (b_x -. a_x) +. (c_y -. a_y) *. (b_y -. a_y);

    if (Js.Math.abs_float(crossproduct) != 0. || dotproduct < 0.) {
      false;
    } else {
      let squaredlengthba =
        (b_x -. a_x) *. (b_x -. a_x) +. (b_y -. a_y) *. (b_y -. a_y);

      if (dotproduct > squaredlengthba) {
        false;
      } else {
        true;
      };
    };
  };

  let toFloat = ((x, y)) => (float_of_int(x), float_of_int(y));

  let distanceToIntersection = (wire, coord) => {
    let sum = ref(0);
    let intersected = ref(false);
    wire
    |> Array.iteri((ia, a) =>
         switch (ia, intersected^) {
         | (0, _) => ()
         | (_, true) => ()
         | (_, false) =>
           let segmentA = (wire[ia - 1], a);

           let intersection =
             isBetween(toFloat(wire[ia - 1]), toFloat(a), toFloat(coord));

           intersection
             ? {
               let ((x0, y0), _) = segmentA;
               let (x1, y1) = coord;

               let distance =
                 Js.Math.abs_int(x0 - x1) + Js.Math.abs_int(y0 - y1);

               sum := sum^ + distance;
               intersected := true;
             }
             : {
               let ((x0, y0), (x1, y1)) = segmentA;

               let distance =
                 Js.Math.abs_int(x0 - x1) + Js.Math.abs_int(y0 - y1);

               sum := sum^ + distance;
               ();
             };
         }
       );
    ();

    sum^;
  };

  let solve = ((wireA: string, wireB: string)) => {
    let wireACoords = Part1.stringToCoords(wireA);
    let wireBCoords = Part1.stringToCoords(wireB);

    let intersections = Part1.findIntersections(wireACoords, wireBCoords);

    let min =
      intersections
      |> List.map(coord => {
           let wireADistance = distanceToIntersection(wireACoords, coord);
           let wireBDistance = distanceToIntersection(wireBCoords, coord);

           switch (wireADistance + wireBDistance) {
           | 0 => max_int
           | v => v
           };
         })
      |> ListLabels.fold_left(~f=(acc, i) => min(acc, i), ~init=max_int);

    min;
  };
};
