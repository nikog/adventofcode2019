let coordinatesFromMap = (map: array(string)) => {
  let mapX = (y, (acc, x), i) => {
    let coords = i == "#" ? [|(x, y)|] |> Array.append(acc) : acc;
    (coords, x + 1);
  };

  let mapY = (initialX, (acc, y), row) => {
    let coords =
      row
      |> Js.String.split("")
      |> Array.fold_left(mapX(y), ([||], initialX))
      |> fst
      |> Array.append(acc);

    (coords, y + 1);
  };

  let initialX = 0;
  let initialY = 0;

  map |> Array.fold_left(mapY(initialX), ([||], initialY)) |> fst;
};

let intTupleToFloat = ((a, b)) => (a |> float_of_int, b |> float_of_int);

let isBetween = (coordA, coordB, coordC) => {
  let (a_x, a_y) = intTupleToFloat(coordA);
  let (b_x, b_y) = intTupleToFloat(coordB);
  let (c_x, c_y) = intTupleToFloat(coordC);

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

let sortCoordinates = (center, a, b) => {
  let (c_x, c_y) = intTupleToFloat(center);
  let (a_x, a_y) = intTupleToFloat(a);
  let (b_x, b_y) = intTupleToFloat(b);

  if (a_x -. c_x >= 0. && b_x -. c_x < 0.) {
    1;
  } else if (a_x -. c_x < 0. && b_x -. c_x >= 0.) {
    (-1);
  } else if (a_x -. c_x == 0. && b_x -. c_x == 0.) {
    if (a_y -. c_y >= 0. || b_y -. c_y >= 0.) {
      a_y > b_y ? 1 : (-1);
    } else {
      b_y > a_y ? 1 : (-1);
    };
  } else {
    let det = (a_x -. c_x) *. (b_y -. c_y) -. (b_x -. c_x) *. (a_y -. c_y);

    if (det < 0.) {
      1;
    } else if (det > 0.) {
      (-1);
    } else {
      let d1 = (a_x -. c_x) *. (a_x -. c_x) +. (a_y -. c_y) *. (a_y -. c_y);
      let d2 = (b_x -. c_x) *. (b_x -. c_x) +. (b_y -. c_y) *. (b_y -. c_y);
      d1 > d2 ? 1 : (-1);
    };
  };
};

module Part1 = {
  let make = (map: array(string)) => {
    let coordinates = map |> coordinatesFromMap;

    let ((x, y), visible) =
      coordinates->Belt.Array.reduce(
        ((0, 0), 0),
        ((bestCoord, visible), coordA) => {
          let visibleCoordinates =
            coordinates
            ->Belt.Array.keep(coordB =>
                coordA == coordB
                  ? false
                  : !
                      coordinates->Belt.Array.some(coordC => {
                        coordC == coordB || coordC == coordA
                          ? false
                          : {
                            isBetween(coordA, coordB, coordC);
                          }
                      })
              )
            ->Belt.Array.length;

          visibleCoordinates > visible
            ? (coordA, visibleCoordinates) : (bestCoord, visible);
        },
      );

    ((x |> string_of_int) ++ "," ++ (y |> string_of_int), visible);
  };
};

module Part2 = {
  let make = (map: array(string)) => {
    let coordinates = map |> coordinatesFromMap;

    let ((x, y), _, coords) =
      coordinates->Belt.Array.reduce(
        ((0, 0), 0, [||]),
        ((bestCoord, visible, visibleCoords), coordA) => {
          let visibleCoordinates =
            coordinates->Belt.Array.keep(coordB =>
              coordA == coordB
                ? false
                : !
                    coordinates->Belt.Array.some(coordC => {
                      coordC == coordB || coordC == coordA
                        ? false
                        : {
                          isBetween(coordA, coordB, coordC);
                        }
                    })
            );

          let visibleCoordsCount = visibleCoordinates |> Array.length;

          visibleCoordsCount > visible
            ? (coordA, visibleCoordsCount, visibleCoordinates)
            : (bestCoord, visible, visibleCoords);
        },
      );

    let transformed =
      coords |> Array.map(((ax, ay)) => (ax - y, (ay - y) * (-1)));

    transformed |> Array.sort(sortCoordinates((x - y, y - y)));

    transformed
    |> Array.map(((ax, ay)) => (ax + y, ay * (-1) + y))
    |> Belt.Array.reverse;
  };
};

/*
 loop each coordinate a
   loop each coordinate b
     if loop each coordinate c is between coordinates a and b
       return no line of sight from a to b
     else
       return line of sight from a to b
     return total coordinates with line of sight
   return which coordinate has most line of sights
 */
