// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Day09$Adventofcode2019 = require("../src/day09/day09.bs.js");

Jest.describe("day09", (function (param) {
        Jest.testAll("intcode v1 test", /* :: */[
              /* tuple */[
                /* array */[
                  1,
                  0,
                  0,
                  0,
                  99
                ],
                /* array */[
                  2,
                  0,
                  0,
                  0,
                  99
                ]
              ],
              /* :: */[
                /* tuple */[
                  /* array */[
                    2,
                    3,
                    0,
                    3,
                    99
                  ],
                  /* array */[
                    2,
                    3,
                    0,
                    6,
                    99
                  ]
                ],
                /* :: */[
                  /* tuple */[
                    /* array */[
                      2,
                      4,
                      4,
                      5,
                      99,
                      0
                    ],
                    /* array */[
                      2,
                      4,
                      4,
                      5,
                      99,
                      9801
                    ]
                  ],
                  /* :: */[
                    /* tuple */[
                      /* array */[
                        1,
                        1,
                        1,
                        4,
                        99,
                        5,
                        6,
                        0,
                        99
                      ],
                      /* array */[
                        30,
                        1,
                        1,
                        4,
                        2,
                        5,
                        6,
                        0,
                        99
                      ]
                    ],
                    /* [] */0
                  ]
                ]
              ]
            ], (function (param) {
                var match = Day09$Adventofcode2019.Part1.make(undefined, undefined, /* array */[0], param[0]);
                return Jest.Expect.toBeSupersetOf($$Array.map((function (prim) {
                                  return String(prim);
                                }), param[1]), Jest.Expect.expect(match[0]));
              }));
        Jest.test("part1 test", (function (param) {
                var input = /* array */[10];
                var match = Day09$Adventofcode2019.Part1.make(undefined, undefined, input, /* array */[
                      3,
                      0,
                      4,
                      0,
                      99
                    ]);
                return Jest.Expect.toEqual($$Array.map((function (prim) {
                                  return String(prim);
                                }), /* array */[10]), Jest.Expect.expect(match[1]));
              }));
        Jest.test("part1 test", (function (param) {
                var match = Day09$Adventofcode2019.Part1.make(undefined, undefined, /* array */[0], /* array */[
                      1002,
                      4,
                      3,
                      4,
                      33
                    ]);
                return Jest.Expect.toBeSupersetOf($$Array.map((function (prim) {
                                  return String(prim);
                                }), /* array */[
                                1002,
                                4,
                                3,
                                4,
                                99
                              ]), Jest.Expect.expect(match[0]));
              }));
        Jest.test("part2 test position equals 8", (function (param) {
                var match = Day09$Adventofcode2019.Part1.make(undefined, undefined, /* array */[8], /* array */[
                      3,
                      9,
                      8,
                      9,
                      10,
                      9,
                      4,
                      9,
                      99,
                      -1,
                      8
                    ]);
                return Jest.Expect.toEqual($$Array.map((function (prim) {
                                  return String(prim);
                                }), /* array */[1]), Jest.Expect.expect(match[1]));
              }));
        Jest.test("part2 test position doesnt equal 8", (function (param) {
                var match = Day09$Adventofcode2019.Part1.make(undefined, undefined, /* array */[10], /* array */[
                      3,
                      9,
                      8,
                      9,
                      10,
                      9,
                      4,
                      9,
                      99,
                      -1,
                      8
                    ]);
                return Jest.Expect.toEqual($$Array.map((function (prim) {
                                  return String(prim);
                                }), /* array */[0]), Jest.Expect.expect(match[1]));
              }));
        Jest.test("part2 test immediate equals 8", (function (param) {
                var match = Day09$Adventofcode2019.Part1.make(undefined, undefined, /* array */[8], /* array */[
                      3,
                      3,
                      1108,
                      -1,
                      8,
                      3,
                      4,
                      3,
                      99
                    ]);
                return Jest.Expect.toEqual($$Array.map((function (prim) {
                                  return String(prim);
                                }), /* array */[1]), Jest.Expect.expect(match[1]));
              }));
        Jest.test("part2 test immediate doesnt equal 8", (function (param) {
                var match = Day09$Adventofcode2019.Part1.make(undefined, undefined, /* array */[10], /* array */[
                      3,
                      3,
                      1108,
                      -1,
                      8,
                      3,
                      4,
                      3,
                      99
                    ]);
                return Jest.Expect.toEqual($$Array.map((function (prim) {
                                  return String(prim);
                                }), /* array */[0]), Jest.Expect.expect(match[1]));
              }));
        Jest.test("part 2 test large value", (function (param) {
                var match = Day09$Adventofcode2019.Part1.make(undefined, undefined, /* array */[7], /* array */[
                      3,
                      21,
                      1008,
                      21,
                      8,
                      20,
                      1005,
                      20,
                      22,
                      107,
                      8,
                      21,
                      20,
                      1006,
                      20,
                      31,
                      1106,
                      0,
                      36,
                      98,
                      0,
                      0,
                      1002,
                      21,
                      125,
                      20,
                      4,
                      20,
                      1105,
                      1,
                      46,
                      104,
                      999,
                      1105,
                      1,
                      46,
                      1101,
                      1000,
                      1,
                      20,
                      4,
                      20,
                      1105,
                      1,
                      46,
                      98,
                      99
                    ]);
                return Jest.Expect.toEqual($$Array.map((function (prim) {
                                  return String(prim);
                                }), /* array */[999]), Jest.Expect.expect(match[1]));
              }));
        Jest.test("part 2 test large value", (function (param) {
                var match = Day09$Adventofcode2019.Part1.make(undefined, undefined, /* array */[8], /* array */[
                      3,
                      21,
                      1008,
                      21,
                      8,
                      20,
                      1005,
                      20,
                      22,
                      107,
                      8,
                      21,
                      20,
                      1006,
                      20,
                      31,
                      1106,
                      0,
                      36,
                      98,
                      0,
                      0,
                      1002,
                      21,
                      125,
                      20,
                      4,
                      20,
                      1105,
                      1,
                      46,
                      104,
                      999,
                      1105,
                      1,
                      46,
                      1101,
                      1000,
                      1,
                      20,
                      4,
                      20,
                      1105,
                      1,
                      46,
                      98,
                      99
                    ]);
                return Jest.Expect.toEqual($$Array.map((function (prim) {
                                  return String(prim);
                                }), /* array */[1000]), Jest.Expect.expect(match[1]));
              }));
        Jest.test("part 2 test large value", (function (param) {
                var match = Day09$Adventofcode2019.Part1.make(undefined, undefined, /* array */[9], /* array */[
                      3,
                      21,
                      1008,
                      21,
                      8,
                      20,
                      1005,
                      20,
                      22,
                      107,
                      8,
                      21,
                      20,
                      1006,
                      20,
                      31,
                      1106,
                      0,
                      36,
                      98,
                      0,
                      0,
                      1002,
                      21,
                      125,
                      20,
                      4,
                      20,
                      1105,
                      1,
                      46,
                      104,
                      999,
                      1105,
                      1,
                      46,
                      1101,
                      1000,
                      1,
                      20,
                      4,
                      20,
                      1105,
                      1,
                      46,
                      98,
                      99
                    ]);
                return Jest.Expect.toEqual($$Array.map((function (prim) {
                                  return String(prim);
                                }), /* array */[1001]), Jest.Expect.expect(match[1]));
              }));
        Jest.test("part1 latest", (function (param) {
                var instructions = /* array */[
                  109,
                  1,
                  204,
                  -1,
                  1001,
                  100,
                  1,
                  100,
                  1008,
                  100,
                  16,
                  101,
                  1006,
                  101,
                  0,
                  99
                ];
                var match = Day09$Adventofcode2019.Part1.make(undefined, undefined, /* array */[0], instructions);
                return Jest.Expect.toEqual($$Array.map((function (prim) {
                                  return String(prim);
                                }), instructions), Jest.Expect.expect(match[1]));
              }));
        Jest.test("part1 large number", (function (param) {
                var instructions = /* array */[
                  104,
                  0,
                  99
                ];
                var match = Day09$Adventofcode2019.Part1.make(undefined, undefined, /* array */[0], instructions);
                return Jest.Expect.toEqual($$Array.map((function (prim) {
                                  return String(prim);
                                }), /* array */[0]), Jest.Expect.expect(match[1]));
              }));
        return Jest.test("part1 large number 2", (function (param) {
                      var instructions = /* array */[
                        1102,
                        34915192,
                        34915192,
                        7,
                        4,
                        7,
                        99,
                        0
                      ];
                      var match = Day09$Adventofcode2019.Part1.make(undefined, undefined, /* array */[0], instructions);
                      return Jest.Expect.toEqual(16, Jest.Expect.expect(Caml_array.caml_array_get(match[1], 0).split("").length));
                    }));
      }));

/*  Not a pure module */
