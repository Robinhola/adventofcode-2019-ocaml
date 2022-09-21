open! Base
open! Core
(*
--- Day 3: Crossed Wires ---

The gravity assist was successful, and you're well on your way to the Venus
refuelling station. During the rush back on Earth, the fuel management system
wasn't completely installed, so that's next on the priority list.

Opening the front panel reveals a jumble of wires. Specifically, two wires are
connected to a central port and extend outward on a grid. You trace the path
each wire takes as it leaves the central port, one wire per line of text (your
puzzle input).

The wires twist and turn, but the two wires occasionally cross paths. To fix
the circuit, you need to find the intersection point closest to the central
port. Because the wires are on a grid, use the Manhattan distance for this
measurement. While the wires do technically cross right at the central port
where they both start, this point does not count, nor does a wire count as
crossing with itself.

For example, if the first wire's path is R8,U5,L5,D3, then starting from the
central port (o), it goes right 8, up 5, left 5, and finally down 3:

...........
...........
...........
....+----+.
....|....|.
....|....|.
....|....|.
.........|.
.o-------+.
...........
Then, if the second wire's path is U7,R6,D4,L4, it goes up 7, right 6, down 4,
and left 4:

...........
.+-----+...
.|.....|...
.|..+--X-+.
.|..|..|.|.
.|.-X--+.|.
.|..|....|.
.|.......|.
.o-------+.
...........
These wires cross at two locations (marked X), but the lower-left one is closer
to the central port: its distance is 3 + 3 = 6.

Here are a few more examples:

R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83 = distance 159
R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135
What is the Manhattan distance from the central port to the closest intersection?
*)

let lines =
  try List.map (Stdio.In_channel.read_lines "input/day03.in") ~f:(fun line -> String.split ~on:',' line) with 
  _ -> []
;;

(* let lines = [["R8";"U5";"L5";"D3"]; ["U7";"R6";"D4";"L4"]] in  *)

module T : sig
  include Day.T

end = struct
  let name () = print_endline "--- Day 3: Crossed Wires ---"

  module Coord = struct 
    type t = int * int [@@deriving sexp, compare]
    let from_char c = match c with
    | 'U' -> (0, 1)
    | 'R' -> (1, 0)
    | 'D' -> (0, -1)
    | 'L' -> (-1, 0)
    | _ -> Error.raise_s [%message (c: char)]

    let distance t = 
      let x ,y = t in 
      abs x + abs y 
    ;;
  end

  module CoordMap = Map.Make(Coord) [@@deriving sexp]

  let%expect_test "distance" =
  let t1 = (5, 5) in
  let t2 = (-5, 5) in
  printf "%d %d" (Coord.distance t1) (Coord.distance t2);
  [%expect {| 10 10 |}]


  let part1 () =
    let rec make_move history position ~direction ~move =
      let px, py = position in
      let dx, dy = direction in 
      match move with 
      | 0 -> history, position
      | _ -> (
        let position = px + dx, py + dy in 
        let history = Map.add_multi history ~key:position ~data:true in
        let move = move - 1 in 
        make_move history position ~direction ~move
      )
    in 
    let rec move_wire history position moves =
      match moves with 
      | [] -> history
      | m :: moves -> 
        let direction = Coord.from_char (Char.of_string(String.sub m ~pos:0 ~len:1)) in
        let move = Int.of_string (String.sub m ~pos:1 ~len:((String.length m) - 1)) in 
        let history, position = make_move history position ~direction ~move in
        move_wire history position moves;
    in
    let find_wire_positions id = 
      let wire_moves = List.nth_exn lines id in 
      let wire_positions  = Map.key_set (move_wire CoordMap.empty (0, 0) wire_moves) in
      wire_positions
    in
    let intersections = Set.to_list (Set.inter (find_wire_positions 0) (find_wire_positions 1)) in
    let closest_intersection = List.min_elt intersections ~compare:(fun a b -> Coord.distance a - Coord.distance b) in
    printf "part1:\t%i\n" (Coord.distance (Option.value_exn closest_intersection))
  ;;

  let part2 () =
    printf "part2:\t%i\n" (0)
  ;;

end
