open! Base
open! Core

let () = print_endline "Welcome to 2019! Let the advent of code... begin!!!"

let solve (module M: Advent_2019.Day.T) = 
  let () = M.name () in 
  let () = M.part1 () in 
  let () = M.part2 () in 
  ()
;;

let () = solve (module Advent_2019.Day01.T)
let () = solve (module Advent_2019.Day02.T)
let () = solve (module Advent_2019.Day03.T)
let () = solve (module Advent_2019.Day04.T)