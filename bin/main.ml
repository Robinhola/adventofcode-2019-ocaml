open! Base
open! Core
open Advent_2019

let () = print_endline "Welcome to 2019! Let the advent of code... begin!!!"

let solve (module M: Day.T) = 
  let () = M.name () in 
  let () = M.part1 () in 
  let () = M.part2 () in 
  ()
;;

let () = solve (module Day01.T)
let () = solve (module Day02.T)
let () = solve (module Day03.T)
let () = solve (module Day04.T)
let () = solve (module Day05.T)
let () = solve (module Day06.T)