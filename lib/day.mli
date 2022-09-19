open! Base
open! Core

module type T = sig
  val name : unit -> unit 
  val part1 : unit -> unit 
  val part2 : unit -> unit 
end