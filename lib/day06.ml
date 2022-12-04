open! Base
open! Core


module Orbiting_object = struct
  type t = 
  { name : string
  ; is_orbited_by : string list
  ; orbit_around : string list 
  } [@@deriving sexp_of]

  let default name = { name; is_orbited_by = []; orbit_around = [] }

  let get_or_create orbiting_objects name =
    (* print_s [%message (name: string) (orbiting_objects: t String.Map.t)]; *)
    Map.find orbiting_objects name
    |> Option.value ~default:(default name)
  ;;

  let rec add_to orbiting_objects = function 
    | [] -> orbiting_objects
    | o :: rest -> 
      let orbiting_objects = Map.set orbiting_objects ~key:o.name ~data:o in
      add_to orbiting_objects rest
  ;;

  let add_to_is_orbited_by orbiting_objects ~name ~by =
    let t = get_or_create orbiting_objects name in
    let t = { t with is_orbited_by = by :: t.is_orbited_by} in 
    let by = get_or_create orbiting_objects by in 
    let by = { by with orbit_around = name :: by.is_orbited_by} in 
    add_to orbiting_objects [ t; by ]
  ;;

  let add_to_orbit_around orbiting_objects ~name ~around =
    let t = get_or_create orbiting_objects name in
    let t = { t with orbit_around = around :: t.orbit_around} in 
    let around = get_or_create orbiting_objects around in 
    let around = { around with orbit_around = name :: around.orbit_around} in 
    add_to orbiting_objects [ t; around ]
  ;;

  let update orbiting_objects line =
    match line |> String.split ~on:')' with
    | heavy :: light :: [] -> 
      orbiting_objects
      |> add_to_is_orbited_by ~name:heavy ~by:light
      |> add_to_orbit_around ~name:light ~around:heavy
    | _ -> failwith "unsupported"
  ;;

  let rec count orbiting_objects distance name =
    (* print_s [%message (name : string)]; *)
    let t = get_or_create orbiting_objects name in
    match t.is_orbited_by with
    | [] -> distance
    | satellites -> satellites |>
      List.fold ~init:distance ~f:(fun accum s ->
        let distance = distance + 1 in
        accum + count orbiting_objects distance s
      )
  ;;

  let rec breath_first_search orbiting_objects ~start ~goal ~distance ~to_visit ~seen =
    if phys_equal start goal then Some distance
    else 
      let distance = distance + 1 in
      let t = start |> Map.find_exn orbiting_objects in 
      t.orbit_around @ t.is_orbited_by
      |> List.filter ~f:(fun e -> not (Set.exists seen ~f:(phys_equal e)))
      |> List.iter ~f:(Queue.enqueue to_visit);
      let element = ref None in
      while not (Queue.is_empty to_visit) do
        let start = Queue.dequeue_exn to_visit in
        let seen = start |> Set.add seen in
        element := breath_first_search orbiting_objects ~start ~goal ~distance ~to_visit ~seen;
      done;
      !element
  ;;
end

let lines = 
  Stdio.In_channel.read_lines "input/day06.in"
;;

let rec read_lines orbiting_objects = function
  | [] -> orbiting_objects
  | line :: rest ->
    let orbiting_objects = Orbiting_object.update orbiting_objects line in
    rest |> read_lines orbiting_objects
;;

module T : sig
  include Day.T
end = struct
  let name () = print_endline "--- Day 6: Universal Orbit Map ---"

  let orbiting_objects = read_lines String.Map.empty lines

  let part1 () =
    printf "part1:\t%i\n" (Orbiting_object.count orbiting_objects 0 "COM")
  ;;

  let part2 () =
    let start = "YOU" in
    let goal = "SAN" in
    let distance = Orbiting_object.breath_first_search orbiting_objects ~start ~goal ~distance:0 ~to_visit:(Queue.create ()) ~seen:String.Set.empty in
    printf "part2:\t%i\n" (distance |> Option.value_exn)
  ;;
end