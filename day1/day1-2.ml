let file = "input.txt"

(* Process file into lst *)
let rec process_file_rec fs acc =
  match In_channel.input_line fs with
  | Some line -> (line::(process_file_rec fs acc))
  | None -> acc

let process_file filename =
  let fs = (open_in filename) in
  (process_file_rec fs [])

(* Mod function that behaves like I want Img(f) = (0,100] *)

let rec hmod num =
  if num > 100 then (hmod (num-100))
  else if num < -100 then (hmod (num+100))
  else if num >= 0 then num else (100+num)


(* convert string into directional data *)
let process_line line =
  if line.[0] = 'R'
  then
    int_of_string (String.sub line 1 ((String.length line)-1) )
  else
    -(int_of_string (String.sub line 1 ((String.length line)-1)))

let calc_passes start step =
  let cycles =  step/100 in
  let tail = (hmod step) in
  let stop = (start+tail) in
  (Printf.printf "%d\n" stop;
  if ((hmod stop) = 0) then (1 + cycles)
  else if (99 < stop) then (1 + cycles)
  else 0)



let rec part_2_rec lst pos =
  match lst with
  | h::t -> let x = (process_line h) in
            let y = (calc_passes pos x) in
              (y + (part_2_rec t (hmod (pos + x)) ))
  | [] -> 0

let part_2 lst =
  (part_2_rec lst 50)



let () =
  let x = (process_file file) in
  (Printf.printf "%d" (part_2 x))
