let file = "input.txt"


(* Process file into lst *)
let rec process_file_rec fs acc =
  match In_channel.input_line fs with
  | Some line -> (line::(process_file_rec fs acc))
  | None -> acc

let process_file filename =
  let fs = (open_in filename) in
  (process_file_rec fs [])

(* sol p1 *)
let process_line line =
  if line.[0] = 'R'
  then
    int_of_string (String.sub line 1 ((String.length line)-1) )
  else
    100-(int_of_string (String.sub line 1 ((String.length line)-1)))


let rec part_1_rec lst pos =
  match lst with
  | h::t -> let x = (process_line h) in let y = ((pos + x) mod 100) in (if y = 0 then (1 + (part_1_rec t y)) else ((Printf.printf "%d\n" y);(part_1_rec t y)) )
  | [] -> 0

let part_1 lst =
  (part_1_rec lst 50)



let () =
  let x = (process_file file) in
  (List.iter (Printf.printf "%s ") x ; Printf.printf "%d" (part_1 x))
