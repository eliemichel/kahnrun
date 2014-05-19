module L = Kahn.Lib (Kahn.Mono)
open L


let f x = x * x

let nTasks = 1000
let nWorkers = 10
let groupSize = 3

let tab = Array.init nTasks (fun i -> i)

let computation i = (i, f i)

let handler (i, fi) = 
  tab.(i) <- fi;
  print_int fi; 
  print_newline ()


let tasks = range 0 (nTasks - 1)

let scheduler = make_scheduler nWorkers groupSize computation handler tasks

let () = Kahn.Mono.run scheduler
