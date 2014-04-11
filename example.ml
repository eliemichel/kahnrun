module Example (K : Kahn.S) = struct
  module K = K
  module Lib = Kahn.Lib(K)
  open Lib

  let integers (qo : int K.out_port) : unit K.process =
    let rec loop n =
      (K.put n qo) >>= (fun () -> loop (n + 1))
    in
    Format.printf "integers@.";
    loop 2

  let output (qi : int K.in_port) : unit K.process =
    let rec loop () =
    	Format.printf "output !@.";
      (K.get qi) >>= (fun v -> if v = 1000000 then ignore(1/0) else Format.printf "%d@." v; loop ())
    in
    Format.printf "output@.";
    loop ()

  let main : unit K.process =
    (delay K.new_channel ()) >>=
    (fun (q_in, q_out) -> K.doco [ integers q_out ; output q_in ; ])

end

module E = Example(Kahn.Proc)

let () = E.K.run E.main
