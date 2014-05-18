module Example (K : Handcut.S) = struct
  module K = K
  module Lib = Handcut.Lib(K)
  open Lib

  let integers (qo : int K.out_port) : unit K.process =
    let rec loop n =
      (K.put n qo) >>= (fun () -> loop (n + 1))
    in
    loop 2

  let main : unit K.process =
    (delay K.new_channel ()) >>=
    (fun (q_in, q_out) -> K.doco [ integers q_out ; K.export ("integers", q_in) ])

end

module E = Example(Handcut.Node)

let () = E.K.run E.main
