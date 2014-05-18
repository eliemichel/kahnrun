module Example (K : Handcut.S) = struct
  module K = K
  module Lib = Handcut.Lib(K)
  open Lib

  let main : unit K.process =
    (delay K.new_channel ()) >>=
    (fun (q_in, q_out) -> K.doco [ K.import ("integers", q_out) ; K.export ("output", q_in) ])

end

module E = Example(Handcut.Node)

let () = E.K.run E.main
