module Example (K : Handcut.S) = struct
  module K = K
  module Lib = Handcut.Lib(K)
  open Lib

  let main : unit K.process =
    (delay K.new_channel ()) >>=
    (fun (q_in, q_out) -> K.doco [ delay K.export ("integers", q_out) ; delay K.import ("output", q_in) ; ])

end

module E = Example(Handcut.Master)

let () = E.K.run E.main
