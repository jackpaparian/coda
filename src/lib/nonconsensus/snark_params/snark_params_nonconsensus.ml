(* snark_params_nonconsensus.ml *)

[%%import
"/src/config.mlh"]

[%%ifdef
consensus_mechanism]

[%%error
"Snark_params_nonconsensus cannot be used when building code for consensus"]

[%%endif]

open Snarkette

[%%if
curve_size = 298]

module Field0 = Mnt6_80.Fq
module G1 = Mnt6_80.G1

[%%elif
curve_size = 753]

module Field0 = Mnt6753.Fq
module G1 = Mnt6753.G1

[%%else]

[%%show
curve_size]

[%%error
"invalid value for \"curve_size\""]

[%%endif]

[%%inject
"ledger_depth", ledger_depth]

module Field = struct
  include Field0

  let size = order |> Nat.to_string |> Bigint.of_string

  let size_in_bits = length_in_bits

  let unpack t = to_bits t

  let project bits =
    Core_kernel.Option.value_exn ~message:"project: invalid bits"
      (of_bits bits)
end

module Inner_curve = struct
  open Field
  module Coefficients = G1.Coefficients

  let find_y (x : Field.t) =
    let y2 = (x * square x) + (Coefficients.a * x) + Coefficients.b in
    if is_square y2 then Some (sqrt y2) else None

  [%%define_locally
  G1.(to_affine_exn, of_affine)]
end
