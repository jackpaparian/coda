(* receipt.mli *)

[%%import "/src/config.mlh"]

open Core_kernel

[%%ifdef consensus_mechanism]

open Snark_params.Tick

[%%else]

open Snark_params_nonconsensus

[%%endif]

module Chain_hash : sig
  include Data_hash.Full_size

  include Codable.S with type t := t

  val gen : t Quickcheck.Generator.t

  val to_string : t -> string

  val of_string : string -> t

  val empty : t

  val cons : User_command.Payload.t -> t -> t

  module Checked : sig
    val constant : t -> var

    type t = var

    val if_ : Boolean.var -> then_:t -> else_:t -> (t, _) Checked.t

    val cons : payload:Transaction_union_payload.var -> t -> (t, _) Checked.t
  end
end
