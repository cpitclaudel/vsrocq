(**************************************************************************)
(*                                                                        *)
(*                                 VSRocq                                  *)
(*                                                                        *)
(*                   Copyright INRIA and contributors                     *)
(*       (see version control and README file for authors & dates)        *)
(*                                                                        *)
(**************************************************************************)
(*                                                                        *)
(*   This file is distributed under the terms of the MIT License.         *)
(*   See LICENSE file.                                                    *)
(*                                                                        *)
(**************************************************************************)
open Lsp.Types
open Sexplib.Std
open Printing

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Position = struct

  include Lsp.Types.Position

  type t = [%import: Lsp.Types.Position.t] [@@deriving sexp]
 
  let compare pos1 pos2 =
    match Int.compare pos1.line pos2.line with
    | 0 -> Int.compare pos1.character pos2.character
    | x -> x

  let to_string pos = Format.sprintf "(%i,%i)" pos.line pos.character

end

module Range = struct

  include Lsp.Types.Range

  type t = [%import: Lsp.Types.Range.t] [@@deriving sexp]
  
  let top () =
    let start = Position.{ line=0; character=0} in
    let end_ = Position.{ line=0; character=0} in
    Range.{start; end_}

  let compare r1 r2 =
    match Position.compare r1.start r2.start with
    | 0 -> Position.compare r1.end_ r2.end_
    | x -> x

  let equals r1 r2 =
    Position.compare r1.start r2.start == 0 && Position.compare r1.end_ r2.end_ == 0

  let included ~in_ { start; end_ } =
    let (<=) x y = Position.compare x y <= 0 in
    in_.start <= start && end_ <= in_.end_

  let strictly_included ~in_ { start; end_ } =
    let (<) x y = Position.compare x y < 0 in
    in_.start < start && end_ < in_.end_

  let prefixes ~in_ { start; end_ } =
    let (<) x y = Position.compare x y < 0 in
    let (<=) x y = Position.compare x y <= 0 in
    start <= in_.start && end_ < in_.end_ && in_.start <= end_

  let postfixes ~in_ { start; end_ } =
    let (<) x y = Position.compare x y < 0 in
    let (<=) x y = Position.compare x y <= 0 in
    start <= in_.end_ && in_.start < start && in_.end_ < end_

  let to_string range = Format.sprintf ("Range (start: %s, end: %s)") (Position.to_string range.start) (Position.to_string range.end_)

end 

module QuickFixData = struct
  type t = {text: string; range: Range.t} [@@deriving yojson]
end 

module DiagnosticSeverity = struct

  type t = [%import: Lsp.Types.DiagnosticSeverity.t] [@@deriving sexp]

  let yojson_of_t v = Lsp.Types.DiagnosticSeverity.yojson_of_t v
  let t_of_yojson v = Lsp.Types.DiagnosticSeverity.t_of_yojson v

  let of_feedback_level = let open DiagnosticSeverity in function
    | Feedback.Error -> Error
    | Feedback.Warning -> Warning
    | Feedback.(Info | Debug | Notice) -> Information

end

module Feedback = struct
  include Feedback

  type foo = int

  let yojson_of_level = function
  | Debug -> `Int 5
  | Info -> `Int 4
  | Notice -> `Int 3
  | Warning -> `Int 2
  | Error -> `Int 1

  let level_of_yojson = function
  | `Int 5 -> Debug
  | `Int 4 -> Info
  | `Int 3 -> Notice
  | `Int 2 -> Warning
  | _ -> Error
end

type query_result =
  { id : string;
    name : pp;
    statement : pp;
  } [@@deriving yojson]

type overview = {
  uri : DocumentUri.t;
  preparedRange: Range.t list;
  processingRange : Range.t list;
  processedRange : Range.t list;
} [@@deriving yojson]

type notification =
  | QueryResultNotification of query_result
