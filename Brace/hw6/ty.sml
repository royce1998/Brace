structure Ty : sig

  datatype ty
    = Bool
    | Unit
    | Function of ty * ty
    | Ref of ty
    | Record of (string * ty) list

  val tos : ty -> string
  val eq  : ty * ty -> bool

  val subtype : ty * ty -> bool

end = struct

  datatype ty
    = Bool
    | Unit
    | Function of ty * ty
    | Ref of ty
    | Record of (string * ty) list

  val spc = String.concatWith " "

  fun tos Bool = "Bool"
    | tos Unit = "Unit"
    | tos (Function (t1, t2)) = "(-> " ^ tos t1 ^ " " ^ tos t2 ^ ")"
    | tos (Ref t1) = "(Ref " ^ tos t1 ^ ")"
    | tos (Record items) = "{" ^ record items ^ "}"
  and record items = String.concatWith "," (List.map (fn (l,t) => l^":"^(tos t)) items)

  fun eq (t1:ty, t2) = (t1=t2)

  fun getRecord (r, str) = (case r of Record [] => NONE
    | Record ((str', ty') :: ts) => if (str = str') then (SOME ty') else (getRecord (Record ts, str))
    | _ => NONE)

(* Return true if t1 is a subtype of t2, false otherwise. *)
  fun subtype (t1, t2) =
    (eq (t1, t2)) orelse
    (case t2 of Record [] => true
      | Record ((str, ty) :: ts) => (case (getRecord (t2, str)) of NONE => false
        | SOME ty' => (subtype (ty, ty'))) andalso (subtype (t1, Record ts))
      | Function (t1, t2) => (case t1 of Function (t1', t2') => (subtype (t1, t1')) andalso (subtype (t2', t2))
        | _ => false)
      | Ref t => (case t1 of Ref s => (subtype (t, s)) andalso (subtype (s, t))
        | _ => false)
      | _ => false)

end
