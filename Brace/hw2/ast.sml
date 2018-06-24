structure AST : sig

  datatype term
    = True
    | False
    | Zero
    | If of term * term * term
    | Succ of term
    | Pred of term
    | IsZero of term
    | And of term * term
    | Or of term * term
    | Not of term
    | Equal of term * term

  val unparse : term -> string
  val equal : term * term -> bool

  val isValue : term -> bool
  val isNumericValue : term -> bool

end = struct

  datatype term
    = True
    | False
    | Zero
    | If of term * term * term
    | Succ of term
    | Pred of term
    | IsZero of term
    | And of term * term
    | Or of term * term
    | Not of term
    | Equal of term * term

  fun unparse (t : term) : string =
    (case t
      of True => "T"
      | False => "F"
      | Zero => "0"
      | If (t1, t2, t3) => "{if " ^ unparse(t1) ^ " then " ^ unparse(t2) ^ " else " ^ unparse(t3) ^ "}"
      | Succ t' => "{+1 " ^ unparse(t') ^ "}"
      | Pred t' => "{-1 " ^ unparse(t') ^ "}"
      | IsZero t' => "{isz " ^ unparse(t') ^ "}"
      | And (t1, t2) => "{&& " ^ unparse(t1) ^ " " ^ unparse(t2) ^ "}"
      | Or (t1, t2) => "{|| " ^ unparse(t1) ^ " " ^ unparse(t2) ^ "}"
      | Not t' => "{! " ^ unparse(t') ^ "}"
      | Equal (t1, t2) => "{== " ^ unparse(t1) ^ " " ^ unparse(t2) ^ "}")

  fun equal (t1 : term, t2 : term) : bool = (if (unparse(t1) = unparse(t2)) then true else false)

  fun isNumericValue (t : term) =
    (case t
      of Zero => true
      | Succ t' => isNumericValue(t')
      | Pred t' => isNumericValue(t')
      | _ => false)

  fun isValue (t : term) : bool =
    (case t
      of True => true
      | False => true
      | _ => isNumericValue(t))

end
