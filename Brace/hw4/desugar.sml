structure Desugar : sig

  val term : AST.term -> ULC.term

end = struct

  fun term t = (case t
    of (AST.Let (s, t1, t2)) => Subst.subst(s, term t1, term t2)
    | (AST.Var s) => ULC.Var s
    | (AST.Abs (s, t)) => ULC.Abs(s, term t)
    | (AST.App (t1, t2)) => ULC.App(term t1, term t2))

(* Notice how helpful the type system is for implementing this
 * operation. This is SML at its very best.
 *)

end
