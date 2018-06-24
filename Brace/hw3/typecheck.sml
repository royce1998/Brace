structure TypeCheck : sig

  val typeof : AST.term -> Ty.ty
  val check  : AST.term -> unit

end = struct

  structure A = AST

(* A module within a module! *)
(* Typing environments track which variable has what type. *)
  structure TypeEnv : sig
    type env   = (string * Ty.ty) list
    val empty  : env
    val lookup : string * env -> Ty.ty option
    val extend : env * string * Ty.ty -> env
  end = struct
    type env   = (string * Ty.ty) list
    val empty = []
    fun lookup (str : string, e : env) : Ty.ty option =
      (case e
        of [] => NONE
        | (e :: es) => if (#1 e = str) then SOME(#2 e) else lookup(str, es))
    fun extend (e : env, str : string, t : Ty.ty) : env =
      ((str, t) :: e)
  end

(* A convenience: infix notation for extending typing environments. *)
  infix <+>
  fun gamma <+> (x,ty) = TypeEnv.extend (gamma, x, ty)

  fun typeof t =
    let
      (* lp is an internal function that has a typing environment argument as well as a term *)
      (* The typing environment is capital gamma in the formal presentation (and "g" here). *)
      fun lp (g, A.True) = Ty.Bool
        | lp (g, A.False) = Ty.Bool
        | lp (g, A.Zero) = Ty.Nat
        | lp (g, A.If(t1, t2, t3)) = (case (lp (g, t1), lp(g, t2), lp(g, t3))
          of (Ty.Bool, t2, t3) => if Ty.equal(t2, t3) then t2 else raise Fail "Unequal types in if"
          | _ => raise Fail "if Fail")
        | lp (g, A.Succ t) = if Ty.equal(lp (g, t), Ty.Nat) then Ty.Nat else raise Fail "Succ fail"
        | lp (g, A.Pred t) = if Ty.equal(lp (g, t), Ty.Nat) then Ty.Nat else raise Fail "Pred fail"
        | lp (g, A.IsZero t) = if Ty.equal(lp (g, t), Ty.Nat) then Ty.Bool else raise Fail "IsZ fail"
        | lp (g, A.And(t1, t2)) = if Ty.equal(lp (g, t1), Ty.Bool) andalso Ty.equal(lp (g, t2), Ty.Bool) then Ty.Bool else raise Fail "And fail"
        | lp (g, A.Or(t1, t2)) = if Ty.equal(lp (g, t1), Ty.Bool) andalso Ty.equal(lp (g, t2), Ty.Bool) then Ty.Bool else raise Fail "Or fail"
        | lp (g, A.Not t) = if Ty.equal(lp (g, t), Ty.Bool) then Ty.Bool else raise Fail "Not fail"
        | lp (g, A.Equal(t1, t2)) = if Ty.equal(lp (g, t1), lp (g, t2)) then Ty.Bool else raise Fail "Equal fail"
        | lp (g, A.Unit) = Ty.Unit
        | lp (g, A.Var str) = (case TypeEnv.lookup(str, g)
          of NONE => raise Fail "unbound variable"
          | SOME t => t)
        | lp (g, A.Let(str, t1, t2)) = (lp ((g<+>(str, lp (g, t1))), t2))
        | lp (g, A.Pair(t1, t2)) = Ty.Pair((lp (g, t1)), (lp (g, t2)))
        | lp (g, A.Select1 t) = (case t
          of A.Pair(t1, t2) => (lp (g, t1))
          | _ => raise Fail "Need a pair fail")
        | lp (g, A.Select2 t) = (case t
          of A.Pair(t1, t2) => (lp (g, t2))
          | _ => raise Fail "Need a pair fail")
        | lp (g, A.Some t) = Ty.Opt(lp (g, t))
        | lp (g, A.None ty) = Ty.Opt(ty)
        | lp (g, A.OptCase(t1, (str, t2), t3)) = (case t1
          of A.Some t1' =>
            let
              val ty = (lp ((g<+>(str, (lp (g, t1')))), t2))
            in
              if Ty.equal(ty, lp (g, t3)) then ty else raise Fail "different types in OptCase"
            end
          | A.None _ => if Ty.equal((lp (g,t3)),(lp (g, t2))) then (lp (g, t3)) else raise Fail "different types in OptCase"
          | _ => raise Fail "OptCase must have Some or None")
    in
      lp (TypeEnv.empty, t)
    end

(* ignore is a build-in function of type 'a -> unit *)
  fun check t = ignore (typeof t)

end
