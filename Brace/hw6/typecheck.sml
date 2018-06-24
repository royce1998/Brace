structure TypeCheck : sig

  val typeof : AST.term -> Ty.ty
  val check  : AST.term -> unit

end = struct

  structure A = AST

  structure TypeEnv : sig
    type env   = (string * Ty.ty) list
    val empty  : env
    val lookup : string * env -> Ty.ty option
    val extend : env * string * Ty.ty -> env
  end = struct
    type env   = (string * Ty.ty) list
    val empty = []
    fun lookup (x:string, []) = NONE
      | lookup (x, (y,ty)::gamma') =
	  if x=y
	  then SOME ty
	  else lookup (x, gamma')
    fun extend (gamma,x,t) = (x,t)::gamma
  end

  infix <+>
  fun g <+> (x,ty) = TypeEnv.extend (g, x, ty)

  fun typeof t =
    let
      fun lp (g, A.Var x) =
           (case TypeEnv.lookup (x, g)
	      of SOME ty => ty
	       | NONE => raise Fail ("type error: free variable " ^ x))
	| lp (g, A.Abs (x, ty1, t1)) = Ty.Function(ty1, typeof t1)
	| lp (g, A.App (t1, t2)) = (case (typeof t1) of
    (Ty.Function (ty1, ty2))  => if (Ty.eq (ty1, typeof t2)) then ty2 else raise Fail "Function input type mismatch"
    | _ => raise Fail "App typing error")
	| lp (g, A.Let (x, t1, t2)) = (case (typeof t1) of _ => (typeof t2))
	| lp (_, A.Unit) = Ty.Unit
	| lp (_, A.True) = Ty.Bool
	| lp (_, A.False) = Ty.Bool
	| lp (g, A.Not t1) =
	    if Ty.eq (Ty.Bool, lp (g, t1))
	    then Ty.Bool
	    else raise Fail "type error: not applied to non-bool"
	| lp (g, A.If (t1, t2, t3)) =
	    (case (lp (g, t1), lp (g, t2), lp (g, t3))
	       of (Ty.Bool, ty2, ty3) =>
	            if Ty.eq (ty2, ty3)
		    then ty2
                    else raise Fail "type error: type mismatch in if branches"
		| _ => raise Fail "type error: non-bool test in if")
        | lp (g, A.Alloc t1) = Ty.Ref (typeof t1)
	| lp (g, A.Read t1) = (case t1 of (A.Location loc) => (typeof (Store.read loc)) | _ => (raise Fail "Need location ref in read"))
        | lp (g, A.Write (t1, t2)) = (case t1 of (A.Location loc) => if (Ty.eq (typeof (Store.read loc), typeof t2)) then Ty.Unit else (raise Fail "Writing wrong type to location") | _ => (raise Fail "Need reference cell in Write"))
	| lp (_, A.Location _) = raise Fail "BUG: there no locations in surface language; this shouldn't happen"
        | lp (g, A.Record items) = (Ty.Record (List.map (fn i => (case i of (str, t) => (str, typeof t))) items))
	| lp (g, A.Select (label, t1)) = (case t1 of A.Record [] => raise Fail "No such label in record"
    | A.Record ((str, t) :: rs) => if (str = label) then (typeof t) else (typeof (A.Select (label, A.Record rs)))
    | _ => raise Fail "Can only select from record")
	| lp (g, A.Sequence (terms:A.term list)) = (case terms of (t :: []) => (typeof t)
    | (t :: t' :: ts) => (case (typeof t) of Ty.Function (ty1, ty2) => (if (Ty.eq (ty1, typeof t')) then (typeof (A.Sequence (t' :: ts))) else raise Fail "Function input type error")
      | _ => raise Fail "Need function at beginning of sequence")
    | _ => raise Fail "Empty sequence")
    in
      lp (TypeEnv.empty, t)
    end

  fun check t = (typeof t; ())

end
