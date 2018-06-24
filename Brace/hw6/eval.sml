structure Eval : sig

  val eval  : AST.term -> AST.term
  val steps : AST.term -> AST.term list

end = struct

  structure A = AST

  fun step t =
    let
      fun lp (A.Var x) = NONE
        | lp (A.Abs _) = NONE
	| lp (A.App (t1, t2)) = (case (step t1) of SOME x => SOME (A.App (x, t2))
    | _ => (case (step t2) of SOME x => SOME (A.App (t1, x))
      | _ => (case t1 of A.Abs (str, _, t') => SOME (Subst.subst {replaceThis=str, withThis=t2, inThis=t'})
        | _ => NONE)))
        | lp (A.Let (x, t1, t2)) = if (A.isValue t1) then (SOME (Subst.subst {replaceThis=x, withThis=t1, inThis=t2})) else (case step t1 of SOME t1' => SOME (A.Let (x, t1', t2))
          | _ => NONE)
        | lp A.Unit = NONE
	| lp A.True = NONE
	| lp A.False = NONE
	| lp (A.Not t1) =
	    (case t1
	       of A.True  => SOME A.False
	        | A.False => SOME A.True
		| _ => (case lp t1
		          of SOME t1' => SOME (A.Not t1')
			   | NONE => NONE))
        | lp (A.If (t1, t2, t3)) =
	    (case t1
	       of A.True => SOME t2
	        | A.False => SOME t3
		| _ => (case lp t1
		          of SOME t1' => SOME (A.If (t1', t2, t3))
			   | NONE => NONE))
        | lp (A.Alloc t1) = (case step t1 of SOME t1' => SOME (A.Alloc t1')
          | NONE => SOME (A.Location (Store.malloc t1)))
        | lp (A.Read t1) = (case step t1 of SOME t1' => SOME (A.Read t1')
          | NONE => (case t1 of A.Location l => SOME (Store.read l) | _ => NONE))
        | lp (A.Write (t1, t2)) = (case step t1 of SOME t1' => SOME (A.Write (t1', t2))
          | _ => (case step t2 of SOME t2' => SOME (A.Write (t1, t2'))
            | _ => (case t1 of (A.Location l) => (
              let
                val _ = Store.write (l, t2)
              in
                SOME A.Unit
              end
              ) | _ => NONE)))
        | lp (A.Location _) = NONE
        | lp (A.Record items) = (case items of [] => NONE
          | ((str, item) :: []) => (case step item of SOME item' => SOME (A.Record ((str, item') :: [])) | NONE => NONE)
          | ((str, item) :: items) => (case step item of SOME item' => SOME (A.Record ((str, item') :: items)) | NONE => (case step (A.Record items) of NONE => NONE | SOME record' => (case record' of (A.Record items') => SOME (A.Record ((str, item) :: items')) | _ => NONE))))
	| lp (A.Select (label, t1)) = (case t1 of (A.Record []) => NONE
    | (A.Record ((str, t) :: rs)) => if (str = label) then (SOME t) else (step (A.Select (label, (A.Record rs))))
    | _ => NONE)
	| lp (A.Sequence ts) = (case ts of (t1 :: ts) => (case step t1 of SOME t1' => SOME (A.Sequence (t1' :: ts)) | NONE => (case ts of [] => SOME t1 | _ => (case t1 of A.Unit => SOME (A.Sequence ts) | _ => NONE)))
  | _ => NONE)
    in
      lp t
    end

  fun eval t =
    let
      val _ = Store.clear()
      fun lp t =
        (case step t
           of SOME t' => lp t'
	    | NONE => t)
    in
      lp t
    end

  fun steps t =
    let
      val _ = Store.clear()
      fun lp t =
        (case step t
           of SOME t' => t :: lp t'
            | NONE => [t])
    in
      lp t
    end

end
