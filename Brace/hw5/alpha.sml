structure Alpha : sig

  val equiv : ULC.term * ULC.term -> bool

end = struct

  structure s = Subst

(* Determine if any two terms are the same except for their variable names. *)

(* For example, {a . a} and {b . b} are alpha-equivalent. *)
(* Similarly, {t . {f . f}} and {s . {z . z}} are alpha-equivalent. *)
(* {t . {f . f}} and {@3 . {@4 . @4}} are alpha-equivalent. *)
(* {t . {f . t}} and {s . {z . z}} are not equivalent. *)

  fun varOrder t = (case t
    of ULC.Var s => s :: []
    | ULC.Abs (s, t) => s :: (varOrder t)
    | ULC.App (t1, t2) => (varOrder t1) @ (varOrder t2))

  fun structOrder str =
    if (size str = 0) then "" else
      let
        val s = (String.sub (str, 0))
        val rest = (substring (str, 1, (size str) + 1))
      in
        if (s = #"{") then ("a" ^ (structOrder rest))
        else if (s = #"}") then ("b" ^ (structOrder rest))
        else if (s = #".") then ("c" ^ (structOrder rest))
        else if (s = #"(") then ("d" ^ (structOrder rest))
        else if (s = #")") then ("e" ^ (structOrder rest))
        else if (s = #" ") then ("f" ^ (structOrder rest))
        else (structOrder rest)
      end

  fun pairUp (vs1, vs2) = (case vs1
    of [] => []
    | (v1 :: vs1) => (case vs2
      of [] => []
      | (v2 :: vs2) => (v1, v2) :: pairUp(vs1, vs2)))

  fun removeDuplicates [] = []
    | removeDuplicates ((x1, x2)::xs) = (x1, x2) :: removeDuplicates(List.filter (fn y => (#1 y) <> x1 andalso (#2 y) <> x2) xs)

  fun search1 (v : string, ls) =
    (case ls of [] => false
      | ((l1, l2) :: ls) => if v = l1 then true else search1(v, ls))

  fun search2 (v : string, ls) =
    (case ls of [] => false
      | ((l1, l2) :: ls) => if v = l2 then true else search2(v, ls))

  fun bijection ls = (case ls
    of [] => true
    | ((v1, v2) :: ls) => (not (search1 (v1, ls))) andalso (not (search2 (v2, ls))) andalso (bijection ls))

  fun equiv (t1, t2) =
    let
      val vs1 = varOrder t1
      val vs2 = varOrder t2
      val paired = pairUp (vs1, vs2)
      val paired' = removeDuplicates paired
      val bij = bijection paired'
    in
      (structOrder (ULC.tos t1)) = (structOrder (ULC.tos t2)) andalso bij
    end


(* Some tests would be nice here... *)

  val _ = Check.expect(varOrder(ULC.Var "hi"), ["hi"], "Test1")
  val _ = Check.expect(varOrder(ULC.Abs ("hi2", ULC.Var "hi")), ["hi2", "hi"], "Test2")
  val _ = Check.expect(pairUp(["a", "b", "c"], ["d", "e", "f"]), [("a", "d"), ("b", "e"), ("c", "f")], "Error in pairUp")


end
