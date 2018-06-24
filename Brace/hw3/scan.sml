structure Scan : sig

  datatype token
    = LBrace
    | RBrace
    | T
    | F
    | Zero
    | If
    | Then
    | Else
    | PlusOne
    | MinusOne
    | IsZ
    | DoubleAmpersand
    | DoublePipe
    | Bang
    | DoubleEqual
    | Identifier of string
    | In
    | Of
    | LeftArrow   (* this is "<-" *)
    | RightArrow  (* this is "->" *)
    | Hash
    | Hash1
    | Hash2
    | Some
    | None
    | LParen
    | RParen
    | Case
    | Pipe
    | Star
    | Plus

  val scan : string -> token list
  val tos  : token -> string

end = struct

  datatype token
    = LBrace
    | RBrace
    | T
    | F
    | Zero
    | If
    | Then
    | Else
    | PlusOne
    | MinusOne
    | IsZ
    | DoubleAmpersand
    | DoublePipe
    | Bang
    | DoubleEqual
    | Identifier of string
    | In
    | Of
    | LeftArrow
    | RightArrow
    | Hash
    | Hash1
    | Hash2
    | Some
    | None
    | LParen
    | RParen
    | Case
    | Pipe
    | Star
    | Plus

  fun afterNewline cs =
    let
      fun lp [] = []
        | lp (#"\n"::cs) = cs
        | lp (_::cs) = lp cs
    in
      lp cs
    end

  fun nextIsValid (cs : char list) : bool =
    let
      fun lp [] = false
        | lp (#"i" :: #"f" :: cs) = false
        | lp (#"t" :: #"h" :: #"e" :: #"n" :: cs) = false
        | lp (#"e" :: #"l" :: #"s" :: #"e" :: cs) = false
        | lp (#"i" :: #"s" :: #"z" :: cs) = false
        | lp (#"i" :: #"n" :: cs) = false
        | lp (#"s" :: #"o" :: #"m" :: #"e" :: cs) = false
        | lp (#"n" :: #"o" :: #"n" :: #"e" :: cs) = false
        | lp (#"c" :: #"a" :: #"s" :: #"e" :: cs) = false
        | lp (#"o" :: #"f" :: cs) = false
        | lp (#"_" :: cs) = true
        | lp (c :: cs) =
          let
            val cascii = (ord c)
          in
            if ((cascii > 47 andalso cascii < 58) orelse (cascii > 64 andalso cascii < 91) orelse (cascii > 96 andalso cascii < 123))
            then true else false
          end
    in
      lp cs
    end

  fun scanReserve (cs : char list) :  (token * char list) option =
    let
      fun lp [] = NONE
        | lp (#"i" :: #"f" :: cs) = SOME(If, cs)
        | lp (#"t" :: #"h" :: #"e" :: #"n" :: cs) = SOME(Then, cs)
        | lp (#"e" :: #"l" :: #"s" :: #"e" :: cs) = SOME(Else, cs)
        | lp (#"i" :: #"s" :: #"z" :: cs) = SOME(IsZ, cs)
        | lp (#"i" :: #"n" :: cs) = SOME(In, cs)
        | lp (#"s" :: #"o" :: #"m" :: #"e" :: cs) = SOME(Some, cs)
        | lp (#"n" :: #"o" :: #"n" :: #"e" :: cs) = SOME(None, cs)
        | lp (#"c" :: #"a" :: #"s" :: #"e" :: cs) = SOME(Case, cs)
        | lp (#"o" :: #"f" :: cs) = SOME(Of, cs)
        | lp cs = raise Fail "Not a reserve word scanned, invalid"
    in
      lp cs
    end

  fun matchType (cs : (char list)) : (token * char list) option =
    let
      fun lp [] = NONE
        | lp (#"N" :: #"a" :: #"t" :: cs) = SOME(Identifier "Nat", cs)
        | lp (#"B" :: #"o" :: #"o" :: #"l" :: cs) = SOME(Identifier "Bool", cs)
        |lp (#"U" :: #"n" :: #"i" :: #"t" :: cs) = SOME(Identifier "Unit", cs)
        | lp (#"P" :: #"a" :: #"i" :: #"r" :: cs) = SOME(Identifier "Pair", cs)
        | lp (#"O" :: #"p" :: #"t" :: cs) = SOME(Identifier "Opt", cs)
        | lp cs = raise Fail "Cannot cap var name"
    in
      lp cs
    end

  fun scanLetters (str : string, cs' : (char list)) : (token * char list) option =
    let
      fun lp [] = SOME (Identifier str, cs')
        | lp (c :: cs) =
          if str = "" andalso not(Char.isLower c) then matchType(cs')
          else if str = "" andalso not(nextIsValid cs') then scanReserve(cs')
          else
            if (nextIsValid cs') then scanLetters(str ^ Char.toString(c), cs) else SOME (Identifier str, cs)
    in
      lp cs'
    end

  fun nextToken (cs : char list) : (token * char list) option =
    let
(* This tokenizer handles the symbolic tokens, but none of the alphabetic ones. *)
(* Your scanner needs to determine whether a string of characters is a reserved word or an identifier. *)
(* An identifier is the broad category including variable names and type names. *)
      fun lp [] = NONE
        | lp (#"{" :: cs) = SOME (LBrace, cs)
        | lp (#"}" :: cs) = SOME (RBrace, cs)
        | lp (#"0" :: cs) = SOME (Zero, cs)
        | lp (#"+" :: #"1" :: cs) = SOME (PlusOne, cs)
        | lp (#"-" :: #"1" :: cs) = SOME (MinusOne, cs)
        | lp (#"&" :: #"&" :: cs) = SOME (DoubleAmpersand, cs)
        | lp (#"|" :: #"|" :: cs) = SOME (DoublePipe, cs)
        | lp (#"|" :: cs) = SOME (Pipe, cs)
        | lp (#"!" :: cs) = SOME (Bang, cs)
        | lp (#"=" :: #"=" :: cs) = SOME (DoubleEqual, cs)
        | lp (#"(" :: cs) = SOME (LParen, cs)
        | lp (#")" :: cs) = SOME (RParen, cs)
        | lp (#"<" :: #"-" :: cs) = SOME (LeftArrow, cs)
        | lp (#"-" :: #">" :: cs) = SOME (RightArrow, cs)
        | lp (#"#" :: #"1" :: cs) = SOME (Hash1, cs)
        | lp (#"#" :: #"2" :: cs) = SOME (Hash2, cs)
        | lp (#"#" :: cs) = SOME (Hash, cs)
        | lp (#"*" :: cs) = SOME (Star, cs)
        | lp (#"+" :: cs) = SOME (Plus, cs)
        | lp (#"/" :: #"/" :: cs) = lp (afterNewline cs)
        | lp (#" " :: cs) = lp cs
        | lp (#"\n" :: cs) = lp cs
        | lp (#"\t" :: cs) = lp cs
        | lp (#"T" :: cs) = SOME (T, cs)
        | lp (#"F" :: cs) = SOME (F, cs)
        | lp cs = scanLetters("", cs)
        (*)| lp cs = raise Fail ("scan failure or unimplemented token at " ^ implode cs) (* todo *)*)
    in
      lp cs
    end

  fun scan program =
    let
      fun lp cs =
       (case nextToken cs
          of NONE => []
           | SOME (tok, cs') => tok :: lp cs')
    in
      lp (explode program)
    end

  fun tos LBrace = "{"
    | tos RBrace = "}"
    | tos T = "T"
    | tos F = "F"
    | tos Zero = "0"
    | tos If = "if"
    | tos Then = "then"
    | tos Else = "else"
    | tos PlusOne = "+1"
    | tos MinusOne = "-1"
    | tos IsZ = "isz"
    | tos DoubleAmpersand = "&&"
    | tos DoublePipe = "||"
    | tos Bang = "!"
    | tos DoubleEqual = "=="
    | tos (Identifier s) = "Identifier(" ^ s ^ ")"
    | tos In = "in"
    | tos Of = "of"
    | tos LeftArrow = "<-"
    | tos RightArrow = "->"
    | tos Hash = "#"
    | tos Hash1 = "#1"
    | tos Hash2 = "#2"
    | tos Some = "some"
    | tos None = "none"
    | tos LParen = "("
    | tos RParen = ")"
    | tos Case = "case"
    | tos Pipe = "|"
    | tos Star = "*"
    | tos Plus = "+"

end
