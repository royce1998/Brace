structure Store : sig

(* This is like CS154, but 1000x easier. *)

  exception OutOfMemory
  exception UnallocatedMemory
  exception SegmentationFault

  type loc = int

(* Allocate a place to put the data, put it there, and return the location. *)
(* Raise an OutOfMemory exception if no more memory is available. *)
  val malloc : AST.term -> loc

(* Read the data at the location. *)
(* Raise an UnallocatedMemory exception if the location contains unitialized data. *)
(* Raise a SegmentationFault if the location is outside the heap. *)
  val read   : loc -> AST.term

(* Write the data to the location. *)
(* Raise an UnallocatedMemory exception if the location contains unitialized data. *)
(* Raise a SegmentationFault if the location is outside the heap. *)
  val write  : loc * AST.term -> unit

(* Erase all heap data and make all memory available again. *)
  val clear  : unit -> unit

end = struct

  exception OutOfMemory
  exception UnallocatedMemory
  exception SegmentationFault

	val heap = Array.array (1001, NONE) : AST.term option array
	(* Note that the last position of the array is for temp out of bounds terms *)

	fun findEmpty (h : AST.term option array, c) =
		if (c > 999) then 1000
		else if (case (Array.sub (h, c)) of NONE => true | _ => false) then c
		else findEmpty (h, c + 1)

  type loc = int

  fun malloc t =
		let
			val l = findEmpty (heap, 0)
			val _ = (Array.update (heap, l, SOME t))
		in
			if l = 1000 then raise OutOfMemory else l
		end

  fun read loc = if (loc > 999 orelse loc < 0) then raise SegmentationFault
		else (case (Array.sub (heap, loc)) of SOME t => t | _ => raise UnallocatedMemory)

  fun write (loc, t) = if (loc > 999 orelse loc < 0) then raise SegmentationFault
		else (case (Array.sub (heap, loc)) of NONE => raise UnallocatedMemory | _ => (Array.update (heap, loc, SOME t)))

  fun clear () =
		let
			val h = Array.array (1001, NONE)
		in
			if (heap = h) then () else () (* I use SML's built in garbage collector! *)
		end

end
