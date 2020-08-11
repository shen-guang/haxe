open Path

class virtual vfs_base (name : string) (parent : vfs_directory option) =
	let full_path = match parent with
		| None ->
			name
		| Some vp ->
			vp#full_path ^ name
	in
object(self)

	method full_path = full_path
	method name = name

	method virtual print : string -> bool -> string

	method append (part : string) =
		self#full_path ^ part
end

and virtual vfs_directory (name : string) (parent : vfs_directory option) =
	let name =
		if name = "" then name else add_trailing_slash name
	in
object(self)
	inherit vfs_base name parent

	val mutable valid = false
	val mutable directories = DynArray.create ()
	val mutable files = DynArray.create ()
	val directoryLut = Hashtbl.create 0
	val fileLut = Hashtbl.create 0

	method add_directory (vd : vfs_directory) =
		DynArray.add directories vd;
		self#invalidate

	method add_file (vf : vfs_file) =
		DynArray.add files vf;
		self#invalidate

	method resolve_directory (namme : string) : vfs_directory =
		self#cache;
		Hashtbl.find directoryLut name

	method resolve_file (name : string) : vfs_file =
		self#cache;
		Hashtbl.find fileLut name

	method virtual get_children : unit

	method clear_cache =
		Hashtbl.clear directoryLut;
		Hashtbl.clear fileLut;

	method cache =
		if not valid then begin
			self#clear_cache;
			self#get_children;
			valid <- true;
			DynArray.iter (fun vd ->
				Hashtbl.add directoryLut vd#name vd
			) directories;
			DynArray.iter (fun vf ->
				Hashtbl.add fileLut vf#name vf
			) files
		end

	method invalidate =
		valid <- false

	method print (indent : string) (recursive : bool) =
		self#cache;
		let buf = Buffer.create 0 in
		let add s =
			Buffer.add_string buf s
		in
		add indent;
		add name;
		if recursive then begin
			DynArray.iter (fun vd ->
				add "\n  ";
				add indent;
				add (vd#print (indent ^ "  ") recursive)
			) directories;
			DynArray.iter (fun vf ->
				add "\n  ";
				add indent;
				add (vf#print (indent ^ "  ") recursive)
			) files
		end;
		Bytes.unsafe_to_string (Buffer.to_bytes buf)
end

and virtual vfs_file (name : string) (parent : vfs_directory) =

object(self)
	inherit vfs_base name (Some parent)

	method separator = "@"

	method virtual content : Bytes.t

	method print (indent : string) (recursive : bool) =
		indent ^ name
end


class sys_directory (name : string) (parent : vfs_directory option) =

object(self)
	inherit vfs_directory name parent

	method get_children =
		directories <- DynArray.create ();
		files <- DynArray.create ();
		let sl = Sys.readdir self#full_path in
		Array.iter (fun s ->
			let full = self#append s in
			if Sys.file_exists full then begin
				if Sys.is_directory full then begin
					DynArray.add directories (new sys_directory s (Some (self :> sys_directory)))
				end else begin
					DynArray.add files (new sys_file s (self :> sys_directory))
				end
			end
		) sl
end

and sys_file (name : string) (parent : vfs_directory) =

object(self)
	inherit vfs_file name parent

	method content =
		let ch = open_in_bin self#full_path in
		let b = IO.read_all (IO.input_channel ch) in
		close_in ch;
		Bytes.unsafe_of_string b
end