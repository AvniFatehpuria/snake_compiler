(** This file is the main entry point to our compilation toolchain.  It can be
    used as the "main" routine for a command-line tool that will compile;
    assemble; and link the user's code into an executable binary. *)

open Batteries;;
open ClosureConversion;;
open Expressions;;
open Lexing;;
open Printf;;
open SubprocessUtils;;

(* An exception representing a controlled failure of the hatching process. *)
exception BuildFailure of string;;

(* A list of the runtime resources we need to link in to build an executable.
   These are .c files which should be compiled into .o files and then linked at
   the end.  They are compiled in the order listed here. *)
let runtime_code = ["driver.c"; "printer.c"; "error.c"; "gc.c"];;

(* A utility function which, while compiling an AST into assembly, translates
   compiler-specific exceptions into build error messages. *)
let compile_to_assembly_code ast =

  try
    (* Try to compile the code. *)
    let _ = Wellformedness.check_well_formed ast in
    let p = closure_convert_program ast in
    Compiler.compile_to_assembly_code p
  with
  (* If an exception is raised, attempt to convert to a BuildFailure. *)
  | Environment.UnboundVariable(x,env) ->
    raise (BuildFailure(
        sprintf "Unbound variable %s with environment: %s" x
          (Environment.string_of_environment env)
      ))
  | Wellformedness.IllFormed(errors) ->
    raise (BuildFailure(List.fold_left (fun (acc) (error) -> acc^error) "" errors
    ))
;;

(* Compiles, assembles, and links a single source file into a program.  If
   anything goes wrong, raises BuildFailure with a message.  On success, returns
   the name of the compiled executable file. *)
let build (filename : string) : string =
  (* Verify that the filename ends in ".snake" *)
  if not (String.ends_with filename ".snake") then
    raise (BuildFailure "I only know how to hatch .snake files.");
  let basename = Filename.chop_extension @@ Filename.basename filename in
  (* Create the output directory. *)
  let output_directory = "output" in
  begin
    try
      Unix.mkdir output_directory 0o700
    with
    | Unix.Unix_error(Unix.EEXIST, _, _) -> ()
  end;
  (* Read in the contents of the provided file. *)
  let contents = FileUtils.read_file filename in
  (* Parse it into an AST. *)
  let ast =
    try
      ParserTool.parse filename contents
    with
    | ParserTool.ParseFailure msg -> raise (BuildFailure msg)
  in
  (* Compile it into assembly code. *)
  let assembly_code = compile_to_assembly_code ast in
  (* Write those assembly instructions to a file. *)
  let asm_filename = Filename.concat output_directory @@ basename ^ ".s" in
  FileUtils.write_file asm_filename assembly_code;
  (* Determine appropriate binary format for this system. *)
  let format =
    match determine_runtime_sort () with
    | Some Linux -> "elf32"
    | Some MacOSX -> "macho"
    | _ ->
      raise (BuildFailure
               "Could not determine appropriate binary format for this system.")
  in
  (* Bizarre things happen on some systems in some corner cases if we don't
     flush before launching subprocesses (like failed subprocesses getting a
     copy of our buffer. *)
  (* Assemble the generated x86 into a program object. *)
  try
    let program_o_filename =
      Filename.concat output_directory @@ basename ^ ".o"
    in
    ignore @@ run_and_log_command ("nasm_" ^ basename)
      ["nasm"; "-f"; format; "-o"; program_o_filename; asm_filename];
    (* Compile the runtime into object files. *)
    let object_files =
      runtime_code
      |> List.map
        (fun filename ->
           let o_filename_base = Filename.chop_extension filename ^ ".o" in
           let o_filename = Filename.concat "output" o_filename_base in
           ignore @@ run_and_log_command
             ~pwd:(Some "resources")
             ("clang_compile_" ^ Filename.chop_extension filename)
             ["clang"; "-g"; "-m32"; "-c"; "-o";
              Filename.concat ".." o_filename; filename];
           o_filename)
    in
    (* Link the driver and the program object together. *)
    let program_run_filename =
      Filename.concat output_directory @@ basename ^ ".run"
    in
    ignore @@ run_and_log_command ("clang_link_" ^ basename)
      (["clang"; "-g"; "-m32"; "-o"; program_run_filename; program_o_filename] @
       object_files);
    program_run_filename
  with
  | SubprocessUtils.SubprocessFailure msg -> raise (BuildFailure msg)
