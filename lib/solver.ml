(* Cached Solver for 0install *)
module Cached_dir_context = struct
  include Opam_0install.Dir_context

  let cache = Hashtbl.create 100000

  let candidates t name =
    match Hashtbl.find_opt cache name with
    | Some x -> x
    | None ->
        let r = candidates t name in
        Hashtbl.add cache name r;
        r
end

module Solver = Opam_0install.Solver.Make (Cached_dir_context)

(* module Solver = Opam_0install.Solver.Make (Opam_0install.Dir_context) *)

let check_coinstallable ~packages_dir packages =
  let env =
    Opam_0install.Dir_context.std_env ~arch:"x86_64" ~os:"linux"
      ~os_family:"debian" ~os_distribution:"debian" ~os_version:"10" ()
  in
  let constraints =
    List.map
      (fun pkg -> (OpamPackage.name pkg, (`Eq, OpamPackage.version pkg)))
      packages
    |> OpamPackage.Name.Map.of_list
  in
  let context =
    Opam_0install.Dir_context.create ~constraints ~env packages_dir
  in
  let packages = List.map OpamPackage.name packages in
  let result = Solver.solve context packages in
  match result with Error _ -> false | Ok _ -> true
