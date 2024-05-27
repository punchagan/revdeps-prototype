open OpamTypes

(* Cached Solver for 0install *)
let ( / ) = Filename.concat

let check_coinstallable ~universe ~packages_dir packages =
  let module Cached_dir_context = struct
    include Opam_0install.Dir_context

    let candidates _t name =
      let packages =
        OpamPackage.Set.filter
          (fun p -> OpamPackage.Name.equal (OpamPackage.name p) name)
          universe.u_available
      in
      OpamPackage.Set.elements packages
      |> List.map (fun p ->
             let opam = OpamFile.OPAM.create p in
             let depends = OpamPackage.Map.find p universe.u_depends in
             let opam = OpamFile.OPAM.with_depends depends opam in
             (OpamPackage.version p, Ok opam))
  end in
  let module Solver = Opam_0install.Solver.Make (Cached_dir_context) in
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
  (* HACK: Checking that we are really not doing any disk access *)
  let packages_dir = packages_dir / "blah" in
  let context =
    Opam_0install.Dir_context.create ~constraints ~env packages_dir
  in
  let packages = List.map OpamPackage.name packages in
  let result = Solver.solve context packages in
  match result with Error _ -> false | Ok _ -> true
