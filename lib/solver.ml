module Solver = Opam_0install.Solver.Make (Opam_0install.Dir_context)

let check_coinstallable ~local_repo_dir packages =
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
    Opam_0install.Dir_context.create local_repo_dir ~constraints ~env
  in
  let packages = List.map OpamPackage.name packages in
  let result = Solver.solve context packages in
  match result with Error _ -> false | Ok _ -> true
