let create_temp_dir prefix =
  let base_temp_dir = Filename.get_temp_dir_name () in
  let unique_temp_dir =
    Filename.concat base_temp_dir
      (prefix ^ (Unix.time () |> int_of_float |> string_of_int))
  in
  Unix.mkdir unique_temp_dir 0o700;
  unique_temp_dir

let remove_dir dir =
  let rec remove_dir_rec dir =
    let entries = Sys.readdir dir in
    Array.iter
      (fun entry ->
        let entry = Filename.concat dir entry in
        if Sys.is_directory entry then remove_dir_rec entry
        else Unix.unlink entry)
      entries;
    Unix.rmdir dir
  in
  remove_dir_rec dir
