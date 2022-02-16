let file_to_string filename =
    let input_file = open_in filename in
    let rec aux () =
        try
            let line = input_line input_file in
            line ^ (aux ())
        with
        | End_of_file ->
            close_in input_file;
            "" in
    aux ()


let write_to_file data filename =
    let output_file = open_out filename in
    Printf.fprintf output_file "%s\n" data;
    close_out output_file
