let scale_file fname =
  let ic = open_in fname in
    Printf.printf "real start time of first dialog (in format HH:MM:SS,MSS): %!";
    let start = Scanf.scanf "%12s\n" Time.of_string in
      Printf.printf "real start time of last dialog (in format HH:MM:SS,MSS): %!";
      let finish = Scanf.scanf "%12s\n" Time.of_string in
        Printf.printf "output file name [scaled-%s]: %!" fname;
        let outfname = ref ("scaled-" ^ fname) in
          Scanf.scanf "%s\n" (fun s -> if s <> "" then outfname := s);
          let oc = open_out !outfname in
            Srt_format.scale_channel ic oc start finish;
            close_out oc;
            close_in ic

let _ =
  Arg.parse [] scale_file "Use this program to re-scale .rst files"
