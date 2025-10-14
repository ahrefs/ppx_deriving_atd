type single_record = { a : int; x : string } [@@deriving atd]

let () = export_atd_file "single_record.atd.out"
