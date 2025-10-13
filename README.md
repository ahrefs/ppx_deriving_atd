# ppx\_deriving\_atd

## Overview
This is a PPX deriver that creates [ATD](https://github.com/ahrefs/atd) files.  

## Usage
The current setup is not very ergonomic.

In your normal OCaml files, add the deriving annotation:
```ocaml
type my_type = {
  field1: string;
  field2: int;
} [@@deriving atd]
```

This will generate a `export_atd_file` function inside the file.  Currently, you can create another file e.g. [export_atd.ml](./ppx_deriving_atd/test/export_atd.ml) and call that function to generate the file:
```
let () = Readme.export_atd_file "readme.atd.out"
```

Notice the `readme.atd.out` extension.  This is because to get the actual file, we need to add some dune rules to call this new executable and then promote this `readme.atd.out` from `_build` directory into `readme.atd` in our current directory.
```
(executable
 (name export_atd)
 (libraries ppx_deriving_atd)
 (preprocess (pps ppx_deriving_atd)))

(rule
 (alias gen)
 (targets readme.atd.out)
 (action
  (run ./export_atd.exe)))

(rule
 (alias gen)
 (action (diff readme.atd readme.atd.out)))
 ```

Now to generate the file, you can run `dune build @gen --auto-promote` and the `readme.atd` file will be generated:

```atd
type my_type2  = {
	?field1 : string   option ;
	field2 : int   option ;
} 
```

### Supported Annoations
#### default
Annotating a record field with `[@default v]` will allow the generated reader to give default value for the field (and so the field no longer have an optional type).  Note `v` must be in a stringify form: `"999"` instead of `999`.

```ocaml
type my_type_default = {
  field1: string;
  field2: int [@default "999"];
} [@@deriving atd]
```

```atd
type my_type_default  = {
	field1 : string  ;
	~field2 <ocaml default="999">: int  ;
} 
```

#### required
All option typed fields are "optional" by default, i.e. if the value is `None`, it will not be included when serialization and when deserializing, ATD will assume the value is `None` if the field is not present.  

Annotating a record field with `[@required]` will always keep the optional value in the serialized record and ATD will keep looking for it when deserializing the records.

```ocaml
type my_type_required = {
  field1: string option;
  field2: int option [@required];
} [@@deriving atd]
```

```atd
type my_type_required  = {
	?field1 : string   option ;
	field2 : int   option ;
} 
```
