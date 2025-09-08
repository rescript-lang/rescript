# Phony targets


# collect file groups

1. we need check integrity of files here?
cases:
one directory have two same files -- ignore, does not matter here?
two directories ha

# generate ninja from file groups

`Bsb_file_groups.file_group list`
one directory, one kind --
when we merge we will have two `dirs`

do we allow duplicate modules?

suppose :
lib -> 0

dev -> 1
dev -> 2 
dev -> 3 

so that they can have same names


.default
All  output (not just js in case we support native build)

.install

It's hard to bake it in built rules, since it is flag dependent, if you have `-bin-annot` 
then you would like to install `cmt` too, however, it might be or not be there 

# meta-data files 

- .bsbuild
- .bsdeps
- .sourcedirs
# post-build

Here we have `js` generated, we can do either post-build or
create a new rule.

Note creating new rules will get more concurrency while post-build is easy 
and can do in source modification

https://groups.google.com/forum/#!searchin/ninja-build/post$20process%7Csort:relevance/ninja-build/Q4hpcDmhPzw/KZpDyOEFuTkJ 

# Performance tweaks

Writing(truncating) files are significantly slower (20~30x) and it destroys cache(see Appendix), we should 
try to avoid writing too many files.

bsb is optimized for incremental build (especially for modifying files ).

There is a trade off here: if we generate `.bsdep` file, whenever adding or removing file, `.bscache` will not 
impact the integrity of `.bsdep`, so that it will run `.bsdep -> .d`.

The downside is 
1. clean build will generate more smaller files (slow down), 
2. build system has to track more outputs  (latency, stat more files almost doubled)

Whenever we change a file it will regenerate the ast, optionally update `.bsdep`      

So assuming that merge `.bsdep` into `.mlast`, build system will track not more files.
The integrity of `.mlast` is not impacted by `.bscache`.
`.mlast -> .d` can be still improved, not as good as `.bsdep -> .d` since `.bsdep` 
can check `.bsdep` time stamp.

So let's change the `.mlast` to such format

----------
magic number
length of dependent modules
dependent modules
binary ast   
----------

This file is integrity is not impacted by `.bscache`. whenever `.bscache` changes we check if we need regenerate `.d`

# Appendix
[source,ocaml]
--------------
module Set_string = Set.Make(String)

(* let v = Set_string.of_list ["List" ; "Set" ; "String" ; "Test_order"] *)
let v = Set_string.of_list []
let deseralize f =
  let ichan  = open_in_bin f in
  let v : Set_string.t = input_value ichan  in
  close_in ichan ;
  v

let time f arg =
  let v0 = Unix.gettimeofday () in
  ignore @@ f arg;
  let v1 = Unix.gettimeofday ()  in
  print_endline (Printf.sprintf "%f elapsed" (v1 -. v0))

let deseralize_and_compare f =
  ignore @@ Set_string.equal v (deseralize f)

let  seralize f =
  let ochan = open_out_bin f in
  output_value ochan  v ;
  close_out ochan

let try_seralize f =
  match open_in_bin f with
  | ichan ->
    close_in ichan ;
    let ochan = open_out_bin f in
    output_value ochan  v ;
    close_out ochan
  | exception _ ->
    let ochan = open_out_bin f in
    output_value ochan  v ;
    close_out ochan

let try_seralize2 f =
  if Sys.file_exists f then
    let ochan = open_out_bin f in
    output_value ochan  v ;
    close_out ochan
  else
    let ochan = open_out_bin f in
    output_value ochan  v ;
    close_out ochan



let () =
  let file = "/Users/hzhang295/git/tmp/bench/e.mldeps" in
  time try_seralize file;
  Unix.unlink file ;
  time try_seralize2 file;
  Unix.unlink file ;
  time seralize file;
  time deseralize_and_compare file;
  Unix.unlink file

(*                                                                                                                                                                                                                                                              
0.002452 elapsed                                                                                                                                                                                                                                                
0.002440 elapsed                                                                                                                                                                                                                                                
0.001954 elapsed                                                                                                                                                                                                                                                
0.000079 elapsed                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                
*)

--------------

# package-flags

 when designing bsc command line flags, we ask user to specify the output path of package output 
 instead of calculating, 
 the reason is that the user input can be absolute path or relative path, to calculate 
 we also need the location of package.json.


 ## document when regenerating `build.ninja`
 
 - when `bsb.exe` path is changed
 - when `bsb.exe` version is changed 

 ## other internal options
 
-no-dev -- don't build dev directory group
-install -- install files

## document when regenerating `.merlin`

## TODO: seems we can do it 

1. instead of specifying the whole relative path, just specifying the offset 
 ```
-bs-package-output commonjs:+lib/js -bs-package-output amdjs:+lib/amdjs xx.mlast
 ```   

 With this we would simplify the build a lot.

 on Windows
 ```
 -bs-package-output commonjs:+lib\js -bs-package-output:+lib\amdjs xx.ml a/b/c/xx.mlast
 ``` 

 so when the user input is relative path, we do the concat,
 if it is absolute path, we calculate the relative path first.

 This is complicated vs  
 
 ```
 -bs-package-output commonjs -bs-package-output amdjs
 ```

 however, the bsc is almost sitting in `lib/bs`

2. caching Directory operations
3. Read `bsconfig.json` from watchcer  side, so that we can caching io operations more effeciently?
