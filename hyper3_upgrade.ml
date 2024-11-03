(***


          __   __                                   ___
         / /  / /                                  ___/
        / /__/ /__    __  _______  ______  --___  ___/
       / ___  / \ \  / / / ___  / / ____/ / ___/
      / /  / /   \ \/ / / /__/ / / /___/ / /
     / /  /_/     \  / / _____/ /_____/ /_/
                  / / / /
                 /_/ /_/


***)


let timeit f x n =
  let t = Sys.time() in
  for i = 1 to n-1 do
    let _ = f x in ()
  done;
  let fx = f x in
  Printf.printf "Execution time : %fs\n" (Sys.time()-. t) ;
  fx
;;

let timeit_2var f x y n =
  let t = Sys.time() in
  for i = 1 to n-1 do
    let _ = f x y in ()
  done;
  let fx = f x y in
  Printf.printf "Execution time : %fs\n" (Sys.time()-. t) ;
  fx
;;

let format_time f =
  let h, r = floor (f /. 3600.), Float.rem f (3600.) in
  let m, s = floor (r /. 60.), Float.rem r (60.) in
  if h>0. then Printf.sprintf "%d heure(s), %d minute(s) et %.3f secondes" 
    (int_of_float h) (int_of_float m) s
  else if m>0. then Printf.sprintf "%d minute(s) et %.3f secondes" (int_of_float m) s
  else Printf.sprintf "%.3f secondes" s;;


(*############################################################################################################*)
(*############################################################################################################*)
(*######################################  STRUCTURE DE DONNEES  ##############################################*)
(*############################################################################################################*)
(*############################################################################################################*)

type cube =  {cornerperm : int array;
edgeperm : int array;
cornerrot : int array;
edgerot : int array};;

type move = cube -> cube;;

let resolu = {cornerperm=[|0;1;2;3;4;5;6;7|];
edgeperm=[|0;1;2;3;4;5;6;7;8;9;10;11|];
cornerrot=[|0;0;0;0;0;0;0;0|];
edgerot=[|0;0;0;0;0;0;0;0;0;0;0;0|]} ;;

let copy cube =
  {
    cornerperm = Array.copy cube.cornerperm;
    edgeperm = Array.copy cube.edgeperm;
    cornerrot = Array.copy cube.cornerrot;
    edgerot = Array.copy cube.edgerot
  }
;;

let identical_array a1 a2 =
  let n = Array.length a1 in
  let test = ref (n=Array.length a2) and i = ref 0 in
  while !i<n && !test do
    test:= !test && (a1.(!i)=a2.(!i));
    i:= !i + 1
  done;
  !test
;;

let are_identical c1 c2 =
  identical_array c1.cornerperm c2.cornerperm &&
  identical_array c1.cornerrot c2.cornerrot &&
  identical_array c1.edgeperm c2.edgeperm &&
  identical_array c1.edgerot c2.edgerot ;;

let id (cube:cube) = ();;

(*Face U*)

let u cube =
  let cp0 = cube.cornerperm.(0) in
  cube.cornerperm.(0) <- cube.cornerperm.(1);
  cube.cornerperm.(1) <- cube.cornerperm.(2);
  cube.cornerperm.(2) <- cube.cornerperm.(3);
  cube.cornerperm.(3) <- cp0;
  
  let ep0 = cube.edgeperm.(0) in
  cube.edgeperm.(0) <- cube.edgeperm.(1);
  cube.edgeperm.(1) <- cube.edgeperm.(2);
  cube.edgeperm.(2) <- cube.edgeperm.(3);
  cube.edgeperm.(3) <- ep0;
  
  let cr0 = cube.cornerrot.(0) in
  cube.cornerrot.(0) <- cube.cornerrot.(1);
  cube.cornerrot.(1) <- cube.cornerrot.(2);
  cube.cornerrot.(2) <- cube.cornerrot.(3);
  cube.cornerrot.(3) <- cr0;
  
  let er0 = cube.edgerot.(0) in
  cube.edgerot.(0) <- cube.edgerot.(1);
  cube.edgerot.(1) <- cube.edgerot.(2);
  cube.edgerot.(2) <- cube.edgerot.(3);
  cube.edgerot.(3) <- er0
;;

let u' cube =
  let cp0 = cube.cornerperm.(0) in
  cube.cornerperm.(0) <- cube.cornerperm.(3);
  cube.cornerperm.(3) <- cube.cornerperm.(2);
  cube.cornerperm.(2) <- cube.cornerperm.(1);
  cube.cornerperm.(1) <- cp0;
  
  let ep0 = cube.edgeperm.(0) in
  cube.edgeperm.(0) <- cube.edgeperm.(3);
  cube.edgeperm.(3) <- cube.edgeperm.(2);
  cube.edgeperm.(2) <- cube.edgeperm.(1);
  cube.edgeperm.(1) <- ep0;
  
  let cr0 = cube.cornerrot.(0) in
  cube.cornerrot.(0) <- cube.cornerrot.(3);
  cube.cornerrot.(3) <- cube.cornerrot.(2);
  cube.cornerrot.(2) <- cube.cornerrot.(1);
  cube.cornerrot.(1) <- cr0;
  
  let er0 = cube.edgerot.(0) in
  cube.edgerot.(0) <- cube.edgerot.(3);
  cube.edgerot.(3) <- cube.edgerot.(2);
  cube.edgerot.(2) <- cube.edgerot.(1);
  cube.edgerot.(1) <- er0
;;

let u2 cube =
  let cp0, cp2 = cube.cornerperm.(0), cube.cornerperm.(1) in
  cube.cornerperm.(0) <- cube.cornerperm.(2);
  cube.cornerperm.(2) <- cp0;
  cube.cornerperm.(1) <- cube.cornerperm.(3);
  cube.cornerperm.(3) <- cp2;
  
  let ep0, ep2 = cube.edgeperm.(0), cube.edgeperm.(1) in
  cube.edgeperm.(0) <- cube.edgeperm.(2);
  cube.edgeperm.(2) <- ep0;
  cube.edgeperm.(1) <- cube.edgeperm.(3);
  cube.edgeperm.(3) <- ep2;
  
  let cr0, cr2 = cube.cornerrot.(0), cube.cornerrot.(1) in
  cube.cornerrot.(0) <- cube.cornerrot.(2);
  cube.cornerrot.(2) <- cr0;
  cube.cornerrot.(1) <- cube.cornerrot.(3);
  cube.cornerrot.(3) <- cr2;
  
  let er0, er2 = cube.edgerot.(0), cube.edgerot.(1) in
  cube.edgerot.(0) <- cube.edgerot.(2);
  cube.edgerot.(2) <- er0;
  cube.edgerot.(1) <- cube.edgerot.(3);
  cube.edgerot.(3) <- er2
;;

(*Face D*)

let d cube =
  let cp0 = cube.cornerperm.(4) in
  cube.cornerperm.(4) <- cube.cornerperm.(7);
  cube.cornerperm.(7) <- cube.cornerperm.(6);
  cube.cornerperm.(6) <- cube.cornerperm.(5);
  cube.cornerperm.(5) <- cp0;
  
  let ep0 = cube.edgeperm.(8) in
  cube.edgeperm.(8) <- cube.edgeperm.(11);
  cube.edgeperm.(11) <- cube.edgeperm.(10);
  cube.edgeperm.(10) <- cube.edgeperm.(9);
  cube.edgeperm.(9) <- ep0;
  
  let cr0 = cube.cornerrot.(4) in
  cube.cornerrot.(4) <- cube.cornerrot.(7);
  cube.cornerrot.(7) <- cube.cornerrot.(6);
  cube.cornerrot.(6) <- cube.cornerrot.(5);
  cube.cornerrot.(5) <- cr0;
  
  let er0 = cube.edgerot.(8) in
  cube.edgerot.(8) <- cube.edgerot.(11);
  cube.edgerot.(11) <- cube.edgerot.(10);
  cube.edgerot.(10) <- cube.edgerot.(9);
  cube.edgerot.(9) <- er0
;;

let d' cube =
  let cp0 = cube.cornerperm.(4) in
  cube.cornerperm.(4) <- cube.cornerperm.(5);
  cube.cornerperm.(5) <- cube.cornerperm.(6);
  cube.cornerperm.(6) <- cube.cornerperm.(7);
  cube.cornerperm.(7) <- cp0;
  
  let ep0 = cube.edgeperm.(8) in
  cube.edgeperm.(8) <- cube.edgeperm.(9);
  cube.edgeperm.(9) <- cube.edgeperm.(10);
  cube.edgeperm.(10) <- cube.edgeperm.(11);
  cube.edgeperm.(11) <- ep0;
  
  let cr0 = cube.cornerrot.(4) in
  cube.cornerrot.(4) <- cube.cornerrot.(5);
  cube.cornerrot.(5) <- cube.cornerrot.(6);
  cube.cornerrot.(6) <- cube.cornerrot.(7);
  cube.cornerrot.(7) <- cr0;
  
  let er0 = cube.edgerot.(8) in
  cube.edgerot.(8) <- cube.edgerot.(9);
  cube.edgerot.(9) <- cube.edgerot.(10);
  cube.edgerot.(10) <- cube.edgerot.(11);
  cube.edgerot.(11) <- er0
;;

let d2 cube =
  let cp0, cp2 = cube.cornerperm.(4), cube.cornerperm.(7) in
  cube.cornerperm.(4) <- cube.cornerperm.(6);
  cube.cornerperm.(6) <- cp0;
  cube.cornerperm.(7) <- cube.cornerperm.(5);
  cube.cornerperm.(5) <- cp2;
  
  let ep0, ep2 = cube.edgeperm.(8), cube.edgeperm.(11) in
  cube.edgeperm.(8) <- cube.edgeperm.(10);
  cube.edgeperm.(10) <- ep0;
  cube.edgeperm.(11) <- cube.edgeperm.(9);
  cube.edgeperm.(9) <- ep2;
  
  let cr0, cr2 = cube.cornerrot.(4), cube.cornerrot.(7) in
  cube.cornerrot.(4) <- cube.cornerrot.(6);
  cube.cornerrot.(6) <- cr0;
  cube.cornerrot.(7) <- cube.cornerrot.(5);
  cube.cornerrot.(5) <- cr2;
  
  let er0, er2 = cube.edgerot.(8), cube.edgerot.(11) in
  cube.edgerot.(8) <- cube.edgerot.(10);
  cube.edgerot.(10) <- er0;
  cube.edgerot.(11) <- cube.edgerot.(9);
  cube.edgerot.(9) <- er2
;;

(*Face F*)

let f cube =
  let cp0 = cube.cornerperm.(1) in
  cube.cornerperm.(1) <- cube.cornerperm.(5);
  cube.cornerperm.(5) <- cube.cornerperm.(6);
  cube.cornerperm.(6) <- cube.cornerperm.(2);
  cube.cornerperm.(2) <- cp0;
  
  let ep0 = cube.edgeperm.(1) in
  cube.edgeperm.(1) <- cube.edgeperm.(5);
  cube.edgeperm.(5) <- cube.edgeperm.(9);
  cube.edgeperm.(9) <- cube.edgeperm.(6);
  cube.edgeperm.(6) <- ep0;
  
  let cr0 = cube.cornerrot.(1) in
  cube.cornerrot.(1) <- (cube.cornerrot.(5) + 2) mod 3 ;
  cube.cornerrot.(5) <- (cube.cornerrot.(6) + 1) mod 3;
  cube.cornerrot.(6) <- (cube.cornerrot.(2) + 2) mod 3;
  cube.cornerrot.(2) <- (cr0 + 1) mod 3;
  
  let er0 = cube.edgerot.(1) in
  cube.edgerot.(1) <- cube.edgerot.(5);
  cube.edgerot.(5) <- cube.edgerot.(9);
  cube.edgerot.(9) <- cube.edgerot.(6);
  cube.edgerot.(6) <- er0
;;

let f' cube =
  let cp0 = cube.cornerperm.(1) in
  cube.cornerperm.(1) <- cube.cornerperm.(2);
  cube.cornerperm.(2) <- cube.cornerperm.(6);
  cube.cornerperm.(6) <- cube.cornerperm.(5);
  cube.cornerperm.(5) <- cp0;
  
  let ep0 = cube.edgeperm.(1) in
  cube.edgeperm.(1) <- cube.edgeperm.(6);
  cube.edgeperm.(6) <- cube.edgeperm.(9);
  cube.edgeperm.(9) <- cube.edgeperm.(5);
  cube.edgeperm.(5) <- ep0;
  
  let cr0 = cube.cornerrot.(1) in
  cube.cornerrot.(1) <- (cube.cornerrot.(2) + 2) mod 3;
  cube.cornerrot.(2) <- (cube.cornerrot.(6) + 1) mod 3;
  cube.cornerrot.(6) <- (cube.cornerrot.(5) + 2) mod 3;
  cube.cornerrot.(5) <- (cr0 + 1) mod 3;
  
  let er0 = cube.edgerot.(1) in
  cube.edgerot.(1) <- cube.edgerot.(6);
  cube.edgerot.(6) <- cube.edgerot.(9);
  cube.edgerot.(9) <- cube.edgerot.(5);
  cube.edgerot.(5) <- er0
;;

let f2 cube =
  let cp0, cp2 = cube.cornerperm.(1), cube.cornerperm.(2) in
  cube.cornerperm.(1) <- cube.cornerperm.(6);
  cube.cornerperm.(6) <- cp0;
  cube.cornerperm.(2) <- cube.cornerperm.(5);
  cube.cornerperm.(5) <- cp2;
  
  let ep0, ep2 = cube.edgeperm.(1), cube.edgeperm.(6) in
  cube.edgeperm.(1) <- cube.edgeperm.(9);
  cube.edgeperm.(9) <- ep0;
  cube.edgeperm.(6) <- cube.edgeperm.(5);
  cube.edgeperm.(5) <- ep2;
  
  let cr0, cr2 = cube.cornerrot.(1), cube.cornerrot.(2) in
  cube.cornerrot.(1) <- cube.cornerrot.(6);
  cube.cornerrot.(6) <- cr0;
  cube.cornerrot.(2) <- cube.cornerrot.(5);
  cube.cornerrot.(5) <- cr2;
  
  let er0, er2 = cube.edgerot.(1), cube.edgerot.(6) in
  cube.edgerot.(1) <- cube.edgerot.(9);
  cube.edgerot.(9) <- er0;
  cube.edgerot.(6) <- cube.edgerot.(5);
  cube.edgerot.(5) <- er2
;;

(*Face B*)

let b cube =
  let cp0 = cube.cornerperm.(0) in
  cube.cornerperm.(0) <- cube.cornerperm.(3);
  cube.cornerperm.(3) <- cube.cornerperm.(7);
  cube.cornerperm.(7) <- cube.cornerperm.(4);
  cube.cornerperm.(4) <- cp0;
  
  let ep0 = cube.edgeperm.(3) in
  cube.edgeperm.(3) <- cube.edgeperm.(7);
  cube.edgeperm.(7) <- cube.edgeperm.(11);
  cube.edgeperm.(11) <- cube.edgeperm.(4);
  cube.edgeperm.(4) <- ep0;
  
  let cr0 = cube.cornerrot.(0) in
  cube.cornerrot.(0) <- (cube.cornerrot.(3) + 1) mod 3;
  cube.cornerrot.(3) <- (cube.cornerrot.(7) + 2) mod 3;
  cube.cornerrot.(7) <- (cube.cornerrot.(4) + 1) mod 3;
  cube.cornerrot.(4) <- (cr0 + 2) mod 3;
  
  let er0 = cube.edgerot.(3) in
  cube.edgerot.(3) <- cube.edgerot.(7);
  cube.edgerot.(7) <- cube.edgerot.(11);
  cube.edgerot.(11) <- cube.edgerot.(4);
  cube.edgerot.(4) <- er0
;;

let b' cube =
  let cp0 = cube.cornerperm.(0) in
  cube.cornerperm.(0) <- cube.cornerperm.(4);
  cube.cornerperm.(4) <- cube.cornerperm.(7);
  cube.cornerperm.(7) <- cube.cornerperm.(3);
  cube.cornerperm.(3) <- cp0;
  
  let ep0 = cube.edgeperm.(3) in
  cube.edgeperm.(3) <- cube.edgeperm.(4);
  cube.edgeperm.(4) <- cube.edgeperm.(11);
  cube.edgeperm.(11) <- cube.edgeperm.(7);
  cube.edgeperm.(7) <- ep0;
  
  let cr0 = cube.cornerrot.(0) in
  cube.cornerrot.(0) <- (cube.cornerrot.(4) + 1) mod 3;
  cube.cornerrot.(4) <- (cube.cornerrot.(7) + 2) mod 3;
  cube.cornerrot.(7) <- (cube.cornerrot.(3) + 1) mod 3;
  cube.cornerrot.(3) <- (cr0 + 2) mod 3;
  
  let er0 = cube.edgerot.(3) in
  cube.edgerot.(3) <- cube.edgerot.(4);
  cube.edgerot.(4) <- cube.edgerot.(11);
  cube.edgerot.(11) <- cube.edgerot.(7);
  cube.edgerot.(7) <- er0
;;

let b2 cube =
  let cp0, cp2 = cube.cornerperm.(0), cube.cornerperm.(3) in
  cube.cornerperm.(0) <- cube.cornerperm.(7);
  cube.cornerperm.(7) <- cp0;
  cube.cornerperm.(3) <- cube.cornerperm.(4);
  cube.cornerperm.(4) <- cp2;
  
  let ep0, ep2 = cube.edgeperm.(4), cube.edgeperm.(3) in
  cube.edgeperm.(4) <- cube.edgeperm.(7);
  cube.edgeperm.(7) <- ep0;
  cube.edgeperm.(3) <- cube.edgeperm.(11);
  cube.edgeperm.(11) <- ep2;
  
  let cr0, cr2 = cube.cornerrot.(0), cube.cornerrot.(3) in
  cube.cornerrot.(0) <- cube.cornerrot.(7);
  cube.cornerrot.(7) <- cr0;
  cube.cornerrot.(3) <- cube.cornerrot.(4);
  cube.cornerrot.(4) <- cr2;
  
  let er0, er2 = cube.edgerot.(4), cube.edgerot.(3) in
  cube.edgerot.(4) <- cube.edgerot.(7);
  cube.edgerot.(7) <- er0;
  cube.edgerot.(3) <- cube.edgerot.(11);
  cube.edgerot.(11) <- er2
;;

(*Face L*)

let l cube =
  let cp0 = cube.cornerperm.(0) in
  cube.cornerperm.(0) <- cube.cornerperm.(4);
  cube.cornerperm.(4) <- cube.cornerperm.(5);
  cube.cornerperm.(5) <- cube.cornerperm.(1);
  cube.cornerperm.(1) <- cp0;
  
  let ep0 = cube.edgeperm.(0) in
  cube.edgeperm.(0) <- cube.edgeperm.(4);
  cube.edgeperm.(4) <- cube.edgeperm.(8);
  cube.edgeperm.(8) <- cube.edgeperm.(5);
  cube.edgeperm.(5) <- ep0;
  
  let cr0 = cube.cornerrot.(0) in
  cube.cornerrot.(0) <- (cube.cornerrot.(4) + 2) mod 3;
  cube.cornerrot.(4) <- (cube.cornerrot.(5) + 1) mod 3;
  cube.cornerrot.(5) <- (cube.cornerrot.(1) + 2) mod 3;
  cube.cornerrot.(1) <- (cr0 + 1) mod 3;
  
  let er0 = cube.edgerot.(0) in
  cube.edgerot.(0) <- (1 - cube.edgerot.(4));
  cube.edgerot.(4) <- (1 - cube.edgerot.(8));
  cube.edgerot.(8) <- (1 - cube.edgerot.(5));
  cube.edgerot.(5) <- (1 - er0)
;;

let l' cube =
  let cp0 = cube.cornerperm.(0) in
  cube.cornerperm.(0) <- cube.cornerperm.(1);
  cube.cornerperm.(1) <- cube.cornerperm.(5);
  cube.cornerperm.(5) <- cube.cornerperm.(4);
  cube.cornerperm.(4) <- cp0;
  
  let ep0 = cube.edgeperm.(0) in
  cube.edgeperm.(0) <- cube.edgeperm.(5);
  cube.edgeperm.(5) <- cube.edgeperm.(8);
  cube.edgeperm.(8) <- cube.edgeperm.(4);
  cube.edgeperm.(4) <- ep0;
  
  let cr0 = cube.cornerrot.(0) in
  cube.cornerrot.(0) <- (cube.cornerrot.(1) + 2) mod 3;
  cube.cornerrot.(1) <- (cube.cornerrot.(5) + 1) mod 3;
  cube.cornerrot.(5) <- (cube.cornerrot.(4) + 2) mod 3;
  cube.cornerrot.(4) <- (cr0 + 1) mod 3;
  
  let er0 = cube.edgerot.(0) in
  cube.edgerot.(0) <- (1 - cube.edgerot.(5));
  cube.edgerot.(5) <- (1 - cube.edgerot.(8));
  cube.edgerot.(8) <- (1 - cube.edgerot.(4));
  cube.edgerot.(4) <- (1 - er0)
;;

let l2 cube =
  let cp0, cp2 = cube.cornerperm.(0), cube.cornerperm.(4) in
  cube.cornerperm.(0) <- cube.cornerperm.(5);
  cube.cornerperm.(5) <- cp0;
  cube.cornerperm.(4) <- cube.cornerperm.(1);
  cube.cornerperm.(1) <- cp2;
  
  let ep0, ep2 = cube.edgeperm.(0), cube.edgeperm.(4) in
  cube.edgeperm.(0) <- cube.edgeperm.(8);
  cube.edgeperm.(8) <- ep0;
  cube.edgeperm.(4) <- cube.edgeperm.(5);
  cube.edgeperm.(5) <- ep2;
  
  let cr0, cr2 = cube.cornerrot.(0), cube.cornerrot.(4) in
  cube.cornerrot.(0) <- cube.cornerrot.(5);
  cube.cornerrot.(5) <- cr0;
  cube.cornerrot.(4) <- cube.cornerrot.(1);
  cube.cornerrot.(1) <- cr2;
  
  let er0, er2 = cube.edgerot.(0), cube.edgerot.(4) in
  cube.edgerot.(0) <- cube.edgerot.(8);
  cube.edgerot.(8) <- er0;
  cube.edgerot.(4) <- cube.edgerot.(5);
  cube.edgerot.(5) <- er2
;;

(*Face R*)

let r cube =
  let cp0 = cube.cornerperm.(2) in
  cube.cornerperm.(2) <- cube.cornerperm.(6);
  cube.cornerperm.(6) <- cube.cornerperm.(7);
  cube.cornerperm.(7) <- cube.cornerperm.(3);
  cube.cornerperm.(3) <- cp0;
  
  let ep0 = cube.edgeperm.(2) in
  cube.edgeperm.(2) <- cube.edgeperm.(6);
  cube.edgeperm.(6) <- cube.edgeperm.(10);
  cube.edgeperm.(10) <- cube.edgeperm.(7);
  cube.edgeperm.(7) <- ep0;
  
  let cr0 = cube.cornerrot.(2) in
  cube.cornerrot.(2) <- (cube.cornerrot.(6) + 2) mod 3;
  cube.cornerrot.(6) <- (cube.cornerrot.(7) + 1) mod 3;
  cube.cornerrot.(7) <- (cube.cornerrot.(3) + 2) mod 3;
  cube.cornerrot.(3) <- (cr0 + 1) mod 3;
  
  let er0 = cube.edgerot.(2) in
  cube.edgerot.(2) <- (1 - cube.edgerot.(6));
  cube.edgerot.(6) <- (1 - cube.edgerot.(10));
  cube.edgerot.(10) <- (1 - cube.edgerot.(7));
  cube.edgerot.(7) <- (1 - er0)
;;

let r' cube =
  let cp0 = cube.cornerperm.(2) in
  cube.cornerperm.(2) <- cube.cornerperm.(3);
  cube.cornerperm.(3) <- cube.cornerperm.(7);
  cube.cornerperm.(7) <- cube.cornerperm.(6);
  cube.cornerperm.(6) <- cp0;
  
  let ep0 = cube.edgeperm.(2) in
  cube.edgeperm.(2) <- cube.edgeperm.(7);
  cube.edgeperm.(7) <- cube.edgeperm.(10);
  cube.edgeperm.(10) <- cube.edgeperm.(6);
  cube.edgeperm.(6) <- ep0;
  
  let cr0 = cube.cornerrot.(2) in
  cube.cornerrot.(2) <- (cube.cornerrot.(3) + 2) mod 3;
  cube.cornerrot.(3) <- (cube.cornerrot.(7) + 1) mod 3;
  cube.cornerrot.(7) <- (cube.cornerrot.(6) + 2) mod 3;
  cube.cornerrot.(6) <- (cr0 + 1) mod 3;
  
  let er0 = cube.edgerot.(2) in
  cube.edgerot.(2) <- (1 - cube.edgerot.(7));
  cube.edgerot.(7) <- (1 - cube.edgerot.(10));
  cube.edgerot.(10) <- (1 - cube.edgerot.(6));
  cube.edgerot.(6) <- (1 - er0)
;;

let r2 cube =
  let cp0, cp2 = cube.cornerperm.(2), cube.cornerperm.(3) in
  cube.cornerperm.(2) <- cube.cornerperm.(7);
  cube.cornerperm.(7) <- cp0;
  cube.cornerperm.(3) <- cube.cornerperm.(6);
  cube.cornerperm.(6) <- cp2;
  
  let ep0, ep2 = cube.edgeperm.(2), cube.edgeperm.(7) in
  cube.edgeperm.(2) <- cube.edgeperm.(10);
  cube.edgeperm.(10) <- ep0;
  cube.edgeperm.(7) <- cube.edgeperm.(6);
  cube.edgeperm.(6) <- ep2;
  
  let cr0, cr2 = cube.cornerrot.(2), cube.cornerrot.(3) in
  cube.cornerrot.(2) <- cube.cornerrot.(7);
  cube.cornerrot.(7) <- cr0;
  cube.cornerrot.(3) <- cube.cornerrot.(6);
  cube.cornerrot.(6) <- cr2;
  
  let er0, er2 = cube.edgerot.(2), cube.edgerot.(7) in
  cube.edgerot.(2) <- cube.edgerot.(10);
  cube.edgerot.(10) <- er0;
  cube.edgerot.(7) <- cube.edgerot.(6);
  cube.edgerot.(6) <- er2
;;

let rec sequence (cube:cube) seq = List.iter (fun mv -> mv cube) seq;;

let adjacency = [(l, l', l2); (r, r', r2); (f, f', f2); (b, b', b2); (u, u', u2); (d, d', d2); ]

let pruned_adjacency mv =
  let rec is_in elt = function
    | [] -> false
    | v::s -> (elt==v) || (is_in elt s) in
  if is_in mv [u; u'; u2] then
    [(d, d', d2); (l, l', l2); (r, r', r2); (f, f', f2); (b, b', b2)]
  else if is_in mv [d;d';d2] then
    [(u,u',u2); (l, l', l2); (r, r', r2); (f, f', f2); (b, b', b2)]
  else if is_in mv [l; l'; l2] then
    [(u,u',u2); (d, d', d2); (r, r', r2); (f, f', f2); (b, b', b2)]
  else if is_in mv [r; r'; r2] then
    [(u,u',u2); (d, d', d2); (l, l', l2); (f, f', f2); (b, b', b2)]
  else if is_in mv [f; f'; f2] then
    [(u,u',u2); (d, d', d2); (l, l', l2); (r, r', r2); (b, b', b2)]
  else
    [(u,u',u2); (d, d', d2); (l, l', l2); (r, r', r2); (f, f', f2)]
;;

let best_pruned_adjacency mv1 mv2 =
  let rec is_in elt = function
      | [] -> false
      | v::s -> (elt==v) || (is_in elt s) in
  (*Test si les 2 mouvements sont sur des faces opposées*)
  if ((is_in mv1 [u;u';u2]) && (is_in mv2 [d;d';d2] )) || ((is_in mv2 [u;u';u2]) && (is_in mv1 [d;d';d2])) then
    [(l, l', l2); (r, r', r2); (f, f', f2); (b, b', b2)]
  else if ((is_in mv1 [l;l';l2]) && (is_in mv2 [r;r';r2])) || ((is_in mv2 [l;l';l2]) && (is_in mv1 [r;r';r2])) then
    [(u,u',u2); (d, d', d2); (f, f', f2); (b, b', b2)]
  else if ((is_in mv1 [f;f';f2]) && (is_in mv2 [b;b';b2])) || ((is_in mv2 [f;f';f2]) && (is_in mv1 [b;b';b2])) then
    [(u,u',u2); (d, d', d2); (l, l', l2); (r, r', r2)]
  (*Sinon, adjacence améliorée classique*)
  else pruned_adjacency mv1
;;

let mvs_array = [|f;f';f2;b;b';b2;r;r';r2;l;l';l2;u;u';u2;d;d';d2|];;

let move_tbl =
  let t = Hashtbl.create 19
  and mvs = [|("r2",r2);("r'",r');("r",r);
              ("l2",l2);("l'",l');("l",l);
              ("b2",b2);("b'",b');("b",b);
              ("f2",f2);("f'",f');("f",f);
              ("d2",d2);("d'",d');("d",d);
              ("u2",u2);("u'",u');("u",u); ("id",id)|] in
  for i = 0 to 18 do
    let s, mv = mvs.(i) in
    Hashtbl.add t s mv
  done;
  t
;;

let mv_from_stg stg =
  Hashtbl.find move_tbl stg
;;

let stg_tbl =
  let t = Hashtbl.create 19 in
  Hashtbl.iter (fun k v -> Hashtbl.add t v k) move_tbl;
  t
;;

let rec stg_from_mv mv =
  let res = ref "" in
  Hashtbl.iter (fun k v -> if k==mv then res:=v) stg_tbl;
  !res;;

let random_scramble n =
  let meme_face m1 m2 =
    m1/3 = m2/3 in
  let meme_direction m1 m2 =
    m1/6 = m2/6 in
  let rec aux prec1 prec2 l n =
    if n>0 then
      let k = ref (Random.int 18) in
      if prec1>=0 then
        (
          if prec2>=0 && meme_direction prec1 prec2 then
            (
              while meme_direction (!k) prec1 do
                k := Random.int 18
              done;
              aux !k prec1 (mvs_array.(!k)::l) (n-1)
            )
          else 
            (
              while meme_face (!k) prec1 do
                k := Random.int 18
              done;
              aux !k prec1 (mvs_array.(!k)::l) (n-1)
            )
        )
      else aux !k (-1) [mvs_array.(!k)] (n-1)
    else l
  in (aux (-1) (-1) [] n);;

let scramble_cube n =
  let c = copy resolu in
  sequence c (random_scramble n);
  c
;;

let show_cube cube =
  Array.iter (Printf.printf "%d %!") cube.cornerperm; Printf.printf "\n%!";
  Array.iter (Printf.printf "%d %!") cube.cornerrot; Printf.printf "\n%!";
  Array.iter (Printf.printf "%d %!") cube.edgeperm; Printf.printf "\n%!";
  Array.iter (Printf.printf "%d %!") cube.edgerot; Printf.printf "\n%!";;
  
let show_array t =
  Array.iter (fun i -> Printf.printf "%d %!" i) t; Printf.printf "\n%!";; 

(*#####################################################################################################*)
(*#####################################################################################################*)
(*#################################  RECONNAISSANCE UTILISATEUR  ######################################*)
(*#####################################################################################################*)
(*#####################################################################################################*)


type color = W | G | R | B | O | Y
type corner = color*color*color;;
type edge = color*color;;

let get_color c =
  if c='w' || c='W' then W
  else if c='g' || c='G' then G
  else if c='r' || c='R' then R
  else if c='b' || c='B' then B
  else if c='o' || c='O' then O
  else if c='y' || c='Y' then Y
  else failwith "get_color : couleur non reconnue."

let eq c1 c2 =
  match c1, c2 with
  | W, W -> true
  | G, G -> true
  | R, R -> true
  | B, B -> true
  | O, O -> true
  | Y, Y -> true
  | _ -> false;;

let lt c1 c2 =
  let rec onebeforetwo = function
    | [] -> assert false
    | v::s -> if eq c1 v || eq c2 v then eq c1 v
      else onebeforetwo s
  in onebeforetwo [W;G;R;B;O;Y];; (*Lequel on croise en premier dans la liste.*)

let order2 ((a,b):edge) : edge =
  if lt a b then (a,b) else (b,a);;

let rec order3 ((a,b,c):corner) : corner =
  if lt b a then order3 (b,a,c)
  else if lt c a then order3 (c,a,b)
  else if lt c b then order3 (a,c,b)
  else (a,b,c);;

let corner_pos = [|(0,8,38);(2,16,14);(4,24,22);(6,32,30);
                   (42,36,10);(40,12,18);(46,20,26);(44,28,34)|];;

let edge_pos = [|(1,15);(3,23);(5,31);(7,39);
                 (9,37);(13,17);(25,21);(29,33);
                 (41,11);(47,19);(45,27);(43,35)|];;

let corner_digit (corner:corner) =
  let rec aux corner =
    match corner with
    | (W,G,O) -> 0
    | (W,G,R) -> 1
    | (W,R,B) -> 2
    | (W,B,O) -> 3
    | (G,O,Y) -> 4
    | (G,R,Y) -> 5
    | (R,B,Y) -> 6
    | (B,O,Y) -> 7
    | _ -> failwith "corner_digit : Invalid configuration"
  in aux (order3 corner);;

let edge_digit (edge:edge) =
  let rec aux edge =
    match edge with
    | (W,G) -> 0
    | (W,R) -> 1
    | (W,B) -> 2
    | (W,O) -> 3
    | (G,O) -> 4
    | (G,R) -> 5
    | (R,B) -> 6
    | (B,O) -> 7
    | (G,Y) -> 8
    | (R,Y) -> 9
    | (B,Y) -> 10
    | (O,Y) -> 11
    | _ -> failwith "edge_digit : Invalid configuration"
  in aux (order2 edge);;

let corner_rotation (corner:corner) =
  match corner with
  | W,_,_ -> 0
  | _,W,_ -> 1
  | _,_,W -> 2
  | Y,_,_ -> 0
  | _,Y,_ -> 1
  | _,_,Y -> 2
  | _ -> failwith "corner_rotation : Invalid configuration";;

let edge_rotation (edge:edge) =
  match edge with
    | W,_ -> 0
    | _,W -> 1
    | Y,_ -> 0
    | _,Y -> 1
    | G,O -> 0
    | G,R -> 0
    | B,R -> 0
    | B,O -> 0
    | a,b -> if eq a b then failwith "edge_rotation : Invalid configuration" else 1;;

let get_corners s : corner array =
  Array.map (fun (i,j,k) -> (get_color s.[i], get_color s.[j], get_color s.[k])) corner_pos;;

let get_edges s : edge array =
  Array.map (fun (i,j) -> (get_color s.[i], get_color s.[j])) edge_pos;;

let sum_array t =
  let n = Array.length t in
  let s = ref 0 in
  for i = 0 to n-1 do
    s := !s + t.(i)
  done;
  !s;;
  
let pas_doublon n t =
  let count = Array.make n 0 in
  for i = 0 to n-1 do
    let k = t.(i) in
    count.(k) <- count.(k) + 1
  done;
  identical_array count (Array.make n 1);; 

let convert_to_cube s =
  let c, e = get_corners s, get_edges s in
  let cube =
  {
    cornerperm = Array.map corner_digit c;
    edgeperm = Array.map edge_digit e;
    cornerrot = Array.map corner_rotation c;
    edgerot = Array.map edge_rotation e
  }
  in
  if (sum_array cube.cornerrot) mod 3 = 0 &&  (sum_array cube.edgerot) mod 2 = 0
          && pas_doublon 8 cube.cornerperm && pas_doublon 12 cube.edgeperm
  then cube
  else failwith "convert_to_cube : Votre configuration ne respecte pas le théorème fondamental du cube.";;


(*#####################################################################################################*)
(*#####################################################################################################*)
(*#########################################  INDEXATION  ##############################################*)
(*#####################################################################################################*)
(*#####################################################################################################*)

open Printf

let rec pow n k =
  if k = 0 then 1 else n * pow n (k-1);; 

let facto =
  let t = Array.make 14 0 in
  t.(0) <- 1 ;
  for i = 1 to 13 do
    t.(i) <- i*t.(i-1)
  done;
  t
;;

let pow3 =
  let t = Array.make 10 0 in
  t.(0) <- 1;
  for i=1 to 9 do
    t.(i) <- 3 * t.(i-1)
  done;
  t
;;

let pow2 =
  let t = Array.make 14 0 in
  t.(0) <- 1;
  for i=1 to 13 do
    t.(i) <- 2 * t.(i-1)
  done;
  t
;;

let empty_bitset = 0;;


let shift_right set n = (*annule les n bits à gauche*)
  set/pow2.(n)
;;

let set_bit8 set i = set + pow2.(7-i);;
let set_bit12 set i = set + pow2.(11-i);;

let get_bitset8 (set:int) =
  let n = ref set
  and t = Array.make 8 0 in
  for i = 7 downto 0 do
    t.(i) <- (!n) mod 2;
    n := !n /2
  done;
  t
;;

let get_bitset12 (set:int) =
  let n = ref set
  and t = Array.make 12 0 in
  for i = 11 downto 0 do
    t.(i) <- (!n) mod 2;
    n := !n /2
  done;
  t
;;

let howmany0nes tab =
  let c = ref 0 in
  let n = Array.length tab in
  for i = 0 to n-1 do
    c := !c + tab.(i)
  done;
  !c;;

let incr tab =
  let n = Array.length tab in
  let i = ref 0 in
  while !i<(n-1) && (tab.(n-1-(!i))=1) do
    tab.(n-1-(!i)) <- 1 - tab.(n-1-(!i));
    i:= !i + 1
  done;
  tab.(n-1-(!i)) <- 1 - tab.(n-1-(!i))
;;

let count0nes n =
  let t = Array.make (pow2.(n)) 0
  and set = Array.make n 0 in
  for i=0 to (pow2.(n)) - 1 do
    let cp = Array.copy set in
    t.(i) <- howmany0nes cp;
    incr set
  done;
  t
;;

let count0nes8 = count0nes 8;;
let count0nes12 = count0nes 12;;

(*###################### PERMUTATIONS ######################*)

let indexing_eight t =
  let lehmer_base10 = ref 0
  and set = ref 0 in
  for i=0 to 6 do
    let k = t.(i) in
    let position = (k - count0nes8.(shift_right (!set) (8-k))) in

    lehmer_base10 := !lehmer_base10 + position * facto.(8)/facto.(8-i);
    set := set_bit8 (!set) k; (*On "retire k de la liste".*)
  done;
  !lehmer_base10
;;

let edge_permindex1 t =
  let t2 = Array.make 7 (0) in
  for i=0 to 11 do
    let j = t.(i) in
    if j<7 then t2.(j) <- i
  done;
  let lehmer_base10 = ref 0
  and set = ref 0 in
  for i=0 to 6 do
    let k = t2.(i) in

    let position = (k - count0nes12.(shift_right (!set) (12-k))) in
    
    lehmer_base10 := !lehmer_base10 + position * facto.(12)/facto.(12-i);
    set := set_bit12 (!set) k;
  done;
  !lehmer_base10, t2
;;

let edge_permindex2 t =
  let t2 = Array.make 7 (0) in
  for i=0 to 11 do
    let j = t.(i) in
    if j>=5 then t2.(j-5) <- i
  done;
  let lehmer_base10 = ref 0
  and set = ref 0 in
  for i=0 to 6 do
    let k = t2.(i) in
    let position = (k - count0nes12.(shift_right (!set) (12-k))) in
    
    lehmer_base10 := !lehmer_base10 + position * facto.(12)/facto.(12-i);
    set := set_bit12 (!set) k;
  done;
  !lehmer_base10, t2
;;

(*###################### ORIENTATIONS ######################*)

let rotscal6 t =
  let s = ref 0 in
  for i = 0 to 5 do
    s := !s + t.(i)*(pow2.(5-i)) 
  done;
  !s
;;

let rotscal7 t =
  let s = ref 0 in
  for i = 0 to 6 do
    s := !s + t.(i)*(pow2.(6-i)) 
  done;
  !s
;;


let rotscal8 t =
  let s = ref 0 in
  for i = 0 to 6 do
    s := !s + t.(i)*(pow3.(6-i)) 
  done;
  !s
;;

let half1_edgeindex cube =
  let lehmer_code, pos = edge_permindex1 (cube.edgeperm) in
  let orr = Array.map (fun i -> cube.edgerot.(i)) pos in
  (pow2.(7) * lehmer_code) + (rotscal7 orr);;

let half2_edgeindex cube =
  let lehmer_code, pos = edge_permindex2 (cube.edgeperm) in
  let orr = Array.map (fun i -> cube.edgerot.(i)) pos in
  (pow2.(7) * lehmer_code) + (rotscal7 orr);;;;

let corner_index cube = (* De borne supérieure 3^7*8! *)
  (pow3.(7))*(indexing_eight (cube.cornerperm))
  + (rotscal8 (cube.cornerrot));;

(*Corner dtb : 3^7*8! = 88,179,840 states*)

(*################################################################################################*)
(*################################################################################################*)
(*################################## EXPORT ET STATISTIQUES ######################################*)
(*################################################################################################*)
(*################################################################################################*)

let save_array_to_file tab fname =
  let n = Array.length tab in
  let sortie = open_out fname in
  for i = 0 to n-1 do
    Printf.fprintf sortie "%d\n" tab.(i)
  done;
  close_out sortie;
  Printf.printf "Heuristique sauvegarde dans le fichier %S.\n" fname;;

let save_bigarray_to_file tab fname =
  let n = Bigarray.Array1.dim tab in
  let sortie = open_out fname in
  for i = 0 to n-1 do
    Printf.fprintf sortie "%d\n" tab.{i}
  done;
  close_out sortie;
  Printf.printf "Heuristique sauvegarde dans le fichier %S.\n" fname;;

let stats tab =
  let profs = Array.make 20 0 in
  Array.iter (fun v -> profs.(v) <- profs.(v) + 1) tab;
  let i = ref 0 in
  Printf.printf "Statistiques :\n";
  while profs.(!i) > 0 && !i<20 do
    Printf.printf "%d: %d\n" !i (profs.(!i));
    i := !i + 1
  done;;

let bigstats tab =
  let profs = Array.make 20 0 in
  let n = Bigarray.Array1.dim tab in
  for i = 0 to n-1 do
    let v = tab.{i} in
    profs.(v) <- profs.(v) + 1;
  done;
  let i = ref 0 in
  Printf.printf "Statistiques :\n";
  while profs.(!i) > 0 && !i<20 do
    Printf.printf "%d: %d\n" !i (profs.(!i));
    i := !i + 1
  done;;

(*######################################################################################################*)
(*######################################################################################################*)
(*################################# CONSTRUCTION DE L'HEURISTIQUE ######################################*)
(*######################################################################################################*)
(*######################################################################################################*)

let heurDFS indexation borne_sup nombre_configs fname =
  Printf.printf "Initialisation de la table de valeurs.\n%!";
  let heuristic = Bigarray.Array1.init Int8_unsigned Bigarray.c_layout nombre_configs (fun p -> borne_sup) (*Estimation de la borne sup de la profondeur*)
  and cube = copy resolu
  and seen_counter = ref 0
  and improvement_counter = ref 0 in
  Printf.printf "Debut de la recherche heuristique.\n%!";
  let t0 = Sys.time() in
  
  let rec dfs depth =
    let traiter (m, m', m2) =
      m2 cube;
      dfs (depth + 1);

      m cube ;
      dfs (depth + 1);

      m2 cube ;
      dfs (depth + 1);

      m' cube
    in

    let index = indexation cube in
    let approx = heuristic.{index} in
    
    if depth < approx then
      (
        if approx = borne_sup (*Première rencontre du sommet*) then 
          (
            seen_counter := !seen_counter + 1 ;
            if (!seen_counter) mod 10000000 = 0 then Printf.printf "On avance ! %d cubes trouves, %.3f secondes se sont ecoulees.\n%!" (!seen_counter) (Sys.time() -. t0)
            else if (!seen_counter) mod 1000000 = 0 then Printf.printf "%d cubes enregistres. %.3f secondes ecoulees au total.\n%!" (!seen_counter) (Sys.time() -. t0)
          )
        else improvement_counter := !improvement_counter + 1 ;
        
        heuristic.{index} <- depth;
        List.iter traiter adjacency
      )
    
  in dfs 0;
  let t1 = Sys.time() in
  Printf.printf "Recherche terminee en %s.\n%d cubes trouvés, et %d revisites d'états déjà trouvés, soit environ %d revisites par état.\nExport des donnees dans un fichier csv.\n%!"
  (format_time (t1 -. t0)) (!seen_counter) (!improvement_counter) ((!improvement_counter) / (!seen_counter));
  save_bigarray_to_file heuristic fname;
  Printf.printf "L'export a dure %.3f secondes.\n%!" (Sys.time() -. t1);
  bigstats heuristic ;
  heuristic
;;

(*
let h = heurDFS corner_index 13;;

let h1 = heurDFS half1_edgeindex 15 (facto.(12)/facto.(5)*pow2.(7)) "edge1_dtbV3.csv";;
let h2 = heurDFS half2_edgeindex 15 (facto.(12)/facto.(5)*pow2.(7)) "edge2_dtbV3.csv";;*)


(*##################################################################################################*)
(*##################################################################################################*)
(*########################################### RESOLUTION ###########################################*)
(*##################################################################################################*)
(*##################################################################################################*)

let import_array file_name number_line =
  printf "Import de données depuis %s\n%!" file_name;
  let h = Array.make number_line 0 in
  let i = ref 0 in
  try
    let ic = open_in file_name in
    try
      while !i<number_line; do
        (* Create a list of values from a line *)
        let line = input_line ic in
          h.(!i) <- int_of_string line;
        i := !i + 1
      done;
      printf "Fait.\n%!";
      h
    with 
      | End_of_file -> close_in ic; printf "Done.\n%!"; h
    with
      | e -> raise e;;

let import_bigarray file_name number_line =
  printf "Import de données depuis %s\n%!" file_name;
  let h = Bigarray.Array1.init Int8_unsigned Bigarray.c_layout number_line (fun p -> 0) in
  let i = ref 0 in
  try
    let ic = open_in file_name in
    try
      while !i<number_line; do
        (* Create a list of values from a line *)
        let line = input_line ic in
          h.{!i} <- int_of_string line;
        i := !i + 1
      done;
      printf "Done.\n%!";
      h
    with 
      | End_of_file -> close_in ic; printf "Done.\n%!"; h
    with
      | e -> raise e;;



let () = print_endline "Des données vont être importées depuis des fichiers externes.\nCela peut prendre quelques minutes.\n";;

let heur_coins_tab = import_array "corner_dtbV2.csv" (facto.(8)*pow3.(7));;
let half1_edgeheur_tab = import_bigarray "edge1_dtbV3.csv" (facto.(12)/facto.(5)*pow2.(7)) ;;
let half2_edgeheur_tab = import_bigarray "edge2_dtbV3.csv" (facto.(12)/facto.(5)*pow2.(7));;

let heur_coins cube = heur_coins_tab.(corner_index cube);;
let half1_edgeheur cube = half1_edgeheur_tab.{half1_edgeindex cube};;
let half2_edgeheur cube = half2_edgeheur_tab.{half2_edgeindex cube};;

let () = print_endline "L'import des données s'est déroulé avec succès.\n Vous pouvez maintenant utiliser le solveur !";;


let improved_heuristic cube = 
  let rec findmax = function
  | [] -> failwith "max : empty list"
  | v::[] -> v
  | v::s -> max v (findmax s)
  in findmax [heur_coins cube;
              half1_edgeheur cube;
              half2_edgeheur cube]
;;

let show_mvlist l = List.iter (fun mv -> Printf.printf "%s %!" (stg_from_mv mv)) l ; Printf.printf "\n%!";;



let rec tri_a_bulle l = (*Tri exclusivement en fonction de la première composante (ici l'heuristique)*)
  let rec remontee = function
  | [] -> []
  | [v] -> [v]
  | (x1,x2,x3)::(y1,y2,y3)::s -> if x1<=y1 then (x1,x2,x3)::(remontee ((y1,y2,y3)::s))
                           else (y1,y2,y3)::(remontee ((x1,x2,x3)::s))
  in
  match remontee l with
  | [] -> []
  | x::s -> x::(tri_a_bulle s)
;;


let ida_star start heuristic =
  let cube = copy start
  and upper_bound = ref 21
  and final_sequence = ref []
  and found = ref false in
  let get_heur mv recip =
    mv cube;
    let heur = heuristic cube in
    recip cube;
    heur
  in
  let order adjacency =
    let rec aux = function
    | [] -> []
    | (m, m', m2)::s -> (get_heur m m', m, m') :: (get_heur m' m, m', m) :: (get_heur m2 m2, m2, m2) :: (aux s) in
    let l = aux adjacency in
    tri_a_bulle l
  in
  let t0 = Sys.time() in
  let rec dfs depth approx bound move_list =
    let traiter (heur, m, m') =
      m cube ;
      dfs (depth + 1) heur bound (m::move_list);
      m' cube
      in
          if are_identical cube resolu && List.length move_list < !upper_bound then 
            (
            let t1 = Sys.time() in
            printf "Trouvé !!\nRecherche terminée en %s.\nSéquence résolvante : %!" (format_time (t1 -. t0));
            found:=true;
            upper_bound := List.length move_list;
            final_sequence := List.rev move_list;
            show_mvlist (!final_sequence);
          );

        if (not !found) && (depth + approx < bound) then match move_list with
          | [] -> List.iter traiter (order adjacency)
          | mv1::mv2::s -> List.iter traiter (order (best_pruned_adjacency mv1 mv2))
          | mv::s -> List.iter traiter (order (pruned_adjacency mv))
      in
    let bnd = ref (heuristic start) in
    printf "\nDébut de la recherche\n%!";
    let tint = ref (Sys.time ()) in
    while !bnd < !upper_bound do
      dfs 0 (heuristic start) !bnd [];
      bnd := !bnd + 1;
      let t1 = Sys.time () in
      if not !found then
        Printf.printf "Profondeur %d explorée en %s.\n%!" (!bnd-1) (format_time (t1 -. !tint));
      tint := t1
    done;
  !final_sequence;;

(*######################################################################################################*)
(*######################################################################################################*)
(*########################################## ZONE UTILISATEUR ##########################################*)
(*######################################################################################################*)
(*######################################################################################################*)

let g = copy resolu;;

let is_number s =
  let n = String.length s in
  let l = ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9'] in
  let test = ref true in
  let rec is_in l v =
    match l with
    | [] -> false
    | x::s -> (x=v) || is_in s v in 
  for i=0 to n-1 do
    test := !test && (is_in l s.[i])
  done;
  !test;;

(*
let lst = random_scramble 13;;
let () = show_mvlist lst;;
let () = sequence g lst;;


let () = show_cube g;;

let seq = ida_star g improved_heuristic;;*)

let get_scramble s =
  let cur_move = ref ""
  and seq = ref []
  and n = String.length s in
  for i=0 to n-1 do
    if s.[i] = ' ' || i=n-1 then
      (
        if i=n-1 then 
          (
            if s.[i] = '\'' then cur_move := !cur_move ^ "'"
            else cur_move := !cur_move ^ (Char.escaped s.[i])
          );
        if Hashtbl.mem move_tbl (String.lowercase_ascii !cur_move) then
        (
          seq := !seq @ [!cur_move];
          cur_move := ""
        )
        else (print_string !cur_move; failwith "get_scramble : Illegal move.")
      )
    else
      (
        if s.[i] = '\'' then cur_move := !cur_move ^ "'"
        else cur_move := !cur_move ^ (Char.escaped s.[i])
      )
  done;
  List.map mv_from_stg !seq;;

let scramble_from_stg s =
  let g = copy resolu in
  sequence g (get_scramble s);
  g;;

(*###### Main Loop #######*)

let keep_going = ref true;;

let () = 
  while !keep_going do
    print_endline "\nVoulez vous...\n- Générer un cube aléatoire ? (A)\n- Renseigner une séquence de mélange ? (M)\n- Renseigner le codage de votre état à résoudre ? (R)";
    let inp = ref (String.uppercase_ascii (read_line ())) in
    while !inp <> "A" && !inp <> "R" && !inp <> "M" do
      print_endline "Veuillez répondre correctement.";
      inp := String.uppercase_ascii (read_line ())
    done;
    
    if !inp = "A" then
      (
        print_endline "\nUn nouveau mélange va être généré aléatoirement.\nDe quel profondeur voulez-vous qu'il soit ?";
        inp := (read_line ());
        while not (is_number (!inp)) || (int_of_string (!inp) <=0) do
          print_endline "Veuillez entrer une valeur valide.";
          inp := read_line ()
        done;
        let prof = int_of_string (!inp) in
        let g = copy resolu in
        let lst = random_scramble prof in
        print_string "Mélange choisi : ";
        show_mvlist lst;
        sequence g lst;
        let _ = ida_star g improved_heuristic in ()
      )
    
    else if !inp = "R" then
      (
        print_endline "\nVeuillez renseigner le codage de votre configuration.";
        inp := (read_line ());
        let rec loop () =
          try
            let g = convert_to_cube !inp in
            let _ = ida_star g improved_heuristic in ()
          with
          | _ -> print_endline "Erreur : configuration illégale."; inp := (read_line ()); loop ()
        in loop ()
      )
    
    else
      (
        print_endline "\nVeuillez renseigner la suite de mouvements à appliquer.";
        inp := (read_line ());
        let rec loop () =
          try
            let g = scramble_from_stg !inp in
            let _ = ida_star g improved_heuristic in ()
          with
          | _ -> print_endline "Erreur : mouvement inconnu"; inp := (read_line ()); loop ()
        in loop ()
      );


    print_endline "\nVoulez vous lancer une nouvelle résolution ? (Y/N)";
    inp := String.uppercase_ascii (read_line ());
    while !inp <> "Y" && !inp <> "N" do
      print_endline "Veuillez répondre correctement.";
      inp := String.uppercase_ascii (read_line ())
    done;
    keep_going := (!inp="Y")
  done;;


(*####### Main Loop #######
let () =
  for p = 1 to 16 do
    printf "Test à profondeur %d. " p;
    let g = scramble_cube p in
    let _ = ida_star g improved_heuristic in ()
  done;;
let _ = read_line ();;*)