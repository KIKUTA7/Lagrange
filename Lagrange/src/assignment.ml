let rec lagrangeFirst x lst templst index = 
  let rec lagrangeSecondPart templst xj index counter = 
    match templst with
    |[] -> 1.0
    |(xk,yk)::xs -> if (counter <> index) then ( (x -. xk) /.  (xj -. xk))*.lagrangeSecondPart xs xj index (counter + 1) 
        else lagrangeSecondPart xs xj index (counter + 1)
  in
  match lst with
  |[] -> 0.0
  |(xj,yj)::xs -> (yj *. (lagrangeSecondPart templst xj index 0) +. (lagrangeFirst x xs templst (index + 1)));;



let lagrange lst x =  lagrangeFirst x lst lst 0;;

lagrange [100.,231.; 200.,12.; 300.,382.5] 0.3;;
  