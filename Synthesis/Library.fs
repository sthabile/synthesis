module Synthesis

let abelar num =
    (num >12) && (num<3097) && (num%12=0)

let area b a=
    match a<0.0 || b<0.0 with
    | true ->failwith "NegativeInputException"
    | false -> (0.5*b)*a

let zollo num =
    match num<0 with
    |true -> num * -1
    |false -> num *2

let min a b =
    match a<b with
    |true -> a
    |false -> b
    

let max b a =
    match a>b with
    |true -> a
    |false -> b

let ofTime hours mins secs= secs + mins*60 + hours*3600
 

let toTime secs =
    match secs >0 with 
    |false -> (0,0,0)
    |_->
          let hour = secs / 3600
          let mins =  match (secs-hour*3600) >= 60 with
                      |true -> (secs-hour*3600)/60
                      |false ->0
          let secs= ((secs-(secs/3600)*3600)-(((secs-(secs/3600)*3600))/60)*60)
          (hour,mins,secs)


let rec digits num =
    let rec sum value count=
        match value =0 with 
        |false -> sum (value/10) (count+1)
        |true -> count
    match num <> 0 with
        |true -> sum (num) 0
        |_ ->1
      

let minmax (a,b,c,d) =
    min a b |> min c|> min d, max a b |> max c|>max d
  

let isLeap year =
     match (year<1582) with
     |true -> failwith "Shouldfail"
     |false ->  match (year % 4=0 && year % 100 <> 0) || (year % 4=0  && year % 400 = 0) with
                |true -> true
                | _ -> false
     

let month num =
    match num<1 || num > 12 with
    |false -> match num with
              |1 -> ("January",31)
              |2 -> ("February",28)
              |3 -> ("March",31)
              |4 -> ("April",30)
              |5 -> ("May",31)
              |6 -> ("June",30)
              |7 -> ("July",31)
              |8 -> ("August",31)
              |9 -> ("September",30)
              |10 -> ("October",31)
              |11 -> ("November",30)
              | _-> ("December",31)
    | _ -> failwith "Not implemented"

let toBinary num =
    match num <0 with
    |true -> failwith "Not implemented"
    |false ->
        let rec WhileLoop num acc =
            match num <> 0 with
            | true -> match (num%2) with
                       | 0 -> WhileLoop (num/2) ("0" + acc)
                       | 1 -> WhileLoop (num/2) ("1" + acc)
            | false -> acc
        match num = 0 with
        | true -> "0"
        | false -> WhileLoop num ""

        

let bizFuzz  n  =
    let rec mighty v (a,b,c)=
        match v>=1 with 
       |true ->   match (v%3= 0, v%5=0, (v%3=0 && v%5=0)) with
               |true,true ,true-> mighty (v-1)  (a+1,b+1,c+1)
               |true,false,false -> mighty (v-1) (a+1,b+0,c+0)
               |false,true,false -> mighty (v-1) (a+0,b+1,a+0)
               |_ -> mighty (v-1) (a+0,b+0,c+0)
        |_->(a,b,c)
    mighty n (0,0,0)

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"
    
