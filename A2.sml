(*
Your name:Sylvain Taghaoussi
Your student id:v00896706
*)

structure Set =
struct
local
  open Csc330
in

datatype 'a set = EmptySet of ('a * 'a -> order) | Set of 'a list * ('a * 'a -> order)

exception SetIsEmpty

infix 1 IDENTICAL
infix 3 EXCEPT
infix 4 IS_SUBSET_OF
infix 5 INTERSECT
infix 6 UNION
infix 7 IN
infix 8 CONTAINS        
infix 9 ++
infix 9 --

fun is_empty_set s =
    case s of
    EmptySet _ => true
    | other =>  false

fun min_in_set s =
    case s of
    EmptySet f => raise SetIsEmpty
    | Set(xs,f) => 
        let
            fun calc_min(xs,min,f)=
                case xs of
                [] => min
                |x::xs'=> 
                    case f(x,min) of
                    EQUAL => calc_min(xs',min,f)
                    | LESS => calc_min(xs',x,f)
                    | GREATER => calc_min(xs',min,f)
        in
            if null xs then raise SetIsEmpty else calc_min(xs,hd xs, f)           
        end

fun max_in_set s =
   case s of
   EmptySet f => raise SetIsEmpty
   | Set(xs,f) => 
       let
           fun calc_max(xs,max,f)=
               case xs of
               [] => max
               |x::xs'=> 
                   case f(x,max) of
                   EQUAL => calc_max(xs',max,f)
                   | LESS => calc_max(xs',max,f)
                   | GREATER => calc_max(xs',x,f)
       in
           if null xs then raise SetIsEmpty else calc_max(xs,hd xs, f)           
       end


fun insert_into_set(s,v) =
    case s of
    EmptySet f => Set([v],f)
    | Set(lst,f) =>
        case lst of
        [] => Set([v],f)
        | y::ys' => 
        let
            fun insert_sorted(arr: 'a list,v: 'a,f): 'a list=
                case arr of
                    [] => [v]
                    | x::xs' => 
                        if null xs' 
                        then
                            case f(x,v) of
                            EQUAL => [x]
                            | GREATER => [v]@[x]
                            | LESS => [x]@[v]
                        else
                            let
                                val tail_head = hd xs'
                            in
                        
                            (* ORDER -> 'a list *)
                                case (f(x,v), f(tail_head,v)) of
                                (EQUAL,_) => [x]@xs'
                                | (_,EQUAL) => [x]@xs'
                                | (LESS, GREATER) => [x]@[v]@xs'
                                | (LESS, LESS) => [x]@insert_sorted(xs',v,f)
                                | (GREATER, _) => [v]@[x]@xs'

                            end
            val sorted_lst = insert_sorted([y]@ys',v,f)
            val sorted_set = Set(sorted_lst,f)
        in
          sorted_set
        end


fun in_set(s, v) =
    case (s,v) of
        (*case where is int in set*)
        (EmptySet fa,v)=> false
        | (Set(a,fa),v)=>
            case a of
                [] => false
                | x::xs' => if fa(x,v) = EQUAL then true else in_set(Set(xs',fa),v)
        
fun is_subset_of(s, t) =
    case (s,t) of
        (EmptySet fa, EmptySet fb) => true
        | (EmptySet fa, Set(b,fb)) => true
        | (Set(a,fa),EmptySet fb) => if null a then true else false
        | (Set(a,fa),Set(b,fb)) => 
            case (a,b) of
            (x::xs',[])=>false
            | ([],y::ys') => true
            | ([],[]) => true
            | (x::xs',y::ys')=>
            let
                fun belongs(lst,set)=
                    case lst of
                    [] => true
                    | x::xs' => 
                    if in_set(set,x) then true else belongs(xs',set)
            in
                belongs([x]@xs',Set([y]@ys',fb))
            end

                


fun union_set(s, t) =
    case (s,t) of
        (EmptySet fa, EmptySet fb) => Set([],fa)
        | (EmptySet fa, Set(b,fb)) => Set(b,fb)
        | (Set(a,fa),EmptySet fb) => Set(a,fa)
        | (Set(a,fa),Set(b,fb)) => 
        let
            val u_set = Set([],fa)
            fun insert_sets(xs,ys,set)=
                case (xs,ys) of
                    ([],[]) => set
                    | (x::xs',[]) => 
                       insert_sets(xs',[], insert_into_set(set,x))
                    | ([], y::ys') =>
                        insert_sets([],ys', insert_into_set(set,y))
                    | (x::xs',y::ys') =>
                        insert_sets(xs',ys', insert_into_set(insert_into_set(set,x),y))
        in
            insert_sets(a,b,u_set)
        end

fun intersect_set(s, t) =
    case (s,t) of
        (EmptySet fa, EmptySet fb) => Set([],fa)
        | (EmptySet fa, Set(b,fb)) => Set([],fb)
        | (Set(a,fa),EmptySet fb) => Set([],fa)
        | (Set(a,fa),Set(b,fb)) => 
        let
            val u_set = Set([],fa)
            fun insert_sets(xs,ys,set,f)=
                case (xs) of
                    [] => set
                    | x::xs' =>
                        if in_set(Set(ys,f),x) then insert_sets(xs',ys,insert_into_set(set,x),f) else insert_sets(xs',ys,set,f)
        in
            insert_sets(a,b,u_set,fa)
        end

fun remove_from_set(s,v) =
    case s of
        EmptySet f => EmptySet f
        | Set(lst, f) =>
        let
            fun remove(lst,v,f)=
                case lst of
                [] => []
                | x::xs' => if f(x,v) = EQUAL then remove(xs',v,f) else [x]@remove(xs',v,f)
        in
            Set(remove(lst,v,f),f)
        end

fun except_set(s, t) = 
    case (s,t) of
        (EmptySet fa, EmptySet fb) => Set([],fa)
        | (EmptySet fa, Set(b,fb)) => Set([],fb)
        | (Set(a,fa),EmptySet fb) => Set([],fa)
        | (Set(a,fa),Set(b,fb)) => 
        let
            val e_set = Set(a,fa)
            fun rem_e(lst, set)=
                case lst of
                [] => set
                | x::xs' => rem_e(xs',remove_from_set(set,x))
        in
            rem_e(b,e_set)
        end

    
fun size_set(s: 'a set) =
    case s of
        EmptySet f => 0
        | Set(lst,f)=>
            let
                fun list_size(xs)=
                    if null xs
                    then 0
                    else 1 + list_size(tl xs)
            in
                list_size(lst)
            end
            

fun equal_set(s, t) =
    case (s,t) of
        (EmptySet fa, EmptySet fb) => true
        | (EmptySet fa, Set(b,fb)) => if null b then true else false
        | (Set(a,fa),EmptySet fb) => if null a then true else false
        | (Set(a,fa),Set(b,fb)) => 
            if size_set(Set(a,fa)) = size_set(Set(b,fb))
            then
                let
                    fun compare_lists(xs,ys,f)=
                        case (xs,ys) of
                        ([],[]) => true
                        | (x::xs',[]) => false
                        | ([], y::ys') => false
                        | (x::xs',y::ys') => if f(x,y) = EQUAL then compare_lists(xs',ys',f) else false
                in
                    compare_lists(a,b,fa)
                end
            else
                false

        
fun list_to_set(lst, f) =
    let
        val constructed_set = Set([],f)

        fun insert_all_elements(lst, set)=
            case lst of
            [] => set
            | x::xs' => 
                let
                    val new_set = insert_into_set(set,x)
                in
                    insert_all_elements(xs',new_set)
                end

    in
        insert_all_elements(lst,constructed_set)
    end


fun set_to_list s =
    case s of
    EmptySet f => []
    | Set(a,fa) =>
    let
        fun r (lst, acc)=
            case lst of
            [] => acc
            | x::xs' => r(xs',acc@[x])
    in
        r(a,[])
    end

fun str_set (s, fstr) =
    let
        fun loop(lst,f)=
            case lst of
                []=> ""
                |x::xs' =>
                if null xs' then f(x) else f(x) ^":"^loop(xs',f)
    in
        case s of
        EmptySet f => "{}"
        | Set(a,fa) => "{"^loop(a,fstr)^"}"
    end
      
fun map_set (s, fcomp, f) =
    case s of
    EmptySet f => EmptySet fcomp
    | Set(a,fa)=>
    let
        fun map(f,lst)=
            case lst of
            [] => []
            | x::xs'=> f(x)::map(f,xs')
    in
        Set(map(f,a),fcomp)
    end

fun s -- v = remove_from_set(s,v)
fun s ++ v = insert_into_set(s,v)
fun s IDENTICAL t = equal_set(s,t)
fun s UNION t = union_set(s,t)
fun s INTERSECT t = intersect_set(s,t)
fun s EXCEPT t = except_set(s,t)
fun v IN s = in_set(s,v)
fun s IS_SUBSET_OF t = is_subset_of(s,t)


fun comp_list_any (a: 'a list, b: 'a list, fcomp : ('a * 'a) -> order) =          
    case (a,b) of
    ([],[]) => EQUAL
    |   ([],_) => LESS
    |   (_, []) => GREATER
    |   (x,y) => 
    case fcomp(hd x, hd y) of
    EQUAL => comp_list_any(tl x, tl y, fcomp)
    |   GREATER => GREATER
    |   LESS => LESS
end
end    
