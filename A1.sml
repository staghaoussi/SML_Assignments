structure Babies =
struct
local
  open Csc330
in
  
(*
Student name:
Student id:

Add your code below this line

Remember: your submitted code cannot use print

*)

type record = {name: string, year_freq: int list, sum_freq: int}
(* Function that returns length of list*)
fun str_list_len(lst): int=
    if null lst
    then 0
    else 1 + str_list_len(tl lst)


(*
name -> string name to be compared to names selected
names -> list of names selected
Function loops through all selected names and returns a boolean if it is selected
*)
fun is_selected (name: string, names: string list): bool =
    if null names
    then false
    else if name = hd names then true
    else is_selected(name, tl names)


(* Returns the sum of the frequencies*)
fun year_freq_sum(lst: string list): int=
    let
        val tail = tl lst
    in
        if null tail
        then
            let
                val int_option = fromString(hd lst)
            in
                valOf int_option
            end
        else year_freq_sum(tail)
    end


(* 
baby_line -> baby line with all corresponding stats
Returns int list of year frequencies
*)
fun year_frequency(baby_line: string list): int list=
    if null baby_line
    then []
    else
        let
            val len = str_list_len(baby_line)
        in
            if len < 2
            then []
            else
                let
                    val element_option = fromString(hd baby_line)
                in
                    valOf element_option :: year_frequency(tl baby_line)
                end
        end
        



        


(*
babies -> list of lines for each baby
names -> list of names selected
Function returns a list of records of the names selected
*)
fun return_records(babies: string list, names: string list)=
    if null babies
    then []
    else
        let
            val baby_line = split_at(hd babies, #",")
        in    
            if is_selected(hd baby_line, names)
            then
            
            let
                val record = {name= hd baby_line, year_freq= year_frequency(tl baby_line), sum_freq=year_freq_sum(baby_line)}
            in
                record :: return_records (tl babies, names)
            end
    
            else return_records(tl babies, names)
        end


(* 
name -> name of record to return
selected_records -> list of selected records
Returns option storing specified record
*)
fun return_name_record_option(name: string, selected_records: record list)=
    if null selected_records
    then NONE
    else
        let
            val name_of_record = #name(hd selected_records)
        in
            if name = name_of_record
            then
                SOME (hd selected_records)
            else
                return_name_record_option(name, tl selected_records)
        end

(* Function counts the amount of non zero years*)
fun non_zero_years(years: int list):int=
    if null years
    then 0
    else
        let
            val cur_year_val = hd years
        in
            if cur_year_val > 0
            then 1 + non_zero_years(tl years)
            else non_zero_years(tl years)
        end


(*Function calculates amount on last year of record*)        
fun amount_on_LY(years: int list):int=
    if null years
    then 0
    else
        let
            val tail = tl years
        in
            if null tail
            then hd years
            else amount_on_LY(tail)
        end


(*calculates first non zero value in the list*)
fun first_non_zero(years: int list, year_count: int): int*int=
    let
        val yc = year_count + 1
    in
        if null years
        then (0,yc)
        else
            if hd years > 0
            then
                (hd years, yc)
            else
                first_non_zero(tl years, yc)
    end

(*function that calculates the max value and its year*)
fun max(years: int list, mv: int, cur_yc:int, mv_yc: int):int*int=
    let
        val yc = cur_yc + 1
    in
        if null years
        then (mv,mv_yc)
        else
            if mv <= hd years
            then
                let
                    val max_val = hd years
                in
                    max(tl years, max_val, yc, yc)
                end
            else
                max(tl years, mv, yc, mv_yc)
    end

(*function that calculates the minimum non zero value in the list, and its year*)
fun min(years: int list, mv: int, cur_yc, mv_yc: int):int*int=
    let val yc = cur_yc + 1
    in
        if null years
        then
            if mv < 10000
            then
                (mv,mv_yc)
            else
                (0,0)
        else
            if hd years = 0
            then
                min(tl years, mv, yc, mv_yc)
            else
                if mv > hd years
                then
                    let
                        val min_val = hd years
                    in
                        min(tl years, min_val, yc, yc)
                    end
                else
                    min(tl years, mv, yc, mv_yc)
    end




(* Function that constructs the output strings and calls all the calculation functions*)
fun compute_names_output(names: string list, selected_records: record list, offsetSt: string)=
    if null names
    then ""
    else
        let
            val cur_name = hd names
            val cur_name_record_option = return_name_record_option(cur_name, selected_records)
        in
            if isSome cur_name_record_option
            

            then
                let
                    val cur_name_record = valOf cur_name_record_option

                    val offset_int_opt = fromString(offsetSt)
                    val num_entries = str_list_len(#year_freq(cur_name_record))

                    (* calculate min val*)
                    val min_tuple = min(#year_freq(cur_name_record), 10001, ~1, 0)
                    val min_val_year = #2 min_tuple + valOf offset_int_opt
                    val min_val = #1 min_tuple
                    
                    (* calculate max val*)
                    val max_tuple = max(#year_freq(cur_name_record), 0, ~1, 0)
                    val max_val_year = #2 max_tuple + valOf offset_int_opt
                    val max_val = #1 max_tuple

                    (* calculate last non-zero*)
                    val list_to_be_reversed = #year_freq(cur_name_record)
                    val reversed_list = rev list_to_be_reversed
                    val last_nz_tuple = first_non_zero(reversed_list, ~1)
                    val last_nz_val = #1 last_nz_tuple
                    val last_nz_year = valOf offset_int_opt + num_entries - 1 - #2 last_nz_tuple

                    (* calculate first non zero*)
                    val first_nz_tuple = first_non_zero(#year_freq(cur_name_record),~1)
                    val first_nz_val = #1 first_nz_tuple
                    val first_nz_year = valOf offset_int_opt + #2 first_nz_tuple

                    (*Calculate last_year and amount on last year*)
                    val last_year = valOf offset_int_opt + num_entries - 1
                    val amount_last_year = amount_on_LY(#year_freq(cur_name_record))
                    

                    (* Calculate non zero years*)
                    val non_zero = non_zero_years(#year_freq(cur_name_record))

                    (*calculate average*)
                    val avg = int_to_real(#sum_freq(cur_name_record))/int_to_real(num_entries)

                    (* construct string for selected name using values calculate above*)
                    val baby_string = cur_name^ "\n Total: "^ int_to_string(#sum_freq(cur_name_record))^"\n Years: "^int_to_string(non_zero) ^"\n "^int_to_string(last_year)^": "^int_to_string(amount_last_year) ^"\n First: "^int_to_string(first_nz_year)^" "^int_to_string(first_nz_val)^ "\n Last: "^int_to_string(last_nz_year)^" "^int_to_string(last_nz_val)^"\n Min: "^int_to_string(min_val_year)^" "^int_to_string(min_val)^"\n Max: "^int_to_string(max_val_year)^" "^int_to_string(max_val) ^ "\n Avg: "^real_to_string(avg)^"\n"^compute_names_output(tl names, selected_records, offsetSt)

                in
                    baby_string
                end
            



            else
                let
                    val baby_string = cur_name^"\nBaby name ["^cur_name^"] was not found\n" ^ compute_names_output(tl names, selected_records, offsetSt)

                in
                    baby_string
                end
        end







fun babies_program (babiesLines, NamesLines, offsetSt) =
(* the output of the program is the string returned by this function *)
    let 
        




        val babies: string list = split_at(babiesLines, #"\n")
        val names: string list = split_at(NamesLines, #"\n")
        (*create records for selected babies*)
        val selected_records = return_records(babies, names)
        (*return strings with calculated values for all selected babies*)
        val names_string = compute_names_output(names, selected_records, offsetSt)

        (*Construct start output string*)
        val entries_per_baby = int_to_string(str_list_len(split_at(hd babies, #","))-2)
        val start_output: string = "Read " ^ int_to_string(str_list_len(babies)) ^ " babies. Starting year " ^ offsetSt ^ ". Each baby has " ^ entries_per_baby ^" entries.\n"

        (*construct final output string*)
        val final_string = start_output^names_string

    in
        final_string
    end









end
end
