open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open List


module VariableSet = Set.Make(String) 

let paramtype = ref (Exp.construct {txt = Lident "SYSYF_Z"; loc = !default_loc} None) 

let desugar binds t = 
  Exp.apply (Exp.fun_ Nolabel None ((List.hd binds).pvb_pat) t) [(Nolabel, ((List.hd binds).pvb_expr))]

let rec expr_mapper mapper expr = 
  match expr with
  | { pexp_desc = Pexp_extension ({txt = tag; loc = tag_loc}, PStr [{pstr_desc = Pstr_eval (exp, attr)}]); 
      pexp_loc = ext_loc; 
      pexp_attributes = ext_attr } 
     -> if List.mem tag ["dfg"] then exp_translater exp (VariableSet.empty) 
        else if List.mem tag ["fuse"] then fuse_translater mapper exp 
        else default_mapper.expr mapper expr 
  | { pexp_desc = Pexp_extension ({txt = tag; loc = tag_loc}, _); 
      pexp_loc = ext_loc } 
    -> raise (Location.Error (Location.error ~loc:(tag_loc) "Only [%dfg ], [%fuse ] tags are recognised. ")) 

  | _ -> default_mapper.expr mapper expr 

and fuse_translater mapper exp = 
  match exp.pexp_desc with 
  | Pexp_let (Nonrecursive, [ {pvb_pat = pat; pvb_expr = arg; pvb_attributes = attr; pvb_loc = loc} ], t) 
      -> let arg = expr_mapper mapper arg in 
         let t = expr_mapper mapper t in 
         let e = Exp.apply ~loc:loc (Exp.ident {txt = Lident "_fusion"; loc = arg.pexp_loc}) [(Nolabel,arg);(Nolabel,!paramtype)] in 
         paramtype := (Exp.construct {txt = Lident "SYSYF_S"; loc = arg.pexp_loc} (Some !paramtype)); 
         Exp.let_ ~loc:(exp.pexp_loc) Nonrecursive [Vb.mk ~loc:loc pat e] t

and exp_translater exp vars = 
  match exp.pexp_desc with 
  | Pexp_ident id                       -> if VariableSet.mem (Longident.last id.txt) vars then Exp.apply ~loc:(exp.pexp_loc) (Exp.ident ~loc:(exp.pexp_loc) {txt = Lident "lift"; loc = exp.pexp_loc}) [(Nolabel, (exp))] else exp
  | Pexp_constant c                     -> Exp.apply ~loc:(exp.pexp_loc) (Exp.ident ~loc:(exp.pexp_loc) {txt = Lident "lift"; loc = exp.pexp_loc}) [(Nolabel, (Exp.constant ~loc:(exp.pexp_loc) c))]
  | Pexp_apply (f, args)                -> apply_translater f args vars 
  | Pexp_fun (_, _, pat, t)             -> let vars = addVar pat vars in 
                                           let g = exp_translater t vars in 
                                           let body = Exp.apply ~loc:(t.pexp_loc) (Exp.ident ~loc:(t.pexp_loc) {txt = Lident "peek"; loc = t.pexp_loc}) [(Nolabel, g)] in
                                           Exp.apply ~loc:(exp.pexp_loc) (Exp.ident ~loc:(exp.pexp_loc) {txt = Lident "lift"; loc = exp.pexp_loc}) [(Nolabel, (Exp.fun_ Nolabel None pat body))]
  | Pexp_let (rec_flag, binds, t)       -> exp_translater (desugar binds t) vars 
  | Pexp_ifthenelse (cond, t1, t2)      -> ifthenelse_translater (cond, t1, t2) exp.pexp_loc vars
  | Pexp_sequence (t1, t2)              -> Exp.sequence ~loc:(exp.pexp_loc) (exp_translater t1 vars) (exp_translater t2 vars)
  | Pexp_tuple ls                       -> Exp.tuple ~loc:(exp.pexp_loc) (List.map (fun e -> exp_translater e vars) ls)
  | _ -> exp

and addVar pat vars = 
  let {ppat_desc = desc} = pat in 
  match desc with 
  | Ppat_var id   -> VariableSet.add id.txt vars 
  | Ppat_alias (p, id) -> addVar p (VariableSet.add id.txt vars) 
  | Ppat_array ps | Ppat_tuple ps -> 
                      let rec aux ps vars = 
                       begin match ps with
                       | [] -> vars
                       | p :: ps -> aux ps (addVar p vars)
                       end 
                      in aux ps vars
  | Ppat_construct (id, Some p) -> addVar p vars
  | Ppat_variant (id, Some p) -> addVar p vars
  | Ppat_record (ps, f) -> let rec aux ps vars =
                           begin match ps with
                           | [] -> vars
                           | (id, p) :: ps -> aux ps (addVar p vars)
                           end
                          in aux ps vars
  | Ppat_or (p1, p2) -> addVar p2 (addVar p1 vars)
  | Ppat_constraint (p, t) -> addVar p vars
  | Ppat_lazy p -> addVar p vars
  | Ppat_exception p -> addVar p vars 
  | Ppat_type id -> vars 
  | _ -> vars 

and apply_translater f args vars = 
  match f.pexp_desc with   
  | Pexp_ident {txt = Lident "lift"} -> begin match args with
                                        | [arg] -> Exp.apply ~loc:(f.pexp_loc) (Exp.ident ~loc:(f.pexp_loc) {txt = Lident "lift"; loc = f.pexp_loc}) [arg]
                                        | _ -> raise (Location.Error (Location.error ~loc:(f.pexp_loc) "lift should have exactly 1 argument. ")) 
                                        end  
  | Pexp_ident {txt = Lident "lift_list"} -> begin match args with
                                        | [arg] -> Exp.apply ~loc:(f.pexp_loc) (Exp.ident ~loc:(f.pexp_loc) {txt = Lident "lift_list"; loc = f.pexp_loc}) [arg]
                                        | _ -> raise (Location.Error (Location.error ~loc:(f.pexp_loc) "lift_list should have exactly 1 argument. ")) 
                                        end 
  | Pexp_ident {txt = Lident "lift_tuple"} -> begin match args with
                                        | [arg] -> Exp.apply ~loc:(f.pexp_loc) (Exp.ident ~loc:(f.pexp_loc) {txt = Lident "lift_tuple"; loc = f.pexp_loc}) [arg]
                                        | _ -> raise (Location.Error (Location.error ~loc:(f.pexp_loc) "lift_tuple should have exactly 1 argument. ")) 
                                        end 
  | Pexp_ident {txt = Lident "pc"}   -> begin match args with
                                        | [arg] -> Exp.apply ~loc:(f.pexp_loc) (Exp.ident ~loc:(f.pexp_loc) {txt = Lident "pc"; loc = f.pexp_loc}) [arg] 
                                        | _ -> raise (Location.Error (Location.error ~loc:(f.pexp_loc) "pc should have exactly 1 argument. ")) 
                                        end   
  | _ -> let g = exp_translater f vars in 
         let rec fold_apply = function
            | [] -> raise (Location.Error (Location.error ~loc:(f.pexp_loc) "Function application cannot have no args. ")) 
            | [(l, u)] -> let k = exp_translater u vars in
                          Exp.apply ~loc:(f.pexp_loc) (Exp.ident ~loc:(f.pexp_loc) {txt = Lident "apply"; loc = f.pexp_loc}) [(Nolabel, g); (l,k)]
            | (l, u) :: xs -> 
                          let k = exp_translater u vars in
                          Exp.apply ~loc:(f.pexp_loc) (Exp.ident ~loc:(f.pexp_loc) {txt = Lident "apply"; loc = f.pexp_loc}) [(Nolabel, fold_apply xs); (l,k)]
         in
         fold_apply (rev_append args [])

and ifthenelse_translater (cond, t1, t2) loc vars =
  let g = (exp_translater cond vars) in
  let h = Exp.fun_ Nolabel None (Pat.any()) (exp_translater t1 vars) in 
  match t2 with
  | Some t2 -> let k = Exp.fun_ ~loc:(loc) Nolabel None (Pat.any()) (exp_translater t2 vars) in 
               Exp.apply ~loc:(loc) (Exp.ident ~loc:loc {txt = Lident "ifthenelse"; loc = loc}) [(Nolabel, g); (Nolabel, h); (Nolabel, k)]
  | None    -> Exp.apply ~loc:(loc) (Exp.ident ~loc:loc {txt = Lident "ifthenelse"; loc = loc}) [(Nolabel, g); (Nolabel, h)]







let sacml_mapper argv =
  { 
    default_mapper with
    expr = expr_mapper
  }
 
let () = register "sysyf_ext" sacml_mapper