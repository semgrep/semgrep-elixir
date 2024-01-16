(**
   Boilerplate to be used as a template when mapping the elixir CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_keyword_ (env : env) (tok : CST.keyword_) =
  (* keyword_ *) token env tok

let map_quoted_content_single (env : env) (tok : CST.quoted_content_single) =
  (* quoted_content_single *) token env tok

let map_not_in (env : env) (tok : CST.not_in) =
  (* not_in *) token env tok

let map_quoted_content_parenthesis (env : env) (tok : CST.quoted_content_parenthesis) =
  (* quoted_content_parenthesis *) token env tok

let map_quoted_content_square (env : env) (tok : CST.quoted_content_square) =
  (* quoted_content_square *) token env tok

let map_before_unary_op (env : env) (tok : CST.before_unary_op) =
  (* before_unary_op *) token env tok

let map_semgrep_metavariable (env : env) (tok : CST.semgrep_metavariable) =
  (* semgrep_metavariable *) token env tok

let map_imm_tok_pat_562b724 (env : env) (tok : CST.imm_tok_pat_562b724) =
  (* pattern [A-Z] *) token env tok

let map_imm_tok_pat_5eb9c21 (env : env) (tok : CST.imm_tok_pat_5eb9c21) =
  (* pattern :\s *) token env tok

let map_quoted_atom_start (env : env) (tok : CST.quoted_atom_start) =
  (* quoted_atom_start *) token env tok

let map_imm_tok_lpar (env : env) (tok : CST.imm_tok_lpar) =
  (* "(" *) token env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_quoted_content_i_parenthesis (env : env) (tok : CST.quoted_content_i_parenthesis) =
  (* quoted_content_i_parenthesis *) token env tok

let map_alias (env : env) (tok : CST.alias) =
  (* alias *) token env tok

let map_newline_before_do (env : env) (tok : CST.newline_before_do) =
  (* newline_before_do *) token env tok

let map_atom_ (env : env) (tok : CST.atom_) =
  (* atom_ *) token env tok

let map_imm_tok_pat_8f9e87e (env : env) (tok : CST.imm_tok_pat_8f9e87e) =
  (* pattern [a-zA-Z0-9]+ *) token env tok

let map_quoted_content_double (env : env) (tok : CST.quoted_content_double) =
  (* quoted_content_double *) token env tok

let map_imm_tok_lbrack (env : env) (tok : CST.imm_tok_lbrack) =
  (* "[" *) token env tok

let map_float_ (env : env) (tok : CST.float_) =
  (* float *) token env tok

let map_imm_tok_pat_0db2d54 (env : env) (tok : CST.imm_tok_pat_0db2d54) =
  (* pattern [a-z] *) token env tok

let map_quoted_content_slash (env : env) (tok : CST.quoted_content_slash) =
  (* quoted_content_slash *) token env tok

let map_integer (env : env) (tok : CST.integer) =
  (* integer *) token env tok

let map_quoted_content_i_bar (env : env) (tok : CST.quoted_content_i_bar) =
  (* quoted_content_i_bar *) token env tok

let map_char (env : env) (tok : CST.char) =
  (* pattern \?(.|\\.) *) token env tok

let map_pat_509ec78 (env : env) (tok : CST.pat_509ec78) =
  (* pattern \r?\n *) token env tok

let map_quoted_content_heredoc_single (env : env) (tok : CST.quoted_content_heredoc_single) =
  (* quoted_content_heredoc_single *) token env tok

let map_quoted_content_bar (env : env) (tok : CST.quoted_content_bar) =
  (* quoted_content_bar *) token env tok

let map_quoted_content_i_square (env : env) (tok : CST.quoted_content_i_square) =
  (* quoted_content_i_square *) token env tok

let map_pat_5eb9c21 (env : env) (tok : CST.pat_5eb9c21) =
  (* pattern :\s *) token env tok

let map_quoted_content_i_slash (env : env) (tok : CST.quoted_content_i_slash) =
  (* quoted_content_i_slash *) token env tok

let map_quoted_content_i_heredoc_single (env : env) (tok : CST.quoted_content_i_heredoc_single) =
  (* quoted_content_i_heredoc_single *) token env tok

let map_boolean (env : env) (x : CST.boolean) =
  (match x with
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  )

let map_quoted_content_angle (env : env) (tok : CST.quoted_content_angle) =
  (* quoted_content_angle *) token env tok

let map_quoted_content_heredoc_double (env : env) (tok : CST.quoted_content_heredoc_double) =
  (* quoted_content_heredoc_double *) token env tok

let map_quoted_content_i_curly (env : env) (tok : CST.quoted_content_i_curly) =
  (* quoted_content_i_curly *) token env tok

let map_quoted_content_i_double (env : env) (tok : CST.quoted_content_i_double) =
  (* quoted_content_i_double *) token env tok

let map_quoted_content_i_single (env : env) (tok : CST.quoted_content_i_single) =
  (* quoted_content_i_single *) token env tok

let map_quoted_content_i_angle (env : env) (tok : CST.quoted_content_i_angle) =
  (* quoted_content_i_angle *) token env tok

let map_quoted_content_curly (env : env) (tok : CST.quoted_content_curly) =
  (* quoted_content_curly *) token env tok

let map_pat_cf9c6c3 (env : env) (tok : CST.pat_cf9c6c3) =
  (* pattern [_\p{Ll}\p{Lm}\p{Lo}\p{Nl}\u1885\u1886\u2118\u212E\u309B\u309C][\p{ID_Continue}]*[?!]? *) token env tok

let map_quoted_content_i_heredoc_double (env : env) (tok : CST.quoted_content_i_heredoc_double) =
  (* quoted_content_i_heredoc_double *) token env tok

let map_anon_choice_PLUS_8019319 (env : env) (x : CST.anon_choice_PLUS_8019319) =
  (match x with
  | `PLUS tok -> R.Case ("PLUS",
      (* "+" *) token env tok
    )
  | `DASH tok -> R.Case ("DASH",
      (* "-" *) token env tok
    )
  | `BANG tok -> R.Case ("BANG",
      (* "!" *) token env tok
    )
  | `HAT tok -> R.Case ("HAT",
      (* "^" *) token env tok
    )
  | `TILDETILDETILDE tok -> R.Case ("TILDETILDETILDE",
      (* "~~~" *) token env tok
    )
  | `Not tok -> R.Case ("Not",
      (* "not" *) token env tok
    )
  )

let map_quoted_parenthesis (env : env) ((v1, v2, v3) : CST.quoted_parenthesis) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Quoted_content_paren tok -> R.Case ("Quoted_content_paren",
          (* quoted_content_parenthesis *) token env tok
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_quoted_square (env : env) ((v1, v2, v3) : CST.quoted_square) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Quoted_content_square tok -> R.Case ("Quoted_content_square",
          (* quoted_content_square *) token env tok
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_quoted_single (env : env) ((v1, v2, v3) : CST.quoted_single) =
  let v1 = (* "'" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Quoted_content_single tok -> R.Case ("Quoted_content_single",
          (* quoted_content_single *) token env tok
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "'" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_quoted_double (env : env) ((v1, v2, v3) : CST.quoted_double) =
  let v1 = (* "\"" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Quoted_content_double tok -> R.Case ("Quoted_content_double",
          (* quoted_content_double *) token env tok
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "\"" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_quoted_slash (env : env) ((v1, v2, v3) : CST.quoted_slash) =
  let v1 = (* "/" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Quoted_content_slash tok -> R.Case ("Quoted_content_slash",
          (* quoted_content_slash *) token env tok
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "/" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_terminator (env : env) (x : CST.terminator) =
  (match x with
  | `Rep_pat_509ec78_SEMI (v1, v2) -> R.Case ("Rep_pat_509ec78_SEMI",
      let v1 =
        R.List (List.map (fun x ->
          map_pat_509ec78 env x
        ) v1)
      in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Rep1_pat_509ec78 xs -> R.Case ("Rep1_pat_509ec78",
      R.List (List.map (fun x ->
        map_pat_509ec78 env x
      ) xs)
    )
  )

let map_quoted_heredoc_single (env : env) ((v1, v2, v3) : CST.quoted_heredoc_single) =
  let v1 = (* "'''" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Quoted_content_here_single tok -> R.Case ("Quoted_content_here_single",
          (* quoted_content_heredoc_single *) token env tok
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "'''" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_quoted_bar (env : env) ((v1, v2, v3) : CST.quoted_bar) =
  let v1 = (* "|" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Quoted_content_bar tok -> R.Case ("Quoted_content_bar",
          (* quoted_content_bar *) token env tok
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "|" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_quoted_angle (env : env) ((v1, v2, v3) : CST.quoted_angle) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Quoted_content_angle tok -> R.Case ("Quoted_content_angle",
          (* quoted_content_angle *) token env tok
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* ">" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_quoted_heredoc_double (env : env) ((v1, v2, v3) : CST.quoted_heredoc_double) =
  let v1 = (* "\"\"\"" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Quoted_content_here_double tok -> R.Case ("Quoted_content_here_double",
          (* quoted_content_heredoc_double *) token env tok
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "\"\"\"" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_quoted_curly (env : env) ((v1, v2, v3) : CST.quoted_curly) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Quoted_content_curl tok -> R.Case ("Quoted_content_curl",
          (* quoted_content_curly *) token env tok
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_identifier (env : env) (x : CST.identifier) =
  (match x with
  | `Choice_pat_cf9c6c3 x -> R.Case ("Choice_pat_cf9c6c3",
      (match x with
      | `Pat_cf9c6c3 x -> R.Case ("Pat_cf9c6c3",
          map_pat_cf9c6c3 env x
        )
      | `DOTDOTDOT tok -> R.Case ("DOTDOTDOT",
          (* "..." *) token env tok
        )
      )
    )
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  )

let map_operator_identifier (env : env) (x : CST.operator_identifier) =
  (match x with
  | `AMP tok -> R.Case ("AMP",
      (* "&" *) token env tok
    )
  | `Choice_PLUS x -> R.Case ("Choice_PLUS",
      map_anon_choice_PLUS_8019319 env x
    )
  | `AT tok -> R.Case ("AT",
      (* "@" *) token env tok
    )
  | `LTDASH tok -> R.Case ("LTDASH",
      (* "<-" *) token env tok
    )
  | `BSLASHBSLASH tok -> R.Case ("BSLASHBSLASH",
      (* "\\\\" *) token env tok
    )
  | `When tok -> R.Case ("When",
      (* "when" *) token env tok
    )
  | `COLONCOLON tok -> R.Case ("COLONCOLON",
      (* "::" *) token env tok
    )
  | `BAR tok -> R.Case ("BAR",
      (* "|" *) token env tok
    )
  | `EQ tok -> R.Case ("EQ",
      (* "=" *) token env tok
    )
  | `BARBAR tok -> R.Case ("BARBAR",
      (* "||" *) token env tok
    )
  | `BARBARBAR tok -> R.Case ("BARBARBAR",
      (* "|||" *) token env tok
    )
  | `Or tok -> R.Case ("Or",
      (* "or" *) token env tok
    )
  | `AMPAMP tok -> R.Case ("AMPAMP",
      (* "&&" *) token env tok
    )
  | `AMPAMPAMP tok -> R.Case ("AMPAMPAMP",
      (* "&&&" *) token env tok
    )
  | `And tok -> R.Case ("And",
      (* "and" *) token env tok
    )
  | `EQEQ tok -> R.Case ("EQEQ",
      (* "==" *) token env tok
    )
  | `BANGEQ tok -> R.Case ("BANGEQ",
      (* "!=" *) token env tok
    )
  | `EQTILDE tok -> R.Case ("EQTILDE",
      (* "=~" *) token env tok
    )
  | `EQEQEQ tok -> R.Case ("EQEQEQ",
      (* "===" *) token env tok
    )
  | `BANGEQEQ tok -> R.Case ("BANGEQEQ",
      (* "!==" *) token env tok
    )
  | `LT tok -> R.Case ("LT",
      (* "<" *) token env tok
    )
  | `GT tok -> R.Case ("GT",
      (* ">" *) token env tok
    )
  | `LTEQ tok -> R.Case ("LTEQ",
      (* "<=" *) token env tok
    )
  | `GTEQ tok -> R.Case ("GTEQ",
      (* ">=" *) token env tok
    )
  | `BARGT tok -> R.Case ("BARGT",
      (* "|>" *) token env tok
    )
  | `LTLTLT tok -> R.Case ("LTLTLT",
      (* "<<<" *) token env tok
    )
  | `GTGTGT tok -> R.Case ("GTGTGT",
      (* ">>>" *) token env tok
    )
  | `LTLTTILDE tok -> R.Case ("LTLTTILDE",
      (* "<<~" *) token env tok
    )
  | `TILDEGTGT tok -> R.Case ("TILDEGTGT",
      (* "~>>" *) token env tok
    )
  | `LTTILDE tok -> R.Case ("LTTILDE",
      (* "<~" *) token env tok
    )
  | `TILDEGT tok -> R.Case ("TILDEGT",
      (* "~>" *) token env tok
    )
  | `LTTILDEGT tok -> R.Case ("LTTILDEGT",
      (* "<~>" *) token env tok
    )
  | `LTBARGT tok -> R.Case ("LTBARGT",
      (* "<|>" *) token env tok
    )
  | `In tok -> R.Case ("In",
      (* "in" *) token env tok
    )
  | `Not_in tok -> R.Case ("Not_in",
      (* not_in *) token env tok
    )
  | `HATHAT tok -> R.Case ("HATHAT",
      (* "^^" *) token env tok
    )
  | `PLUSPLUS tok -> R.Case ("PLUSPLUS",
      (* "++" *) token env tok
    )
  | `DASHDASH tok -> R.Case ("DASHDASH",
      (* "--" *) token env tok
    )
  | `PLUSPLUSPLUS tok -> R.Case ("PLUSPLUSPLUS",
      (* "+++" *) token env tok
    )
  | `DASHDASHDASH tok -> R.Case ("DASHDASHDASH",
      (* "---" *) token env tok
    )
  | `DOTDOT tok -> R.Case ("DOTDOT",
      (* ".." *) token env tok
    )
  | `LTGT tok -> R.Case ("LTGT",
      (* "<>" *) token env tok
    )
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  | `SLASH tok -> R.Case ("SLASH",
      (* "/" *) token env tok
    )
  | `STARSTAR tok -> R.Case ("STARSTAR",
      (* "**" *) token env tok
    )
  | `DASHGT tok -> R.Case ("DASHGT",
      (* "->" *) token env tok
    )
  | `DOT tok -> R.Case ("DOT",
      (* "." *) token env tok
    )
  )

let rec map_after_block (env : env) ((v1, v2, v3) : CST.after_block) =
  let v1 = (* "after" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119 env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119 (env : env) (x : CST.anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119) =
  (match x with
  | `Choice_stab_clause_rep_term_choice_stab_clause (v1, v2) -> R.Case ("Choice_stab_clause_rep_term_choice_stab_clause",
      let v1 =
        (match v1 with
        | `Stab_clause x -> R.Case ("Stab_clause",
            map_stab_clause env x
          )
        )
      in
      let v2 =
        R.List (List.map (map_anon_term_choice_stab_clause_70647b7 env) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Choice_exp_rep_term_choice_exp_opt_term (v1, v2, v3) -> R.Case ("Choice_exp_rep_term_choice_exp_opt_term",
      let v1 =
        (match v1 with
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        )
      in
      let v2 =
        R.List (List.map (map_anon_term_choice_exp_996111b env) v2)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_exp_0094635 (env : env) (x : CST.anon_choice_exp_0094635) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Keywos x -> R.Case ("Keywos",
      map_keywords env x
    )
  )

and map_anon_choice_quoted_i_double_d7d5f65 (env : env) (x : CST.anon_choice_quoted_i_double_d7d5f65) =
  (match x with
  | `Quoted_i_double x -> R.Case ("Quoted_i_double",
      map_quoted_i_double env x
    )
  | `Quoted_i_single x -> R.Case ("Quoted_i_single",
      map_quoted_i_single env x
    )
  )

and map_anon_exp_rep_COMMA_exp_opt_COMMA_keywos_041d82e (env : env) ((v1, v2, v3) : CST.anon_exp_rep_COMMA_exp_opt_COMMA_keywos_041d82e) =
  let v1 = map_expression env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "," *) token env v1 in
        let v2 = map_keywords env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_anon_opt_opt_nl_before_do_do_blk_3eff85f (env : env) (opt : CST.anon_opt_opt_nl_before_do_do_blk_3eff85f) =
  (match opt with
  | Some (v1, v2) -> R.Option (Some (
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* newline_before_do *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = map_do_block env v2 in
      R.Tuple [v1; v2]
    ))
  | None -> R.Option None)

and map_anon_term_choice_exp_996111b (env : env) ((v1, v2) : CST.anon_term_choice_exp_996111b) =
  let v1 = map_terminator env v1 in
  let v2 =
    (match v2 with
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_anon_term_choice_stab_clause_70647b7 (env : env) ((v1, v2) : CST.anon_term_choice_stab_clause_70647b7) =
  let v1 = map_terminator env v1 in
  let v2 =
    (match v2 with
    | `Stab_clause x -> R.Case ("Stab_clause",
        map_stab_clause env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_anonymous_call (env : env) ((v1, v2) : CST.anonymous_call) =
  let v1 = map_anonymous_dot env v1 in
  let v2 = map_call_arguments_with_parentheses env v2 in
  R.Tuple [v1; v2]

and map_anonymous_dot (env : env) ((v1, v2) : CST.anonymous_dot) =
  let v1 = map_expression env v1 in
  let v2 = (* "." *) token env v2 in
  R.Tuple [v1; v2]

and map_atom (env : env) (x : CST.atom) =
  (match x with
  | `Atom_ tok -> R.Case ("Atom_",
      (* atom_ *) token env tok
    )
  | `Quoted_atom (v1, v2) -> R.Case ("Quoted_atom",
      let v1 = (* quoted_atom_start *) token env v1 in
      let v2 = map_anon_choice_quoted_i_double_d7d5f65 env v2 in
      R.Tuple [v1; v2]
    )
  | `Meta_atom (v1, v2) -> R.Case ("Meta_atom",
      let v1 = (* ":" *) token env v1 in
      let v2 = (* semgrep_metavariable *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_binary_operator (env : env) (x : CST.binary_operator) =
  (match x with
  | `Exp_choice_LTDASH_exp (v1, v2, v3) -> R.Case ("Exp_choice_LTDASH_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `LTDASH tok -> R.Case ("LTDASH",
            (* "<-" *) token env tok
          )
        | `BSLASHBSLASH tok -> R.Case ("BSLASHBSLASH",
            (* "\\\\" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_when_choice_exp (v1, v2, v3) -> R.Case ("Exp_when_choice_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "when" *) token env v2 in
      let v3 = map_anon_choice_exp_0094635 env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_COLONCOLON_exp (v1, v2, v3) -> R.Case ("Exp_COLONCOLON_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BAR_choice_exp (v1, v2, v3) -> R.Case ("Exp_BAR_choice_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_anon_choice_exp_0094635 env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_EQGT_exp (v1, v2, v3) -> R.Case ("Exp_EQGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_EQ_exp (v1, v2, v3) -> R.Case ("Exp_EQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_choice_BARBAR_exp (v1, v2, v3) -> R.Case ("Exp_choice_BARBAR_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `BARBAR tok -> R.Case ("BARBAR",
            (* "||" *) token env tok
          )
        | `BARBARBAR tok -> R.Case ("BARBARBAR",
            (* "|||" *) token env tok
          )
        | `Or tok -> R.Case ("Or",
            (* "or" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_choice_AMPAMP_exp (v1, v2, v3) -> R.Case ("Exp_choice_AMPAMP_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `AMPAMP tok -> R.Case ("AMPAMP",
            (* "&&" *) token env tok
          )
        | `AMPAMPAMP tok -> R.Case ("AMPAMPAMP",
            (* "&&&" *) token env tok
          )
        | `And tok -> R.Case ("And",
            (* "and" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_choice_EQEQ_exp (v1, v2, v3) -> R.Case ("Exp_choice_EQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `EQEQ tok -> R.Case ("EQEQ",
            (* "==" *) token env tok
          )
        | `BANGEQ tok -> R.Case ("BANGEQ",
            (* "!=" *) token env tok
          )
        | `EQTILDE tok -> R.Case ("EQTILDE",
            (* "=~" *) token env tok
          )
        | `EQEQEQ tok -> R.Case ("EQEQEQ",
            (* "===" *) token env tok
          )
        | `BANGEQEQ tok -> R.Case ("BANGEQEQ",
            (* "!==" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_choice_LT_exp (v1, v2, v3) -> R.Case ("Exp_choice_LT_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `LT tok -> R.Case ("LT",
            (* "<" *) token env tok
          )
        | `GT tok -> R.Case ("GT",
            (* ">" *) token env tok
          )
        | `LTEQ tok -> R.Case ("LTEQ",
            (* "<=" *) token env tok
          )
        | `GTEQ tok -> R.Case ("GTEQ",
            (* ">=" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_choice_BARGT_exp (v1, v2, v3) -> R.Case ("Exp_choice_BARGT_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `BARGT tok -> R.Case ("BARGT",
            (* "|>" *) token env tok
          )
        | `LTLTLT tok -> R.Case ("LTLTLT",
            (* "<<<" *) token env tok
          )
        | `GTGTGT tok -> R.Case ("GTGTGT",
            (* ">>>" *) token env tok
          )
        | `LTLTTILDE tok -> R.Case ("LTLTTILDE",
            (* "<<~" *) token env tok
          )
        | `TILDEGTGT tok -> R.Case ("TILDEGTGT",
            (* "~>>" *) token env tok
          )
        | `LTTILDE tok -> R.Case ("LTTILDE",
            (* "<~" *) token env tok
          )
        | `TILDEGT tok -> R.Case ("TILDEGT",
            (* "~>" *) token env tok
          )
        | `LTTILDEGT tok -> R.Case ("LTTILDEGT",
            (* "<~>" *) token env tok
          )
        | `LTBARGT tok -> R.Case ("LTBARGT",
            (* "<|>" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_choice_in_exp (v1, v2, v3) -> R.Case ("Exp_choice_in_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `In tok -> R.Case ("In",
            (* "in" *) token env tok
          )
        | `Not_in tok -> R.Case ("Not_in",
            (* not_in *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_HATHATHAT_exp (v1, v2, v3) -> R.Case ("Exp_HATHATHAT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "^^^" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_SLASHSLASH_exp (v1, v2, v3) -> R.Case ("Exp_SLASHSLASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "//" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_choice_PLUSPLUS_exp (v1, v2, v3) -> R.Case ("Exp_choice_PLUSPLUS_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `PLUSPLUS tok -> R.Case ("PLUSPLUS",
            (* "++" *) token env tok
          )
        | `DASHDASH tok -> R.Case ("DASHDASH",
            (* "--" *) token env tok
          )
        | `PLUSPLUSPLUS tok -> R.Case ("PLUSPLUSPLUS",
            (* "+++" *) token env tok
          )
        | `DASHDASHDASH tok -> R.Case ("DASHDASHDASH",
            (* "---" *) token env tok
          )
        | `DOTDOT tok -> R.Case ("DOTDOT",
            (* ".." *) token env tok
          )
        | `LTGT tok -> R.Case ("LTGT",
            (* "<>" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_choice_PLUS_exp (v1, v2, v3) -> R.Case ("Exp_choice_PLUS_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `PLUS tok -> R.Case ("PLUS",
            (* "+" *) token env tok
          )
        | `DASH tok -> R.Case ("DASH",
            (* "-" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_choice_STAR_exp (v1, v2, v3) -> R.Case ("Exp_choice_STAR_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `STAR tok -> R.Case ("STAR",
            (* "*" *) token env tok
          )
        | `SLASH tok -> R.Case ("SLASH",
            (* "/" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_STARSTAR_exp (v1, v2, v3) -> R.Case ("Exp_STARSTAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "**" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_id_SLASH_int (v1, v2, v3) -> R.Case ("Op_id_SLASH_int",
      let v1 = map_operator_identifier env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = (* integer *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_body (env : env) ((v1, v2, v3, v4) : CST.body) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_expression env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_terminator env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_call (env : env) (x : CST.call) =
  (match x with
  | `Call_with_parens_b98484c x -> R.Case ("Call_with_parens_b98484c",
      map_call_without_parentheses env x
    )
  | `Call_with_parens_403315d x -> R.Case ("Call_with_parens_403315d",
      map_call_with_parentheses env x
    )
  )

and map_call_arguments_with_parentheses (env : env) ((v1, v2, v3) : CST.call_arguments_with_parentheses) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_call_arguments_with_trailing_separator env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_call_arguments_with_parentheses_immediate (env : env) ((v1, v2, v3) : CST.call_arguments_with_parentheses_immediate) =
  let v1 = map_imm_tok_lpar env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_call_arguments_with_trailing_separator env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_call_arguments_with_trailing_separator (env : env) (x : CST.call_arguments_with_trailing_separator) =
  (match x with
  | `Exp_rep_COMMA_exp_opt_COMMA_keywos_with_trai_sepa (v1, v2, v3) -> R.Case ("Exp_rep_COMMA_exp_opt_COMMA_keywos_with_trai_sepa",
      let v1 = map_expression env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_expression env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "," *) token env v1 in
            let v2 = map_keywords_with_trailing_separator env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Keywos_with_trai_sepa x -> R.Case ("Keywos_with_trai_sepa",
      map_keywords_with_trailing_separator env x
    )
  )

and map_call_arguments_without_parentheses (env : env) (x : CST.call_arguments_without_parentheses) =
  (match x with
  | `Exp_rep_COMMA_exp_opt_COMMA_keywos x -> R.Case ("Exp_rep_COMMA_exp_opt_COMMA_keywos",
      map_anon_exp_rep_COMMA_exp_opt_COMMA_keywos_041d82e env x
    )
  | `Keywos x -> R.Case ("Keywos",
      map_keywords env x
    )
  )

and map_call_with_parentheses (env : env) (x : CST.call_with_parentheses) =
  (match x with
  | `Local_call_with_parens x -> R.Case ("Local_call_with_parens",
      map_local_call_with_parentheses env x
    )
  | `Remote_call_with_parens x -> R.Case ("Remote_call_with_parens",
      map_remote_call_with_parentheses env x
    )
  | `Anon_call x -> R.Case ("Anon_call",
      map_anonymous_call env x
    )
  | `Double_call (v1, v2, v3) -> R.Case ("Double_call",
      let v1 =
        (match v1 with
        | `Local_call_with_parens x -> R.Case ("Local_call_with_parens",
            map_local_call_with_parentheses env x
          )
        | `Remote_call_with_parens x -> R.Case ("Remote_call_with_parens",
            map_remote_call_with_parentheses env x
          )
        | `Anon_call x -> R.Case ("Anon_call",
            map_anonymous_call env x
          )
        )
      in
      let v2 = map_call_arguments_with_parentheses env v2 in
      let v3 =
        map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_call_without_parentheses (env : env) (x : CST.call_without_parentheses) =
  (match x with
  | `Local_call_with_parens (v1, v2, v3) -> R.Case ("Local_call_with_parens",
      let v1 = map_identifier env v1 in
      let v2 = map_call_arguments_without_parentheses env v2 in
      let v3 =
        map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3
      in
      R.Tuple [v1; v2; v3]
    )
  | `Local_call_just_do_blk (v1, v2) -> R.Case ("Local_call_just_do_blk",
      let v1 = map_identifier env v1 in
      let v2 = map_do_block env v2 in
      R.Tuple [v1; v2]
    )
  | `Remote_call_with_parens (v1, v2, v3) -> R.Case ("Remote_call_with_parens",
      let v1 = map_remote_dot env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_call_arguments_without_parentheses env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_capture_expression (env : env) (x : CST.capture_expression) =
  (match x with
  | `LPAR_exp_RPAR (v1, v2, v3) -> R.Case ("LPAR_exp_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  )

and map_catch_block (env : env) ((v1, v2, v3) : CST.catch_block) =
  let v1 = (* "catch" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119 env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_charlist (env : env) (x : CST.charlist) =
  (match x with
  | `Quoted_i_single x -> R.Case ("Quoted_i_single",
      map_quoted_i_single env x
    )
  | `Quoted_i_here_single x -> R.Case ("Quoted_i_here_single",
      map_quoted_i_heredoc_single env x
    )
  )

and map_do_block (env : env) ((v1, v2, v3, v4, v5) : CST.do_block) =
  let v1 = (* "do" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119 env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    R.List (List.map (fun x ->
      (match x with
      | `After_blk x -> R.Case ("After_blk",
          map_after_block env x
        )
      | `Rescue_blk x -> R.Case ("Rescue_blk",
          map_rescue_block env x
        )
      | `Catch_blk x -> R.Case ("Catch_blk",
          map_catch_block env x
        )
      | `Else_blk x -> R.Case ("Else_blk",
          map_else_block env x
        )
      )
    ) v4)
  in
  let v5 = (* "end" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_dot (env : env) ((v1, v2, v3) : CST.dot) =
  let v1 = map_expression env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 =
    (match v3 with
    | `Alias tok -> R.Case ("Alias",
        (* alias *) token env tok
      )
    | `Tuple x -> R.Case ("Tuple",
        map_tuple env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_else_block (env : env) ((v1, v2, v3) : CST.else_block) =
  let v1 = (* "else" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119 env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Blk (v1, v2, v3, v4) -> R.Case ("Blk",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119 env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Id x -> R.Case ("Id",
      map_identifier env x
    )
  | `Alias tok -> R.Case ("Alias",
      (* alias *) token env tok
    )
  | `Int tok -> R.Case ("Int",
      (* integer *) token env tok
    )
  | `Float tok -> R.Case ("Float",
      (* float *) token env tok
    )
  | `Char_a87deb0 tok -> R.Case ("Char_a87deb0",
      (* pattern \?(.|\\.) *) token env tok
    )
  | `Bool x -> R.Case ("Bool",
      map_boolean env x
    )
  | `Nil tok -> R.Case ("Nil",
      (* "nil" *) token env tok
    )
  | `Atom x -> R.Case ("Atom",
      map_atom env x
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Char_a593f90 x -> R.Case ("Char_a593f90",
      map_charlist env x
    )
  | `Sigil (v1, v2, v3) -> R.Case ("Sigil",
      let v1 = (* "~" *) token env v1 in
      let v2 =
        (match v2 with
        | `Imm_tok_pat_0db2d54_choice_quoted_i_double (v1, v2) -> R.Case ("Imm_tok_pat_0db2d54_choice_quoted_i_double",
            let v1 = map_imm_tok_pat_0db2d54 env v1 in
            let v2 =
              (match v2 with
              | `Quoted_i_double x -> R.Case ("Quoted_i_double",
                  map_quoted_i_double env x
                )
              | `Quoted_i_single x -> R.Case ("Quoted_i_single",
                  map_quoted_i_single env x
                )
              | `Quoted_i_here_single x -> R.Case ("Quoted_i_here_single",
                  map_quoted_i_heredoc_single env x
                )
              | `Quoted_i_here_double x -> R.Case ("Quoted_i_here_double",
                  map_quoted_i_heredoc_double env x
                )
              | `Quoted_i_paren x -> R.Case ("Quoted_i_paren",
                  map_quoted_i_parenthesis env x
                )
              | `Quoted_i_curl x -> R.Case ("Quoted_i_curl",
                  map_quoted_i_curly env x
                )
              | `Quoted_i_square x -> R.Case ("Quoted_i_square",
                  map_quoted_i_square env x
                )
              | `Quoted_i_angle x -> R.Case ("Quoted_i_angle",
                  map_quoted_i_angle env x
                )
              | `Quoted_i_bar x -> R.Case ("Quoted_i_bar",
                  map_quoted_i_bar env x
                )
              | `Quoted_i_slash x -> R.Case ("Quoted_i_slash",
                  map_quoted_i_slash env x
                )
              )
            in
            R.Tuple [v1; v2]
          )
        | `Imm_tok_pat_562b724_choice_quoted_double (v1, v2) -> R.Case ("Imm_tok_pat_562b724_choice_quoted_double",
            let v1 = map_imm_tok_pat_562b724 env v1 in
            let v2 =
              (match v2 with
              | `Quoted_double x -> R.Case ("Quoted_double",
                  map_quoted_double env x
                )
              | `Quoted_single x -> R.Case ("Quoted_single",
                  map_quoted_single env x
                )
              | `Quoted_here_single x -> R.Case ("Quoted_here_single",
                  map_quoted_heredoc_single env x
                )
              | `Quoted_here_double x -> R.Case ("Quoted_here_double",
                  map_quoted_heredoc_double env x
                )
              | `Quoted_paren x -> R.Case ("Quoted_paren",
                  map_quoted_parenthesis env x
                )
              | `Quoted_curl x -> R.Case ("Quoted_curl",
                  map_quoted_curly env x
                )
              | `Quoted_square x -> R.Case ("Quoted_square",
                  map_quoted_square env x
                )
              | `Quoted_angle x -> R.Case ("Quoted_angle",
                  map_quoted_angle env x
                )
              | `Quoted_bar x -> R.Case ("Quoted_bar",
                  map_quoted_bar env x
                )
              | `Quoted_slash x -> R.Case ("Quoted_slash",
                  map_quoted_slash env x
                )
              )
            in
            R.Tuple [v1; v2]
          )
        )
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_imm_tok_pat_8f9e87e env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `List (v1, v2, v3) -> R.Case ("List",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_items_with_trailing_separator env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Tuple x -> R.Case ("Tuple",
      map_tuple env x
    )
  | `Bits (v1, v2, v3) -> R.Case ("Bits",
      let v1 = (* "<<" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_items_with_trailing_separator env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* ">>" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Map (v1, v2, v3, v4, v5) -> R.Case ("Map",
      let v1 = (* "%" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_struct_ env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "{" *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_items_with_trailing_separator env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* "}" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Un_op x -> R.Case ("Un_op",
      map_unary_operator env x
    )
  | `Bin_op x -> R.Case ("Bin_op",
      map_binary_operator env x
    )
  | `Dot x -> R.Case ("Dot",
      map_dot env x
    )
  | `Call x -> R.Case ("Call",
      map_call env x
    )
  | `Access_call (v1, v2, v3, v4) -> R.Case ("Access_call",
      let v1 = map_expression env v1 in
      let v2 = map_imm_tok_lbrack env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Anon_func (v1, v2, v3, v4, v5) -> R.Case ("Anon_func",
      let v1 = (* "fn" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_stab_clause env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_terminator env v1 in
          let v2 = map_stab_clause env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 = (* "end" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Deep_ellips (v1, v2, v3) -> R.Case ("Deep_ellips",
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "...>" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_interpolation (env : env) ((v1, v2, v3) : CST.interpolation) =
  let v1 = (* "#{" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_items_with_trailing_separator (env : env) (v1 : CST.items_with_trailing_separator) =
  (match v1 with
  | `Exp_rep_COMMA_exp_opt_COMMA (v1, v2, v3) -> R.Case ("Exp_rep_COMMA_exp_opt_COMMA",
      let v1 = map_expression env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_expression env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Opt_exp_rep_COMMA_exp_COMMA_keywos_with_trai_sepa (v1, v2) -> R.Case ("Opt_exp_rep_COMMA_exp_COMMA_keywos_with_trai_sepa",
      let v1 =
        (match v1 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = map_expression env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_expression env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            let v3 = (* "," *) token env v3 in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      let v2 = map_keywords_with_trailing_separator env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_keyword (env : env) (x : CST.keyword) =
  (match x with
  | `Kw_ tok -> R.Case ("Kw_",
      (* keyword_ *) token env tok
    )
  | `Quoted_kw (v1, v2) -> R.Case ("Quoted_kw",
      let v1 = map_anon_choice_quoted_i_double_d7d5f65 env v1 in
      let v2 = map_imm_tok_pat_5eb9c21 env v2 in
      R.Tuple [v1; v2]
    )
  | `Meta_kw (v1, v2) -> R.Case ("Meta_kw",
      let v1 = (* semgrep_metavariable *) token env v1 in
      let v2 = map_pat_5eb9c21 env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_keywords (env : env) ((v1, v2) : CST.keywords) =
  let v1 = map_pair env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_pair env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_keywords_with_trailing_separator (env : env) ((v1, v2, v3) : CST.keywords_with_trailing_separator) =
  let v1 = map_pair env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_pair env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_local_call_with_parentheses (env : env) ((v1, v2, v3) : CST.local_call_with_parentheses) =
  let v1 = map_identifier env v1 in
  let v2 =
    map_call_arguments_with_parentheses_immediate env v2
  in
  let v3 =
    map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3
  in
  R.Tuple [v1; v2; v3]

and map_pair (env : env) (x : CST.pair) =
  (match x with
  | `Kw_exp (v1, v2) -> R.Case ("Kw_exp",
      let v1 = map_keyword env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

and map_quoted_i_angle (env : env) ((v1, v2, v3) : CST.quoted_i_angle) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Quoted_content_i_angle tok -> R.Case ("Quoted_content_i_angle",
          (* quoted_content_i_angle *) token env tok
        )
      | `Interp x -> R.Case ("Interp",
          map_interpolation env x
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* ">" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_quoted_i_bar (env : env) ((v1, v2, v3) : CST.quoted_i_bar) =
  let v1 = (* "|" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Quoted_content_i_bar tok -> R.Case ("Quoted_content_i_bar",
          (* quoted_content_i_bar *) token env tok
        )
      | `Interp x -> R.Case ("Interp",
          map_interpolation env x
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "|" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_quoted_i_curly (env : env) ((v1, v2, v3) : CST.quoted_i_curly) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Quoted_content_i_curl tok -> R.Case ("Quoted_content_i_curl",
          (* quoted_content_i_curly *) token env tok
        )
      | `Interp x -> R.Case ("Interp",
          map_interpolation env x
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_quoted_i_double (env : env) ((v1, v2, v3) : CST.quoted_i_double) =
  let v1 = (* "\"" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Quoted_content_i_double tok -> R.Case ("Quoted_content_i_double",
          (* quoted_content_i_double *) token env tok
        )
      | `Interp x -> R.Case ("Interp",
          map_interpolation env x
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "\"" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_quoted_i_heredoc_double (env : env) ((v1, v2, v3) : CST.quoted_i_heredoc_double) =
  let v1 = (* "\"\"\"" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Quoted_content_i_here_double tok -> R.Case ("Quoted_content_i_here_double",
          (* quoted_content_i_heredoc_double *) token env tok
        )
      | `Interp x -> R.Case ("Interp",
          map_interpolation env x
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "\"\"\"" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_quoted_i_heredoc_single (env : env) ((v1, v2, v3) : CST.quoted_i_heredoc_single) =
  let v1 = (* "'''" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Quoted_content_i_here_single tok -> R.Case ("Quoted_content_i_here_single",
          (* quoted_content_i_heredoc_single *) token env tok
        )
      | `Interp x -> R.Case ("Interp",
          map_interpolation env x
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "'''" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_quoted_i_parenthesis (env : env) ((v1, v2, v3) : CST.quoted_i_parenthesis) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Quoted_content_i_paren tok -> R.Case ("Quoted_content_i_paren",
          (* quoted_content_i_parenthesis *) token env tok
        )
      | `Interp x -> R.Case ("Interp",
          map_interpolation env x
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_quoted_i_single (env : env) ((v1, v2, v3) : CST.quoted_i_single) =
  let v1 = (* "'" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Quoted_content_i_single tok -> R.Case ("Quoted_content_i_single",
          (* quoted_content_i_single *) token env tok
        )
      | `Interp x -> R.Case ("Interp",
          map_interpolation env x
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "'" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_quoted_i_slash (env : env) ((v1, v2, v3) : CST.quoted_i_slash) =
  let v1 = (* "/" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Quoted_content_i_slash tok -> R.Case ("Quoted_content_i_slash",
          (* quoted_content_i_slash *) token env tok
        )
      | `Interp x -> R.Case ("Interp",
          map_interpolation env x
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "/" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_quoted_i_square (env : env) ((v1, v2, v3) : CST.quoted_i_square) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Quoted_content_i_square tok -> R.Case ("Quoted_content_i_square",
          (* quoted_content_i_square *) token env tok
        )
      | `Interp x -> R.Case ("Interp",
          map_interpolation env x
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_remote_call_with_parentheses (env : env) ((v1, v2, v3) : CST.remote_call_with_parentheses) =
  let v1 = map_remote_dot env v1 in
  let v2 =
    map_call_arguments_with_parentheses_immediate env v2
  in
  let v3 =
    map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3
  in
  R.Tuple [v1; v2; v3]

and map_remote_dot (env : env) ((v1, v2, v3) : CST.remote_dot) =
  let v1 = map_expression env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 =
    (match v3 with
    | `Id x -> R.Case ("Id",
        map_identifier env x
      )
    | `Choice_and x -> R.Case ("Choice_and",
        (match x with
        | `And tok -> R.Case ("And",
            (* "and" *) token env tok
          )
        | `In tok -> R.Case ("In",
            (* "in" *) token env tok
          )
        | `Not tok -> R.Case ("Not",
            (* "not" *) token env tok
          )
        | `Or tok -> R.Case ("Or",
            (* "or" *) token env tok
          )
        | `When tok -> R.Case ("When",
            (* "when" *) token env tok
          )
        | `True tok -> R.Case ("True",
            (* "true" *) token env tok
          )
        | `False tok -> R.Case ("False",
            (* "false" *) token env tok
          )
        | `Nil tok -> R.Case ("Nil",
            (* "nil" *) token env tok
          )
        | `After tok -> R.Case ("After",
            (* "after" *) token env tok
          )
        | `Catch tok -> R.Case ("Catch",
            (* "catch" *) token env tok
          )
        | `Do tok -> R.Case ("Do",
            (* "do" *) token env tok
          )
        | `Else tok -> R.Case ("Else",
            (* "else" *) token env tok
          )
        | `End tok -> R.Case ("End",
            (* "end" *) token env tok
          )
        | `Fn tok -> R.Case ("Fn",
            (* "fn" *) token env tok
          )
        | `Rescue tok -> R.Case ("Rescue",
            (* "rescue" *) token env tok
          )
        )
      )
    | `Op_id x -> R.Case ("Op_id",
        map_operator_identifier env x
      )
    | `Quoted_i_double x -> R.Case ("Quoted_i_double",
        map_quoted_i_double env x
      )
    | `Quoted_i_single x -> R.Case ("Quoted_i_single",
        map_quoted_i_single env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_rescue_block (env : env) ((v1, v2, v3) : CST.rescue_block) =
  let v1 = (* "rescue" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119 env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_stab_clause (env : env) ((v1, v2, v3) : CST.stab_clause) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_stab_clause_left env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "->" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_body env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_stab_clause_arguments_with_parentheses (env : env) ((v1, v2, v3) : CST.stab_clause_arguments_with_parentheses) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_stab_clause_arguments_without_parentheses env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_stab_clause_arguments_without_parentheses (env : env) (x : CST.stab_clause_arguments_without_parentheses) =
  (match x with
  | `Exp_rep_COMMA_exp_opt_COMMA_keywos x -> R.Case ("Exp_rep_COMMA_exp_opt_COMMA_keywos",
      map_anon_exp_rep_COMMA_exp_opt_COMMA_keywos_041d82e env x
    )
  | `Keywos x -> R.Case ("Keywos",
      map_keywords env x
    )
  )

and map_stab_clause_left (env : env) (x : CST.stab_clause_left) =
  (match x with
  | `Stab_clause_args_with_parens_bf4a580 x -> R.Case ("Stab_clause_args_with_parens_bf4a580",
      map_stab_clause_arguments_with_parentheses env x
    )
  | `Stab_clause_args_with_parens_with_guard_9d9f341 (v1, v2, v3) -> R.Case ("Stab_clause_args_with_parens_with_guard_9d9f341",
      let v1 =
        map_stab_clause_arguments_with_parentheses env v1
      in
      let v2 = (* "when" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Stab_clause_args_with_parens_a52ef95 x -> R.Case ("Stab_clause_args_with_parens_a52ef95",
      map_stab_clause_arguments_without_parentheses env x
    )
  | `Stab_clause_args_with_parens_with_guard_cfbae3b (v1, v2, v3) -> R.Case ("Stab_clause_args_with_parens_with_guard_cfbae3b",
      let v1 =
        map_stab_clause_arguments_without_parentheses env v1
      in
      let v2 = (* "when" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_string_ (env : env) (x : CST.string_) =
  (match x with
  | `Quoted_i_double x -> R.Case ("Quoted_i_double",
      map_quoted_i_double env x
    )
  | `Quoted_i_here_double x -> R.Case ("Quoted_i_here_double",
      map_quoted_i_heredoc_double env x
    )
  )

and map_struct_ (env : env) (x : CST.struct_) =
  (match x with
  | `Alias tok -> R.Case ("Alias",
      (* alias *) token env tok
    )
  | `Atom x -> R.Case ("Atom",
      map_atom env x
    )
  | `Id x -> R.Case ("Id",
      map_identifier env x
    )
  | `Un_op x -> R.Case ("Un_op",
      map_unary_operator env x
    )
  | `Dot x -> R.Case ("Dot",
      map_dot env x
    )
  | `Call_with_parens x -> R.Case ("Call_with_parens",
      map_call_with_parentheses env x
    )
  )

and map_tuple (env : env) ((v1, v2, v3) : CST.tuple) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_items_with_trailing_separator env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_unary_operator (env : env) (x : CST.unary_operator) =
  (match x with
  | `Opt_before_un_op_AMP_capt_exp (v1, v2, v3) -> R.Case ("Opt_before_un_op_AMP_capt_exp",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* before_unary_op *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_capture_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Opt_before_un_op_choice_PLUS_exp (v1, v2, v3) -> R.Case ("Opt_before_un_op_choice_PLUS_exp",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* before_unary_op *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = map_anon_choice_PLUS_8019319 env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Opt_before_un_op_AT_exp (v1, v2, v3) -> R.Case ("Opt_before_un_op_AT_exp",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* before_unary_op *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "@" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Opt_before_un_op_AMP_int (v1, v2, v3) -> R.Case ("Opt_before_un_op_AMP_int",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* before_unary_op *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "&" *) token env v2 in
      let v3 = (* integer *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_source (env : env) ((v1, v2) : CST.source) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_expression env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = map_terminator env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_terminator env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let dump_tree root =
  map_source () root
  |> Tree_sitter_run.Raw_tree.to_string
  |> print_string
