
include!(concat!(env!("OUT_DIR"), "/exceptional-grammar.rs"));

#[cfg(test)]
mod test_literals {
    use test_helpers::*;

    #[test]
    fn parses_number() {
        assert_eq!(parse_literal(&"1234"), l_number(1234, 1));

        assert_eq!(parse_literal(&"0011"), l_number(11, 1));
    }

    #[test]
    fn parses_strings() {
        assert_eq!(parse_literal(&"\"\""), l_string(&""));

        assert_eq!(parse_literal(&"\"string with more words\""),
                   l_string(&"string with more words"));
    }

    #[test]
    fn parses_booleans() {
        assert_eq!(parse_literal(&"true"), l_bool(true));

        assert_eq!(parse_literal(&"false"), l_bool(false));
    }

    #[test]
    fn parses_maps() {
        assert_eq!(parse_literal(&"{ a => b, b => c }"),
                   l_map(vec![(e_identifier(&"a"), e_identifier(&"b")),
                              (e_identifier(&"b"), e_identifier(&"c"))]))
    }
}

#[cfg(test)]
mod test_expressions {
    use test_helpers::*;

    #[test]
    fn parses_simple_literal_expressions() {
        assert_eq!(parse_expression(&"1"), e_literal(l_number(1, 1)));

        assert_eq!(parse_expression(&"\"\""), e_literal(l_string(&"")));

        assert_eq!(parse_expression(&"{ \"a\" => 1 }"),
                   e_literal(l_map(vec![(e_literal(l_string(&"a")), e_literal(l_number(1, 1)))])))
    }

    #[test]
    fn parses_math() {
        assert_eq!(parse_expression(&"1 + 2 * 3 / 5 - d"),
                   e_binop("+",
                           e_literal(l_number(1, 1)),
                           e_binop("-",
                                   e_binop("*",
                                           e_literal(l_number(2, 1)),
                                           e_binop("/",
                                                   e_literal(l_number(3, 1)),
                                                   e_literal(l_number(5, 1)))),
                                   e_identifier(&"d"))));
    }

    #[test]
    fn parses_identifiers() {
        assert_eq!(parse_expression(&"toto"), e_identifier(&"toto"))
    }

    #[test]
    fn parses_access() {
        assert_eq!(parse_expression(&"toto[titi][tutu]"),
                   e_index_access(e_index_access(e_identifier(&"toto"), e_identifier(&"titi")),
                                  e_identifier(&"tutu")));
        assert_eq!(parse_expression(&"toto.titi.tutu"),
                   e_index_access(e_index_access(e_identifier(&"toto"),
                                                 e_literal(l_string(&"titi"))),
                                  e_literal(l_string(&"tutu"))));
    }

    #[test]
    fn parses_simple_functions() {
        assert_eq!(parse_expression(&"def() do end"),
                   e_literal(l_function(vec![], vec![])))
    }

    #[test]
    fn parses_functions_with_args() {
        assert_eq!(parse_expression(&"def(a, b) do end"),
                   e_literal(l_function(vec!["a".to_owned(), "b".to_owned()], vec![])))
    }

    #[test]
    fn prases_functions_with_bodies() {
        assert_eq!(parse_expression(&"def(a, b) do\nlet c = 1\nend"),
                   e_literal(l_function(vec!["a".to_owned(), "b".to_owned()],
                                        vec![s_assign(&"c", l_number(1, 1))])))
    }

    #[test]
    fn parses_import() {
        assert_eq!(parse_expression(&"import(\"toto\")"),
                   e_import(e_literal(l_string(&"toto"))))
    }
}

#[cfg(test)]
mod test_statements {
    use test_helpers::*;

    #[test]
    fn parses_assigns() {
        assert_eq!(parse_statements(&"let a = 1"),
                   [s_assign(&"a", l_number(1, 1))])
    }

    #[test]
    fn parses_map_assign() {
        assert_eq!(parse_statements(&"a[b][c] = d"),
                   [s_index_assign(e_index_access(e_identifier(&"a"), e_identifier(&"b")),
                                   e_identifier(&"c"),
                                   e_identifier(&"d"))])
    }

    #[test]
    fn parses_calls() {
        assert_eq!(parse_statements("a()"),
                   [s_call(e_identifier(&"a"), vec![])]);
        assert_eq!(parse_statements(&"toto.titi()"),
                   vec![s_call(e_index_access(e_identifier(&"toto"),
                                              e_literal(l_string(&"titi"))),
                               vec![])]);

    }

    #[test]
    fn parses_calls_with_simple_args() {
        assert_eq!(parse_statements("a(1, b)"),
                   [s_call(e_identifier(&"a"),
                           vec![e_literal(l_number(1, 1)), e_identifier(&"b")])])
    }

    #[test]
    fn parses_calls_with_expressive_args() {
        let args = vec![e_binop("+", e_literal(l_number(1, 1)), e_literal(l_number(2, 1))),
                        e_literal(l_function(vec!["x".to_owned()], vec![]))];
        assert_eq!(parse_statements("a(1 + 2, def(x) do end)"),
                   [s_call(e_identifier(&"a"), args)])
    }

    #[test]
    fn parses_raise_statements() {
        assert_eq!(parse_statements("raise(1)"),
                   [s_raise(e_literal(l_number(1, 1)))])
    }

    #[test]
    fn parses_rescue_statements() {
        assert_eq!(parse_statements("rescue({}) do\nend"),
                   [s_rescue(p_map(vec![]), vec![])])
    }

    #[test]
    fn parses_rescue_with_number_patterns() {
        assert_eq!(parse_statements("rescue(1) do\nend"),
                   [s_rescue(p_number(1, 1), vec![])])
    }

    #[test]
    fn parses_rescue_with_string_patterns() {
        assert_eq!(parse_statements("rescue(\"toto\") do\nend"),
                   [s_rescue(p_string("toto"), vec![])])
    }

    #[test]
    fn parses_rescue_with_ident_patterns() {
        assert_eq!(parse_statements("rescue(toto) do\nend"),
                   [s_rescue(p_ident("toto"), vec![])])
    }

    #[test]
    fn parses_rescue_with_bool_patterns() {
        assert_eq!(parse_statements("rescue(false) do\nend"),
                   [s_rescue(p_bool(false), vec![])])
    }

    #[test]
    fn parses_rescue_with_map_patterns() {
        assert_eq!(parse_statements("rescue({ 1 => y }) do\nend"),
                   [s_rescue(p_map(vec![(p_number(1, 1), p_ident("y"))]), vec![])]);
        assert_eq!(parse_statements("rescue({ \"m\" => y, \"n\" => z }) do\nend"),
                   [s_rescue(p_map(vec![(p_string("m"), p_ident("y")),
                                        (p_string("n"), p_ident("z"))]),
                             vec![])])
    }
}
