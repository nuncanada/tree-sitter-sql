const PREC = {
  primary: 8,
  unary: 7,
  exp: 6,
  multiplicative: 5,
  additive: 4,
  comparative: 3,
  and: 2,
  or: 1,
};
const multiplicative_operators = ["*", "/", "%", "<<", ">>", "&"];
const additive_operators = ["+", "-", "|", "#"];
const comparative_operators = [
  "<",
  "<=",
  "<>",
  "!=",
  "=",
  ">",
  ">=",
  "~",
  "!~",
  "~*",
  "!~*",
];

// Generate case insentitive match for SQL keyword
// In case of multiple word keyword provide a seq matcher
function kw(keyword) {
  if (keyword.toUpperCase() != keyword) {
    throw new Error(`Expected upper case keyword got ${keyword}`);
  }
  const words = keyword.split(" ");
  const regExps = words.map(createCaseInsensitiveRegex);

  return regExps.length == 1
    ? alias(regExps[0], keyword)
    : alias(seq(...regExps), keyword.replace(/ /g, "_"));
}

function createOrReplace(item) {
  if (item.toUpperCase() != item) {
    throw new Error(`Expected upper case item got ${item}`);
  }
  return alias(
    seq(
      createCaseInsensitiveRegex("CREATE"),
      field("replace", optional(createCaseInsensitiveRegex("OR REPLACE"))),
      createCaseInsensitiveRegex(item),
    ),
    `CREATE_OR_REPLACE_${item}`,
  );
}

function createCaseInsensitiveRegex(word) {
  return new RegExp(
    word
      .split("")
      .map(letter => `[${letter.toLowerCase()}${letter.toUpperCase()}]`)
      .join(""),
  );
}

function kv(key, value) {
  return alias(
    value === null
      ? createCaseInsensitiveRegex(key)
      : seq(createCaseInsensitiveRegex(key), "=", field("value", value)),
    key.toLowerCase(),
  );
}

module.exports = grammar({
  name: "sql",
  extras: $ => [$.comment, /[\s\f\uFEFF\u2060\u200B]|\\\r?\n/],
  externals: $ => [
    $._dollar_quoted_string_tag,
    $._dollar_quoted_string_content,
    $._dollar_quoted_string_end_tag,
  ],

  rules: {
    source_file: $ => repeat($._statement),

    _statement: $ =>
      seq(
        choice(
          $.select_statement,
          $.create_table_statement,
          $.create_function_statement,
          $.create_view_statement,
        ),
        optional(";"),
      ),

    _simple_statement: $ =>
      prec.right(
        seq(
          choice(
            $.select_statement,
            $.create_table_statement,
            $.create_view_statement,
          ),
          optional(";"),
        ),
      ),

    with_clause: $ =>
      seq(kw("WITH"), optional(kw("RECURSIVE")), commaSep1($.cte)),

    cte: $ =>
      seq(
        $.identifier,
        kw("AS"),
        optional(seq(optional(kw("NOT")), kw("MATERIALIZED"))),
        "(",
        choice(
          $.select_statement,
        ),
        ")",
      ),

    select_statement: $ => seq(optional($.with_clause), $._select_statement),

    _function_language: $ =>
      seq(
        kw("LANGUAGE"),
        alias(choice(/[a-zA-Z]+/, /'[a-zA-Z]+'/), $.language),
      ),
    _compound_statement: $ =>
        prec.right(
            seq(
                optional(seq(field("begin_label", $.identifier), ":")),
                kw("BEGIN"),
                optional(kw("ATOMIC")),
                repeat1($._simple_statement),
                kw("END"),
                optional(field("end_label", $.identifier)),
                optional(";"),
            ),
        ),
    return_statement: $ =>
        seq(kw("RETURN"), choice($._expression, $.select_statement)),
    declare_statement: $ =>
        seq(kw("DECLARE"), $.identifier, $._type, optional($.default_clause)),

    create_function_statement: $ =>
        prec.right(
            seq(
                "FUNCTION",
                $._identifier,
                $.create_function_parameters,
                seq(kw("RETURNS"), $._create_function_return_type),
                repeat(
                    choice(
                        $._function_language,
                        seq(kw("TRANSFORM FOR TYPE"), commaSep1($.identifier)),
                        kw("WINDOW"),
                        seq(optional(kw("NOT")), kw("LEAKPROOF")),
                        seq(kw("COST"), $.number),
                        seq(kw("ROWS"), $.number),
                        seq(kw("SUPPORT"), $.identifier),
                        $.function_body,
                    ),
                ),
            ),
        ),

    _create_function_return_type: $ =>
        prec.right(choice($.setof, $._type, $.constrained_type)),
    setof: $ =>
        prec.right(seq(kw("SETOF"), choice($._type, $.constrained_type))),
    constrained_type: $ => seq($._type, $.null_constraint),
    create_function_parameter: $ =>
        seq(
            field(
                "argmode",
                optional(choice(kw("IN"), kw("OUT"), kw("INOUT"), kw("VARIADIC"))),
            ),
            optional($.identifier),
            choice($._type, $.constrained_type),
            optional(seq("=", alias($._expression, $.default))),
        ),
    create_function_parameters: $ =>
        seq("(", optional(commaSep1($.create_function_parameter)), ")"),

    function_body: $ =>
        choice(
            $._simple_statement,
            $._compound_statement,
            seq(kw("AS"), field("script", $.string)),
            seq(
                kw("AS"),
                field("obj_file", $.string),
                field("link_symbol", $.string),
            ),
        ),
    table_column: $ =>
        prec.right(
            seq(
                field("name", $._identifier),
                field("type", $._type),
                repeat(
                    choice(
                        $.default_clause,
                        $.check_constraint,
                        $.unique_constraint,
                        $.null_constraint,
                    ),
                ),
            ),
        ),
    direction_constraint: _ => choice(kw("ASC"), kw("DESC")),
    named_constraint: $ => seq("CONSTRAINT", $.identifier),
    _column_default_expression: $ =>
        choice(
            $._parenthesized_expression,
            $.string,
            $.number,
            $.identifier,
            $.function_call,
        ),
    default_clause: $ =>
        seq(
            kw("DEFAULT"),
            // TODO: this should be specific variable-free expression https://www.postgresql.org/docs/9.1/sql-createtable.html
            // TODO: simple expression to use for check and default
            choice($._column_default_expression, $.type_cast),
        ),
    table_parameters: $ =>
        seq(
            "(",
            optional(commaSep1(choice($.table_column, $._table_constraint))),
            ")",
        ),
    _table_constraint: $ =>
        prec.right(
            seq(
                optional(seq(kw("CONSTRAINT"), field("name", $._identifier))),
                choice(
                    alias($.table_constraint_unique, $.unique),
                    alias($.table_constraint_check, $.check),
                    alias($.table_constraint_exclude, $.exclude),
                ),
            ),
        ),
    table_constraint_check: $ => seq(kw("CHECK"), $._expression),
    op_class: $ => $._identifier,
    exclude_entry: $ =>
        seq(
            $._identifier,
            optional($.op_class),
            optional(seq(kw("WITH"), $.binary_operator)),
        ),
    table_constraint_exclude: $ =>
        seq(
            kw("EXCLUDE"),
            optional(seq(kw("USING"), $._identifier)),
            "(",
            commaSep1($.exclude_entry),
            ")",
        ),
    table_constraint_unique: $ =>
        seq(kw("UNIQUE"), "(", commaSep1($._identifier), ")"),
    create_table_statement: $ =>
        seq(
            kw("CREATE"),
            kw("TABLE"),
            $._identifier,
            $.table_parameters,
        ),
    using_clause: $ =>
        seq(
            kw("USING"),
            choice($.identifier, seq("(", commaSep1($._identifier), ")")),
        ),
    index_table_parameters: $ =>
        seq(
            "(",
            commaSep1(
                seq(
                    choice($._expression, $.ordered_expression),
                    optional($.op_class),
                ),
            ),
            ")",
        ),
    create_view_statement: $ =>
      prec.right(
        seq(
          kw("CREATE"),
          optional(createCaseInsensitiveRegex("OR REPLACE")),
          optional(choice(kw("TEMPORARY"), kw("TEMP"))),
          kw("VIEW"),
          $._identifier,
          optional($.view_columns),
          optional($.view_options),
          $.view_body,
          optional($.view_check_option),
        ),
      ),
    view_columns: $ => seq("(", commaSep1($._identifier), ")"),
    // PostgreSQL currently only support the SECURITY_BARRIER option
    view_option: $ => choice($._identifier, $.assigment_expression),
    view_options: $ => seq(kw("WITH"), "(", commaSep1($.view_option), ")"),
    // MySQL support
    view_check_option: $ =>
      seq(
        kw("WITH"),
        optional(choice(kw("CASCADED"), kw("LOCAL"))),
        kw("CHECK OPTION"),
      ),
    view_body: $ =>
      seq(
        kw("AS"),
        choice($.select_statement, $.select_subexpression, $.values_clause),
      ),


    // SELECT
    _select_statement: $ =>
      prec.right(
        seq(
          $.select_clause,
          optional($.from_clause),
          optional($.where_clause),
          optional($.group_by_clause),
          optional(commaSep1($.window_clause)),
          optional($.order_by_clause),
          optional($.limit_clause),
          optional($.offset_clause),
        ),
      ),

    group_by_clause: $ =>
      seq(
        kw("GROUP BY"),
        commaSep1($.group_expression),
        optional($.having_clause),
      ),
    having_clause: $ => seq(kw("HAVING"), $._expression),
    group_expression: $ =>
      choice(
        $._expression,
        $.grouping_sets_clause,
        $.rollup_clause,
        $.cube_clause,
      ),
    grouping_sets_clause: $ =>
      seq(kw("GROUPING SETS"), "(", commaSep1($.expression_list), ")"),
    rollup_clause: $ =>
      seq(
        kw("ROLLUP"),
        "(",
        commaSep1(choice($._expression, $.expression_list)),
        ")",
      ),
    cube_clause: $ =>
      seq(
        kw("CUBE"),
        "(",
        commaSep1(choice($._expression, $.expression_list)),
        ")",
      ),
    expression_list: $ => seq("(", optional(commaSep1($._expression)), ")"),
    order_expression: $ =>
      seq(
        $._expression,
        optional(alias(choice(kw("ASC"), kw("DESC")), $.order)),
        optional(
          alias(
            seq(kw("NULLS"), choice(kw("FIRST"), kw("LAST"))),
            $.nulls_order,
          ),
        ),
      ),
    window_clause: $ =>
      seq(kw("WINDOW"), $.identifier, kw("AS"), $.window_definition),
    order_by_clause: $ => seq(kw("ORDER BY"), commaSep1($.order_expression)),
    limit_clause: $ =>
      seq(
        kw("LIMIT"),
        choice($.number, kw("ALL")),
        optional(seq(",", $.number)), // MySQL LIMIT a, b
      ),
    offset_clause: $ =>
      prec.right(
        seq(kw("OFFSET"), $.number, optional(choice(kw("ROW"), kw("ROWS")))),
      ),
    fetch_clause: $ =>
      seq(
        kw("FETCH"),
        choice(kw("FIRST"), kw("NEXT")),
        optional($.number),
        choice(kw("ROW"), kw("ROWS")),
        kw("ONLY"),
      ),
    where_clause: $ => seq(kw("WHERE"), $._expression),
    alias: $ =>
      choice(
        seq(
          $.identifier,
          optional(choice($.column_names, $.column_definitions)),
        ),
        $.column_definitions,
      ),
    _aliased_expression: $ => seq($._expression, optional(kw("AS")), $.alias),
    column_names: $ => seq("(", commaSep1($.identifier), ")"),
    column_definitions: $ => seq("(", commaSep1($.table_column), ")"),
    _aliasable_expression: $ =>
      prec.right(choice($._expression, $._aliased_expression)),
    select_clause_body: $ =>
      commaSep1(
        seq(
          $._aliasable_expression,
          optional(seq(kw("INTO"), field("into", $.identifier))),
        ),
      ),
    select_clause: $ =>
      prec.right(seq(kw("SELECT"), optional($.select_clause_body))),
    from_clause: $ => seq(kw("FROM"), commaSep1($._from_item)),
    _from_item: $ =>
      choice(
        seq(
          optional(kw("ONLY")),
          $._aliasable_expression,
          optional($.tablesample_clause),
        ),
      ),
    tablesample_clause: $ =>
      seq(kw("TABLESAMPLE"), $.function_call, optional($.repeatable_clause)),
    repeatable_clause: $ =>
      seq(kw("REPEATABLE"), "(", field("seed", $._expression), ")"),
    direction_constraint: _ => choice(kw("ASC"), kw("DESC")),

    select_subexpression: $ =>
      prec(1, seq(optional(kw("LATERAL")), "(", $.select_statement, ")")),

    assigment_expression: $ => seq($._identifier, "=", $._expression),

    values_clause: $ =>
      seq(
        kw("VALUES"),
        commaSep1($.values_clause_item),
        optional($.order_by_clause),
        optional($.limit_clause),
        optional($.offset_clause),
        optional($.fetch_clause),
      ),
    values_clause_item: $ => seq("(", commaSep1($._expression), ")"),

    conditional_expression: $ =>
      seq(
        kw("CASE"),
        repeat1(seq(kw("WHEN"), $._expression, kw("THEN"), $._expression)),
        optional(seq(kw("ELSE"), $._expression)),
        kw("END"),
      ),

    in_expression: $ =>
      prec.left(
        PREC.comparative,
        seq($._expression, optional(kw("NOT")), kw("IN"), $.tuple),
      ),
    tuple: $ =>
      seq(
        // TODO: maybe collapse with function arguments, but make sure to preserve clarity
        "(",
        field("elements", commaSep1($._expression)),
        ")",
      ),
    unique_constraint: $ => kw("UNIQUE"),
    null_constraint: $ => seq(optional(kw("NOT")), $.NULL),
    check_constraint: $ => seq(kw("CHECK"), $._expression),
    _constraint: $ =>
        seq(
            choice($.null_constraint, $.check_constraint),
            optional($.check_constraint),
        ),
    function_call: $ =>
      prec.right(
        1,
        seq(
          optional(kw("LATERAL")),
          field("function", $._identifier),
          "(",
          optional(field("arguments", $._function_call_arguments)),
          ")",
          optional($.with_ordinality),
          optional($.within_group_clause),
          optional($.filter_clause),
          optional($.over_clause),
        ),
      ),
    _function_call_arguments: $ =>
      seq(
        optional(choice(kw("ALL"), kw("DISTINCT"))),
        commaSep1($._expression),
        optional($.order_by_clause),
      ),
    within_group_clause: $ =>
      seq(kw("WITHIN GROUP"), "(", $.order_by_clause, ")"),
    filter_clause: $ => seq(kw("FILTER"), "(", $.where_clause, ")"),
    over_clause: $ =>
      seq(kw("OVER"), choice($.identifier, $.window_definition)),
    window_definition: $ =>
      seq(
        "(",
        optional($.partition_by_clause),
        optional($.order_by_clause),
        optional($.frame_clause),
        ")",
      ),
    partition_by_clause: $ => seq(kw("PARTITION BY"), commaSep1($._expression)),
    frame_clause: $ =>
      choice(
        seq(
          $.frame_kind,
          field("frame_start", $.frame_bound),
          optional($.frame_exclusion),
        ),
        seq(
          $.frame_kind,
          kw("BETWEEN"),
          field("frame_start", $.frame_bound),
          kw("AND"),
          field("frame_end", $.frame_bound),
          optional($.frame_exclusion),
        ),
      ),
    frame_kind: $ => choice(kw("RANGE"), kw("ROWS"), kw("GROUPS")),
    frame_bound: $ =>
      choice(
        kw("UNBOUNDED PRECEDING"),
        seq($._expression, kw("PRECEDING")),
        kw("CURRENT ROW"),
        seq($._expression, kw("FOLLOWING")),
        kw("UNBOUNDED FOLLOWING"),
      ),
    frame_exclusion: $ =>
      choice(
        kw("EXCLUDE CURRENT ROW"),
        kw("EXCLUDE GROUP"),
        kw("EXCLUDE TIES"),
        kw("EXCLUDE NO OTHERS"),
      ),

    _parenthesized_expression: $ =>
      prec.left(PREC.unary, seq("(", $._expression, ")")),
    with_ordinality: $ => kw("WITH ORDINALITY"),
    is_expression: $ =>
      prec.left(
        PREC.comparative,
        seq(
          $._expression,
          kw("IS"),
          optional(kw("NOT")),
          choice($.NULL, $.TRUE, $.FALSE, $.distinct_from),
        ),
      ),
    distinct_from: $ => prec.left(seq(kw("DISTINCT FROM"), $._expression)),
    boolean_expression: $ =>
      choice(
        prec.left(PREC.unary, seq(kw("NOT"), $._expression)),
        prec.left(PREC.and, seq($._expression, kw("AND"), $._expression)),
        prec.left(PREC.or, seq($._expression, kw("OR"), $._expression)),
      ),
    at_time_zone_expression: $ =>
      prec.left(
        PREC.primary,
        seq($._expression, kw("AT TIME ZONE"), $._expression),
      ),
    NULL: $ => kw("NULL"),
    TRUE: $ => kw("TRUE"),
    FALSE: $ => kw("FALSE"),

    number: $ => {
      const digits = repeat1(/[0-9]+_?/);
      const exponent = seq(/[eE][\+-]?/, digits);

      return token(
        seq(
          choice(
            seq(digits, ".", optional(digits), optional(exponent)),
            seq(optional(digits), ".", digits, optional(exponent)),
            seq(digits, exponent),
            seq(digits),
          ),
        ),
      );
    },

    _unquoted_identifier: $ => /[a-zA-Z0-9_]+/,
    _quoted_identifier: $ => seq('"', field("name", /(""|[^"])*/), '"'), // ANSI QUOTES
    identifier: $ => choice($._unquoted_identifier, $._quoted_identifier),
    dotted_name: $ => prec.left(PREC.primary, sep2($.identifier, ".")),
    _identifier: $ => choice($.identifier, $.dotted_name),
    string: $ =>
      choice(
        seq("'", field("content", alias(/(''|[^'])*/, $.content)), "'"),
        seq(
          $._dollar_quoted_string_tag,
          field("content", alias($._dollar_quoted_string_content, $.content)),
          $._dollar_quoted_string_end_tag,
        ),
      ),
    json_access: $ =>
      seq(
        $._expression,
        choice("->", "->>", "#>", "#>>"),
        choice($.string, $.number),
      ),
    ordered_expression: $ =>
      seq($._expression, field("order", choice(kw("ASC"), kw("DESC")))),
    type: $ =>
      prec.right(
        seq(
          $._identifier,
          optional(kw("VARYING")), // CHARACTER/BIT VARYING
          optional(kw("PRECISION")), // DOUBLE PRECISION
          optional(seq("(", commaSep1($.number), ")")),
          optional(seq(choice(kw("WITH"), kw("WITHOUT")), kw("TIME ZONE"))), // TIME/TIMESTAMP (n) WITH/WITHOUT TIME ZONE
        ),
      ),
    array_type: $ =>
      prec.right(seq($._type, repeat1(seq("[", optional($.number), "]")))),
    _type: $ => choice($.type, $.array_type),
    type_cast: $ =>
      seq(
        // TODO: should be moved to basic expression or something
        choice(
          $._parenthesized_expression,
          $.string,
          $._identifier,
          $.function_call,
        ),
        "::",
        field("type", $._type),
      ),

    // http://stackoverflow.com/questions/13014947/regex-to-match-a-c-style-multiline-comment/36328890#36328890
    comment: $ =>
      token(
        choice(seq("--", /.*/), seq("/*", /[^*]*\*+([^/*][^*]*\*+)*/, "/")),
      ),
    array_element_access: $ =>
      seq(choice($.identifier, $.argument_reference), "[", $._expression, "]"),

    unary_expression: $ =>
      prec(
        PREC.unary,
        seq(
          field(
            "operator",
            choice(
              "+",
              "-",
              "!!", // Factorial op (Removed in Postgres >= 14)
              "~", // Bitwise not
              "@", // Absolute value
              "|/", // square root
              "||/", // cube root
            ),
          ),
          field("operand", $._expression),
        ),
      ),

    binary_expression: $ => {
      const table = [
        [PREC.exp, "^"],
        [PREC.multiplicative, choice(...multiplicative_operators)],
        [PREC.additive, choice(...additive_operators)],
        [PREC.comparative, choice(...comparative_operators)],
      ];

      return choice(
        ...table.map(([precedence, operator]) =>
          prec.left(
            precedence,
            seq(
              field("left", $._expression),
              field("operator", operator),
              field("right", $._expression),
            ),
          ),
        ),
      );
    },

    binary_operator: $ => choice("=", "&&", "||"),
    asterisk_expression: $ => choice("*", seq($._identifier, ".*")),
    interval_expression: $ => seq(token(prec(1, kw("INTERVAL"))), $.string),
    argument_reference: $ => seq("$", /\d+/),
    _expression: $ =>
      choice(
        $.interval_expression,
        $.function_call,
        $.string,
        $.json_access,
        $.TRUE,
        $.FALSE,
        $.NULL,
        $.asterisk_expression,
        $._identifier,
        $.number,
        $.in_expression,
        $.is_expression,
        $.boolean_expression,
        $._parenthesized_expression,
        $.type_cast,
        $.unary_expression,
        $.binary_expression,
        $.conditional_expression,
        $.array_element_access,
        $.argument_reference,
        $.select_subexpression,
        $.at_time_zone_expression,
      ),
  },
});

function commaSep1(rule) {
  return sep1(rule, ",");
}

function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}

function sep2(rule, separator) {
  return seq(rule, repeat1(seq(separator, rule)));
}
