// DDF declarations and values; verbatim code is deliberately kept opaque.
const commaSep = rule => optional(seq(rule, repeat(seq(',', rule)), optional(',')));
const modifiers = [
  'abstract', 'meta', 'native', 'noretail', 'editor', 'owned', 'shared',
  'override', 'virtual', 'static', 'const', 'global', 'extendable', 'fragment',
  'input', 'output', 'link', 'dynamic', 'observer', 'client', 'server', 'in', 'out', 'ref',
];

module.exports = grammar({
  name: 'ddf',

  extras: $ => [/\s/, $.comment, $.preprocessor_directive],
  word: $ => $.identifier,
  conflicts: $ => [[$.type_body, $.enum_body], [$.modifier, $.enum_declaration]],

  rules: {
    source_file: $ => repeat($._declaration),

    _declaration: $ => choice(
      $.import_declaration,
      $.module_declaration,
      $.namespace_declaration,
      $.type_declaration,
      $.enum_declaration,
      $.extension_declaration,
      $.function_declaration,
      $.hash_declaration,
      $.instance_declaration,
      $.cpp_block,
      $.csharp_block,
      ';',
    ),

    import_declaration: $ => seq('import', field('path', $.string_literal), ';'),
    module_declaration: $ => seq('module', field('name', $.qualified_identifier), ';'),
    namespace_declaration: $ => seq('namespace', field('name', $.qualified_identifier), ';'),

    _attributes: $ => repeat1($.attribute_list),
    attribute_list: $ => seq('[', commaSep($.attribute), ']'),
    attribute: $ => seq(
      field('name', $.qualified_identifier),
      optional($.argument_list),
    ),
    argument_list: $ => seq('(', commaSep(choice($.named_argument, $._expression)), ')'),
    named_argument: $ => seq(
      field('name', $.qualified_identifier), '=', field('value', $._expression),
    ),

    modifier: _ => choice(...modifiers),
    _modifiers: $ => repeat1($.modifier),
    type_declaration: $ => seq(
      optional($._attributes),
      optional($._modifiers),
      field('kind', choice('class', 'struct', 'nativestruct', 'interface',
        'event', 'message', 'entity', 'component')),
      field('name', $.qualified_identifier),
      optional($.base_list),
      choice($.type_body, $.parameter_list, ';'),
    ),
    base_list: $ => seq(':', $.type_reference, repeat(seq(',', $.type_reference))),
    type_body: $ => seq('{', repeat($._member), '}'),
    _member: $ => choice(
      $.field_declaration,
      $.event_field_declaration,
      $.inline_declaration,
      $.function_declaration,
      $.type_declaration,
      $.enum_declaration,
      $.data_declaration,
      $.section_label,
      $.cpp_block,
      $.csharp_block,
      ';',
    ),
    data_declaration: $ => seq(
      optional($._attributes), field('kind', choice('data', 'client', 'server')), $.type_body,
    ),
    section_label: $ => seq(field('name', $.identifier), ':'),
    field_declaration: $ => seq(
      optional($._attributes),
      optional($._modifiers),
      field('type', $.type_reference),
      field('name', $.identifier),
      repeat($.array_suffix),
      optional(seq('=', field('value', $._expression))),
      ';',
    ),
    event_field_declaration: $ => seq(
      optional($._attributes), optional($._modifiers), 'event',
      field('type', $.type_reference),
      field('name', $.identifier),
      optional(seq('=', field('value', $._expression))), ';',
    ),
    inline_declaration: $ => seq(optional($._attributes), 'inline', field('type', $.type_reference), ';'),

    enum_declaration: $ => seq(
      optional($._attributes), optional($._modifiers),
      choice('enum', 'extendable'), field('name', $.qualified_identifier),
      optional(seq(':', $.type_reference)), $.enum_body,
    ),
    enum_body: $ => seq('{', commaSep($.enum_member), '}'),
    enum_member: $ => seq(
      optional($._attributes), field('name', $.identifier),
      optional(seq('=', field('value', $._expression))),
    ),
    extension_declaration: $ => seq(
      optional($._attributes), 'extend',
      optional(field('kind', choice('class', 'enum', 'entity', 'component'))),
      field('name', $.qualified_identifier),
      optional(seq('as', field('extent', $.qualified_identifier))),
      choice($.enum_body, $.type_body),
    ),

    function_declaration: $ => seq(
      optional($._attributes), optional($._modifiers),
      field('kind', choice('function', 'functiondecl', 'delegate')),
      optional($._modifiers),
      field('type', $.type_reference),
      field('name', $.qualified_identifier),
      $.parameter_list,
      optional($._modifiers),
      choice(';', $.type_body),
    ),
    parameter_list: $ => seq('(', commaSep($.parameter), ')'),
    parameter: $ => seq(
      optional($._attributes), optional($._modifiers),
      field('type', $.type_reference),
      field('name', $.identifier),
      repeat($.array_suffix),
      optional(seq('=', field('value', $._expression))),
    ),
    type_reference: $ => seq(
      optional(choice('class', 'struct')),
      $.qualified_identifier,
      optional($.type_arguments),
      repeat(choice('*', '&', $.array_suffix)),
    ),
    type_arguments: $ => seq('<', $.type_reference, repeat(seq(',', $.type_reference)), '>'),
    array_suffix: _ => seq('[', ']'),

    hash_declaration: $ => seq(
      optional($._attributes), 'hash', field('name', $.identifier), optional($.string_literal), ';',
    ),
    instance_declaration: $ => seq(
      optional($._attributes),
      field('type', $.type_reference),
      field('name', choice($.asset_path, $.qualified_identifier)),
      '=', field('value', $.object_initializer),
    ),

    _expression: $ => choice(
      $.number_literal,
      $.string_literal,
      $.boolean_literal,
      $.null_literal,
      $.qualified_identifier,
      $.asset_path,
      $.object_initializer,
      $.array_initializer,
      $.call_expression,
      $.parenthesized_expression,
      $.unary_expression,
      $.binary_expression,
    ),
    object_initializer: $ => seq(
      '{', commaSep(choice($.named_argument, $._expression)), '}',
    ),
    array_initializer: $ => seq('[', commaSep($._expression), ']'),
    call_expression: $ => prec(14, seq(
      field('function', $.qualified_identifier), $.argument_list,
    )),
    parenthesized_expression: $ => seq('(', $._expression, ')'),
    unary_expression: $ => prec(13, seq(
      field('operator', choice('+', '-', '~', '!')), field('operand', $._expression),
    )),
    binary_expression: $ => choice(...[
      [12, ['*', '/', '%']], [11, ['+', '-']], [10, ['<<', '>>']],
      [9, ['<', '<=', '>', '>=']], [8, ['==', '!=']], [7, ['&']],
      [6, ['^']], [5, ['|']], [4, ['&&']], [3, ['||']],
    ].map(([precedence, operators]) => prec.left(precedence, seq(
      field('left', $._expression),
      field('operator', operators.length === 1 ? operators[0] : choice(...operators)),
      field('right', $._expression),
    )))),
    number_literal: _ => token(
      /(?:0[xX][0-9a-fA-F]+|0[bB][01]+|(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?)[uUlLfFdD]*/,
    ),
    string_literal: $ => repeat1($._string),
    _string: _ => token(seq('"', repeat(choice(/[^"\\]/, /\\[\s\S]/)), '"')),
    boolean_literal: _ => choice('true', 'false'),
    null_literal: _ => 'null',
    qualified_identifier: $ => seq($.identifier, repeat(seq(choice('.', '::'), $.identifier))),
    identifier: _ => /[A-Za-z_][A-Za-z0-9_]*/,
    asset_path: _ => /[A-Za-z_][A-Za-z0-9_.-]*(\/[A-Za-z_][A-Za-z0-9_.-]*)+/,

    cpp_block: _ => token(prec(1, choice(
      seq('/$', /([^$]|\$+[^$/])*\$*/, '$/'),
      seq('/%', /([^%]|%+[^%/])*%*/, '%/'),
      seq('/@', /([^@]|@+[^@/])*@*/, '@/'),
    ))),
    csharp_block: _ => token(prec(1, seq(
      '/#', /([^#]|#+[^#/])*#*/, '#/',
    ))),
    preprocessor_directive: _ => token(seq(
      '#', /[ \t]*/, /[A-Za-z_][A-Za-z0-9_]*/, /[^\r\n]*/,
    )),
    comment: _ => token(choice(
      seq('//', /[^\r\n]*/),
      seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/'),
    )),
  },
});
