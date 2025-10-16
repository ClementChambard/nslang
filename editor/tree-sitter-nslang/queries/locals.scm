(fn_decl name: (ident) @local.definition.function)
(param_decl name: (ident) @local.definition.parameter)
(var_decl name: (ident) @local.definition.var)
(field_decl name: (ident) @local.definition.field)
(enum_variant_decl name: (ident) @local.definition.var)
(enum_decl name: (ident) @local.definition.type)
(struct_decl name: (ident) @local.definition.type)
(type_decl name: (ident) @local.definition.type)

(ident) @local.reference
(member_expr field: ((ident) @local.reference (#set! reference.kind "field")))
(type name: ((ident) @local.reference (#set! reference.kind "type")))

[
  (if_stmt)
  (while_stmt)
  (do_stmt)
  (switch_stmt)
  (compound_stmt)
  (source_file)
  (fn_decl)
] @local.scope

; TODO: locals for method decl and call
