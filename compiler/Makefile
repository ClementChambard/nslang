##
# Project Title
#
# @file
# @version 0.1



bin/compiler: src/darray.ns src/opened_file.ns src/main.ns src/tok.ns src/token.ns src/ident_info.ns src/file_lexer.ns src/lexer.ns src/diags.ns
	../nslang -o $@ $^

clean:
	rm bin/*

run: bin/compiler
	bin/compiler
# end
