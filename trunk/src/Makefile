SHELL=/bin/bash
.SUFFIXES: .ml .mli .cmo .cmi .cmx

PREFIX=/usr/local

OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCAMLDEP=ocamldep
OCAMLDOC=ocamldoc

OCAMLLIB= $(shell $(OCAMLC) -where)

INCLUDES= -I misc -I cfeval -I fnflib

OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

.PHONY : all
all : cf fnf doc

.PHONY : doc
doc :
	$(OCAMLDOC) -html $(INCLUDES) -d doc \
    misc/*.ml      misc/*.mli   \
    cfeval/*.ml    cfeval/*.mli \
    fnflib/*.ml    fnflib/*.mli

check: cf
	cf -test

.PHONY : install
install : all
	install -D cf  $(PREFIX)/bin/cf
	install -D fnf $(PREFIX)/bin/fnf

.PHONY : uninstall
uninstall :
	-rm $(PREFIX)/bin/cf
	-rm $(PREFIX)/bin/fnf

CF_CMX=\
  $(OCAMLLIB)/unix.cmxa          \
  $(OCAMLLIB)/nums.cmxa          \
  misc/ut.cmx                    \
  misc/list2.cmx                 \
  misc/string2.cmx               \
  misc/intbig.cmx                \
  misc/loc.cmx                   \
  misc/report.cmx                \
  misc/version.cmx               \
  fnflib/fnf_core.cmx            \
  fnflib/fnf_out.cmx             \
  cfeval/cf_fnf.cmx              \
  cfeval/cfAst.cmx               \
  cfeval/cfParserUtil.cmx        \
  cfeval/cfParser.cmx            \
  cfeval/cfLexer.cmx             \
  cfeval/cfTypes.cmx             \
  cfeval/cfPrims.cmx             \
  cfeval/cfCompiler.cmx          \
  misc/test_suite.cmx            \
  cfeval/cf.cmx

FNF_CMX=\
  misc/ut.cmx              \
  misc/string2.cmx         \
  misc/version.cmx         \
  fnflib/parser_util.cmx   \
  fnflib/fnf_core.cmx      \
  fnflib/fnf_parser.cmx    \
  fnflib/fnf_lexer.cmx     \
  fnflib/fnf_out.cmx       \
  fnflib/fnf_verilog.cmx   \
  fnflib/fnf_vhdl.cmx      \
  fnflib/fnf_jhdl.cmx      \
  fnflib/fnf_nusmv.cmx     \
  fnflib/fnf_c.cmx         \
  fnflib/ltl.cmx           \
  fnflib/psl_ast.cmx       \
  fnflib/psl_parser.cmx    \
  fnflib/psl_lexer.cmx     \
  fnflib/fnf.cmx

cf : $(CF_CMX)
	$(OCAMLOPT) -o cf $(OCAMLOPTFLAGS) $(CF_CMX)

fnf : $(FNF_CMX)
	$(OCAMLOPT) -o fnf $(OCAMLOPTFLAGS) $(FNF_CMX)

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

clean:
	-rm depend
	-rm cf
	-rm fnf
	-rm cfeval/cfParser.ml
	-rm cfeval/cfParser.mli
	-rm cfeval/cfParser.output
	-rm cfeval/cfLexer.ml
	-rm fnflib/fnf_parser.ml
	-rm fnflib/fnf_parser.mli
	-rm fnflib/fnf_parser.output
	-rm fnflib/fnf_lexer.ml
	-rm fnflib/psl_parser.ml
	-rm fnflib/psl_parser.mli
	-rm fnflib/psl_parser.output
	-rm fnflib/psl_lexer.ml
	-rm fnflib/*.cm[iox] fnflib/*.o
	-rm cfeval/*.cm[iox] cfeval/*.o
	-rm misc/*.cm[iox] misc/*.o
	-rm doc/*.html doc/*.css

depend :                 \
  cfeval/cfParser.ml     \
  cfeval/cfParser.mli    \
  cfeval/cfLexer.ml      \
  fnflib/psl_parser.ml   \
  fnflib/psl_parser.mli  \
  fnflib/psl_lexer.ml    \
  fnflib/fnf_parser.ml   \
  fnflib/fnf_parser.mli  \
  fnflib/fnf_lexer.ml
	$(OCAMLDEP) $(INCLUDES) \
    misc/*.ml      misc/*.mli   \
    cfeval/*.ml    cfeval/*.mli \
    fnflib/*.ml    fnflib/*.mli \
    > depend

include depend

cfeval/cfParser.ml cfeval/cfParser.mli: cfeval/cfParser.mly
	cd cfeval && ocamlyacc -v cfParser.mly

cfeval/cfLexer.ml: cfeval/cfLexer.mll
	cd cfeval && ocamllex cfLexer.mll

fnflib/fnf_parser.ml fnflib/fnf_parser.mli: fnflib/fnf_parser.mly
	cd fnflib && ocamlyacc -v fnf_parser.mly

fnflib/fnf_lexer.ml: fnflib/fnf_lexer.mll
	cd fnflib && ocamllex fnf_lexer.mll

fnflib/psl_parser.ml fnflib/psl_parser.mli: fnflib/psl_parser.mly
	cd fnflib && ocamlyacc -v psl_parser.mly

fnflib/psl_lexer.ml: fnflib/psl_lexer.mll
	cd fnflib && ocamllex psl_lexer.mll

