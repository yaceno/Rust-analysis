# Cible par défaut
all: rustine

# Règles génériques pour la compilation
%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml
	ocamlc -c $<

# Règles pour rustinelexer et rustineparser
rustineparser.ml rustineparser.mli: rustineparser.mly
	ocamlyacc rustineparser.mly

rustinelexer.ml: rustinelexer.mll rustineparser.ml
	ocamllex rustinelexer.mll
	ocamlc -c rustineparser.mli

# Compilation des modules

types.cmo: types.ml
	ocamlc -c types.ml

ast.cmo: ast.ml types.cmo
	ocamlc -c ast.ml

rustineparser.cmo: rustineparser.ml ast.cmo types.cmo
	ocamlc -c rustineparser.ml

rustinelexer.cmo: rustinelexer.ml rustineparser.cmo ast.cmo types.cmo
	ocamlc -c rustinelexer.ml

main.cmo: main.ml ast.cmo rustineparser.cmo rustinelexer.cmo types.cmo
	ocamlc -c main.ml

# Compilation de l'exécutable
rustine: types.cmo ast.cmo rustinelexer.cmo rustineparser.cmo main.cmo
	ocamlc -o rustine types.cmo ast.cmo rustinelexer.cmo rustineparser.cmo main.cmo

# Nettoyage des fichiers générés
clean:
	rm -f rustinelexer.ml rustineparser.ml rustineparser.mli *.cmo *.cmi rustine

.PHONY: all clean
