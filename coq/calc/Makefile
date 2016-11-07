TARGET=calc
all:
	coqc $(TARGET).v
	rm -rf $(TARGET).mli
	ocamlc -o $(TARGET) $(TARGET).ml main.ml
	./$(TARGET)

calc1:
	ocaml calc1.ml

clean:
	rm -rf *.glob *.vo .*.aux $(TARGET).mli $(TARGET).ml *.cm* $(TARGET)





