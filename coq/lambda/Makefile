TARGET=lambda
all:
	coqc $(TARGET).v
	rm -rf $(TARGET).mli
	ocamlc -o $(TARGET) $(TARGET).ml main.ml
	./$(TARGET)

clean1:
	rm -rf *.glob *.vo .*.aux $(TARGET).mli *.cm* $(TARGET)

clean:
	rm -rf *.glob *.vo .*.aux $(TARGET).mli $(TARGET).ml *.cm* $(TARGET)





