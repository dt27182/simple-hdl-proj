run:
	sbt "project fsm" "run" 

run-cpu:
	sbt "project cpu" "run 4 2 0" 

clean:
	sbt "project fsm" clean
	sbt "project cpu" clean
	rm -f generated/*.scala
	rm -f emulator/*.cpp
	rm -f emulator/*.h
	rm -f emulator/*.o
	rm -rf target
	rm -f emulator/Hello
	rm *.vpd
	rm *.vcd
