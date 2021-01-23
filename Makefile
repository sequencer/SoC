compile:
	mill -i __.compile

bsp:
	mill -i mill.bsp.BSP/install

clean:
	git clean -fd

reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat 

sbtCompile:
	sbt compile
