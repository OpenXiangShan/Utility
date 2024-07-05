RTL_OPT = --full-stacktrace -td build --target systemverilog --module $(MOD)

init:
	git submodule update --init
	cd rocket-chip && git submodule update --init hardfloat cde

compile:
	mill -i utility.compile

rtl:
	@mkdir -p build
	mill -i xsutils.test.runMain top.TestTop $(RTL_OPT)

idea:
	mill -i mill.scalalib.GenIdea/idea

clean:
	rm -rf ./build

reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat
