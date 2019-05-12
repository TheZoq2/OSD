MAIN=top.v
APIO_FILES=apio.ini pins.pcf
BUILD_DIR=build
APIO_BUILD_DIR=apio_build

IVERILOG_FLAGS=-g2012



# All haskell files in the project
hs_files := $(wildcard *.hs)
hs_targets := $(patsubst %.hs, verilog/%/built, $(wildcard *.hs))
hs_include_dir := clash
hs_lincude_files := $(wildcard clash/*.hs)

# All test files in the project
test_files := $(wildcard test/*.v)

# Output VCD files
vcds := $(patsubst test/%.v, output/%.v.vcd, ${test_files})

# Non test verilog files
hdl = $(wildcard hdl/*.v)
test_only_hdl = hdl/test/assert.v

.SECONDEXPANSION:
# All verilog files built by clash
generated_verilogs=$(shell find verilog -name '*.v')
# Verilog files used in both tests and synthetisation
non_test_verilogs=${generated_verilogs} ${hdl}
# All output files
outfiles := $(patsubst test/%.v, bin/%.v.out, ${test_files})

# Prevent output files from being removed automatically
.SECONDARY: $(outfiles)

# Main rule
all: sim

# Build all HS targets
build_hs: $(hs_targets)


# Build HS files, touch a flag to indicate that the file is built
verilog/%/built: %.hs clash/*.hs
	@echo -e "[\033[0;34mclash\033[0m] Building $<"
	@stack exec -- clash --verilog $< -i${hs_include_dir} \
		-fclash-inline-limit=100 \
		-fmax-relevant-binds=10
	@touch $@


# Simulate the design
sim: build_hs $(vcds)

# Simulation executables
bin/%.v.out: test/%.v $(hs_targets) $(non_test_verilogs) $(test_only_hdl)
	echo $(hdl)
	@echo -e "[\033[0;34miverilog\033[0m] building $@"
	@mkdir -p bin
	@iverilog \
		-o ${@} \
		${IVERILOG_FLAGS} \
		-DVCD_OUTPUT=\"output/${<F}.vcd\" \
		${test_only_hdl} ${non_test_verilogs} $<

# Simulation results
output/%.v.vcd: bin/%.v.out FORCE
	@mkdir -p output
	@echo -e "[\033[0;34mvvp\033[0m] simulating $<"
	@vvp $< | grep -v dumpfile


# Synthetisise for hardware
build: build_hs
	@mkdir -p ${APIO_BUILD_DIR}
	@cp ${hdl} ${non_test_verilogs} ${APIO_FILES} ${APIO_BUILD_DIR}
	@echo -e "[\033[0;34mapio\033[0m] building"
	@apio build -p ${APIO_BUILD_DIR}

# Upload to tinyfpga
upload: build
	@echo -e "[\033[0;34mapio\033[0m] uploading"
	@./upload_until_done.sh ${APIO_BUILD_DIR}

u: upload
b: build

time: build
	@apio time -p ${APIO_BUILD_DIR}


clean:
	rm verilog -rf
	rm output -rf
	rm bin -rf
	rm ${APIO_BUILD_DIR} -rf
	rm ${BUILD_DIR} -rf


# Dummy directive used to force recompilation of all tests
FORCE:

# Builds an iverlog command file with all build options that can be passed to linters
iverilog_commandfile: build_hs
	@echo -e $(patsubst %, '-l %\n', ${non_test_verilogs}) > .verilog_config

