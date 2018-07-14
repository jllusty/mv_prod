# Makefile for Matrix-Vector Product
# by: John Luke Lusty

# Shell
SHELL:=/bin/bash

# Compilation Parameters
# Compiler
FC90 = mpif90
# Files to Include
main_files = src/const_mod.f90 src/main.f90

# all targets
all: mv_prod

# --- Targets ---
# -- 0D Case --
# - Scheme 1 -
mv_prod:$(main_files)
	module purge; \
	module load impi-intel/2017.0.5; \
	module load mkl/2017.0.5; \
	$(FC90) -I${MKLROOT}/include ${GFORT_LINK_THR} $(main_files) -Jobj -o bin/$@_exe

.PHONY: clean

# Clean
clean:
	@rm bin/*_exe obj/*.mod 2>/dev/null || true
