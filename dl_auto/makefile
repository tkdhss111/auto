EXE      := dl_auto
SRCS     := cli_dl_auto_mo.f90 main.f90
DIR_PROJ := auto/
VPATH    := 

include ~/1_Projects/include/exe.mk

run: prep release
	$(DIR_PROJS)$(DIR_PROJ)bin/Release/$(EXE) \
	--config $(DIR_PROJS)$(DIR_PROJ)par/config.nml
