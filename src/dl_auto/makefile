EXE      := dl_auto
SRCS     := cli_dl_auto_mo.f90 dl_auto_mo.f90 main.f90
DIR_PROJ := auto/
VPATH    := 

include ~/1_Projects/include/exe.mk

run: prep release
	$(DIR_PROJS)$(DIR_PROJ)bin/Release/$(EXE) \
	--config $(DIR_PROJS)$(DIR_PROJ)par/config.nml \
	--date_fr  '2006-10-01' \
	--date_to  '2006-10-31'
