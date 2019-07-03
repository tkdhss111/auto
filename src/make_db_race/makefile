EXE      := make_db_race
SRCS     := cli_make_db_race_mo.f90 make_db_race_mo.f90 init_smo.f90 set_get_url_smo.f90 clean_lines_smo.f90 extract_tables_smo.f90 write_csv_files_smo.f90 main.f90
DIR_PROJ := auto/
VPATH    := 

include ~/1_Projects/include/exe.mk

run: prep release
	$(DIR_PROJS)$(DIR_PROJ)bin/Release/$(EXE) \
	--config $(DIR_PROJS)$(DIR_PROJ)par/config.nml \
	--mode 'nonexist' \
	--date_fr  '2017-10-01' \
	--date_to  '2019-06-20'

funabashi: prep release
	$(DIR_PROJS)$(DIR_PROJ)bin/Release/$(EXE) \
	--config $(DIR_PROJS)$(DIR_PROJ)par/config_funabashi.nml \
	--mode 'nonexist' \
	--date_fr  '2006-10-01' \
	--date_to  '2016-03-30'
