! Last Updated: 2019-06-16 00:10:32.

program main

  use file_mo
  use cli_mo
  use dl_auto_mo
  use px_mo
  use license_mo

  implicit none

  type(webpage_ty) :: webpage
  type(cf_ty)      :: cf
  character(19)    :: date_fr, date_to, mode
  character(255)   :: cf_nml
  integer          :: year, mon, day, rd

  cmd%title    = 'Program for downloading auto data as HTML'
  cmd%exe      = 'dl_auto'
  cmd%version  = '1.0'                                                                  ;i=i+1
  cmd%usage(i) = '====================================================================' ;i=i+1
  cmd%usage(i) = 'Usage: '//trim(cmd%exe)//' [OPTIONS]'                                 ;i=i+1
  cmd%usage(i) = ''                                                                     ;i=i+1
  cmd%usage(i) = 'Example: '//trim(cmd%exe)//' --cf cf_nml'                             ;i=i+1
  cmd%usage(i) = '                             --date_fr 2016-01-01'                    ;i=i+1
  cmd%usage(i) = '                             --date_to 2017-01-01'                    ;i=i+1
  cmd%usage(i) = '                             --mode "nonexist"'                       ;i=i+1
  cmd%usage(i) = 'Program options:'                                                     ;i=i+1
  cmd%usage(i) = ''                                                                     ;i=i+1
  cmd%usage(i) = '  --cf      followed by path for db-table namelist'                   ;i=i+1
  cmd%usage(i) = '  --date_fr followed by start date in format as yyyy-mm-dd'           ;i=i+1
  cmd%usage(i) = '  --date_to followed by end   date in format as yyyy-mm-dd'           ;i=i+1
  cmd%usage(i) = '  --mode    followed by donwload mode [default:"nonexist"]'           ;i=i+1
  cmd%usage(i) = ''                                                                     ;i=i+1
  cmd%usage(i) = '    [MODE CHOICES]'                                                   ;i=i+1
  cmd%usage(i) = ''                                                                     ;i=i+1
  cmd%usage(i) = '     "full"    : full donwload htmls from date_fr to date_to.'        ;i=i+1
  cmd%usage(i) = ''                                                                     ;i=i+1
  cmd%usage(i) = '     "nonexist": donwload htmls from date_fr to date_to'              ;i=i+1
  cmd%usage(i) = '                 only if the target html file does not exist in local';i=i+1
  cmd%usage(i) = '                 otherwise skipped.'                                  ;i=i+1
  cmd%usage(i) = ''                                                                     ;i=i+1
  cmd%usage(i) = '     "routine" : download htmls every hour+30min only for today'      ;i=i+1
  cmd%usage(i) = '                 and every midnight (i.e. 0:30) for 7 days'           ;i=i+1
  cmd%usage(i) = '                 N.B. date_fr and date_to are ignored.'               ;i=i+1
  cmd%usage(i) = ''                                                                     ;i=i+1
  cmd%usage(i) = '  -v,  --version print version information and exit'                  ;i=i+1
  cmd%usage(i) = '  -h,  --help    print usage information and exit'                    ;i=i+1
  cmd%usage(i) = ''                                                                     ;i=i+1
  cmd%usage(i) = '====================================================================' ;i=i+1
  cmd%n_usage  = i - 1

#ifdef debug
  cf_nml  = '/home/eric/1_Projects/auto/par/config.nml'
  date_fr = '2019-05-12'
  date_to = '2019-05-12'
  mode    = 'nonexist'
  year    = 2018
  mon     = 10
  day     = 31
  rd      = 2
#else
  call cmd%get_args (cf_nml, date_fr, date_to, mode)
#endif

  call read_config (cf, cf_nml)

  call check_license (cf%f_lic)

  call webpage%get_csv_from_html (year     = year,        &
                                  mon      = mon,         &
                                  day      = day,         &
                                  rd       = rd,          &
                                  place    = cf%place,    &
                                  dir_html = cf%dir_html, &
                                  dir_csv  = cf%dir_csv)

end program
