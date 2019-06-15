! Last Updated: 2019-06-15 17:30:42.

program main

  use dl_auto_mo

  implicit none

  cmd%title    = 'Program for downloading auto data as HTML'
  cmd%exe      = 'dl_auto'
  cmd%version  = '1.0'                                                                  ;i=i+1
  cmd%usage(i) = '====================================================================' ;i=i+1
  cmd%usage(i) = 'Usage: '//trim(cmd%exe)//' [OPTIONS]'                                 ;i=i+1
  cmd%usage(i) = ''                                                                     ;i=i+1
  cmd%usage(i) = 'Example: '//trim(cmd%exe)//' --config config.nml'                     ;i=i+1
  cmd%usage(i) = '                             --date_fr 2016-01-01'                    ;i=i+1
  cmd%usage(i) = '                             --date_to 2017-01-01'                    ;i=i+1
  cmd%usage(i) = '                             --mode "nonexist"'                       ;i=i+1
  cmd%usage(i) = 'Program options:'                                                     ;i=i+1
  cmd%usage(i) = ''                                                                     ;i=i+1
  cmd%usage(i) = '  --config  followed by path for db-table namelist'                   ;i=i+1
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
  config_nml = '/home/eric/1_Projects/JMA/par/0_com/config.nml'
  date_fr    = '2019-05-12'
  date_to    = '2019-05-12'
  mode       = 'nonexist'
#else
  call cmd%get_args (config_nml, date_fr, date_to, mode)
#endif

  call read_config (cf, config_nml)
  call check_license (cf%f_lic)

  call ca%read_calendar (trim(cf%dir_par_com)//'calendar.bin')
  call construct_sites (sites, file = trim(cf%DIR_PAR_COM)//trim(cf%tb_obstry)//'.csv')

  select case (mode)

    case ("full")

      call get_html (cf, ca, sites, date_fr, date_to, replace = .true.)

    case ("nonexist")

      call get_html (cf, ca, sites, date_fr, date_to, replace = .false.)

    case ("routine")

      do ! Endless loop as daemon

        call ca%get_now

        if (ca%sys%getMinute() == 30) then ! Every hour + 30 minutes 

          if (ca%sys%GetHour() == 0) then ! Every midnight 

            t_7days_ago = ca%sys - timedelta (days = 7)

            call get_html (cf, ca, sites,&
                               date_fr = t_7days_ago%dateformat(),&
                               date_to = ca%today,&
                               replace = .true.)
          else

            call get_html (cf, ca, sites,& 
                               date_fr = ca%today,&
                               date_to = ca%today,&
                               replace = .true.)
          end if

        end if

        call sleep(60)

      end do

    case default

      print '(a)', '**********************************************'
      print '(a)', 'Error: Unrecognized mode choice: '//trim(mode)
      print '(a)', 'Run program with option -h for help'
      print '(a)', '**********************************************'
      stop

  end select

end program
