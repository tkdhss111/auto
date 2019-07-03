module cli_mo

  include '/home/eric/1_Projects/include/cli_header.f90'

contains

  subroutine get_args (this, config, date_fr, date_to, mode)

    class(cmd_ty), intent(in)              :: this
    character(*),  intent(inout), optional :: config, date_fr, date_to, mode

    date_time = get_date_time ()

    if (command_argument_count() == 0) call this%print_help

    print '(a)', ''
    print '(a)', '======================================================'
    print '(a)', trim(cmd%title)
    print '(a)', '======================================================'
    print '(a)', 'Command arguments'
    print '(a)', ''

    do while (i_arg <= command_argument_count())

      call get_command_argument (i_arg, arg, status = status)

      select case (adjustl(arg))

        case ('--config')
          call get_value (config, i_arg)

        case ('--date_fr')
          call get_value (date_fr, i_arg)

        case ('--date_to')
          call get_value (date_to, i_arg)

        case ('--mode')
          call get_value (mode, i_arg)

        case ('-v', '--version')
          call this%print_version (date_time)

        case ('-h', '--help')
          call this%print_help

        case ('--time')
          print *, 'Current time: ', date_time

        case ('--idummy')
          idummy = 1
          call get_value (idummy, i_arg)

        case ('--rdummy')
          rdummy = 1.0d0
          call get_value (rdummy, i_arg)

        case ('--ldummy')
          ldummy = .false.
          call get_value (ldummy, i_arg)

        case default
          print '(a, a, /)', 'Unrecognized command-line option: ', arg
          call this%print_help

      end select

      if (status /= 0) print *, 'Error', status, 'on argument', i_arg

      i_arg = i_arg + 1

    end do

    print '(a)', '======================================================'

    call getcwd(cwd)

#ifdef debug
    print *, 'Program name: '//trim(this%exe)
    print *, 'Number of command arguments: ', command_argument_count()
    print *, 'Current working directory: '//trim(cwd)
    print *, ''
#endif

  end subroutine get_args

  include '/home/eric/1_Projects/include/cli_footer.f90'

end module

