! Last Updated: 2019-06-24 09:51:45
!===========================================================
! Submodule for initialization (configuration and days)
!
! Created by : Hisashi Takeda, Ph.D., 2019-06-24
!===========================================================

submodule (dl_auto_mo) init_smo

  implicit none

contains

  module subroutine read_config (cf, cf_nml, is_print)
    
    type(cf_ty)                   :: cf
    character(*), intent(in)      :: cf_nml
    logical, intent(in), optional :: is_print
    integer u

    namelist /config/ cf 

!    print '(a$)', 'Reading configuration file ... '

    open (newunit = u, file = cf_nml, status = 'old')

    read (u, nml = config)

    close (u)

!    print '(a)', 'done'

    if (present (is_print)) then

      if (is_print) then

        print '(a)', 'PLACEk   : '//trim(cf%PLACE   ) 
        print '(a)', 'DIR_HTML : '//trim(cf%DIR_HTML) 
        print '(a)', 'DIR_CSV  : '//trim(cf%DIR_CSV ) 
        print '(a)', 'F_LIC    : '//trim(cf%F_LIC   ) 
        print *,     'MINS_RUN : ', cf%MINS_RUN      
        print *,     'SEC_SLEEP: ', cf%SEC_SLEEP     

      end if

    end if

  end subroutine

  module subroutine construct_days (days, date_fr, date_to)

    type(datetime), intent(out), allocatable :: days(:)
    character(*),   intent(in)               :: date_fr
    character(*),   intent(in)               :: date_to
    type(datetime)                           :: t_fr, t_to
    integer                                  :: ndays

    t_fr = strptime (date_fr//' 00:00:00', "%Y-%m-%d %H:%M:%S")
    t_to = strptime (date_to//' 00:00:00', "%Y-%m-%d %H:%M:%S")
    days = datetimeRange(t_fr, t_to, timedelta(days = 1))
    ndays = size(days)

  end subroutine

end submodule
