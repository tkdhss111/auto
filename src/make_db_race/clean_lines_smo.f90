! Last Updated: 2019-06-24 09:51:45
!===========================================================
! Submodule for cleaning HTML lines
!
! Created by : Hisashi Takeda, Ph.D., 2019-06-24
!===========================================================

submodule (make_db_race_mo) clean_lines_smo

  use string_helpers
  
  implicit none

contains

  module subroutine clean_line_race_conditions (line)

    character(*), intent(inout) :: line

    call clear_tags (line, 'td')

    call string_replace (line, 'ｍ',    '')
    call string_replace (line, '℃',     '')
    call string_replace (line, '％',    '')

    line = adjustl(line)

  end subroutine

  module subroutine clean_line_race_result (line)

    character(*), intent(inout) :: line

    call string_replace (line, ' class="light f16"',        '')
    call string_replace (line, ' class="f8"',               '')
    call string_replace (line, ' class="f16"',              '')
    call string_replace (line, ' class="td_white_center"',  '')
    call string_replace (line, ' class="td_black_center"',  '')
    call string_replace (line, ' class="td_blue_center"',   '')
    call string_replace (line, ' class="td_orange_center"', '')
    call string_replace (line, ' class="td_green_center"',  '')
    call string_replace (line, ' class="td_yellow_center"', '')
    call string_replace (line, ' class="td_pink_center"',   '')
    call string_replace (line, ' class="td_red_center"',    '')
    call string_replace (line, '<font class="bold" color="red">再</font>', '')
    call string_replace (line, '－',                      'NA')
    call string_replace (line, '<td></td>',                '-')
    call string_replace (line, '<td>',                      '')
    call string_replace (line, '</td>',                     '')
    call string_replace (line, '　',                       ' ')

    call clear_tags (line, 'tr')
    call clear_tags (line, 'a')

    line = adjustl(line)

  end subroutine

  module subroutine clean_line_rank_lap (line)

    character(*), intent(inout) :: line

    call clear_tags (line, 'tr')
    call clear_tags (line, 'td')

    line = adjustl(line)

  end subroutine

  module subroutine clean_line_pay (line)

    character(*), intent(inout) :: line

    call clear_tags (line, 'a')
    call clear_tags (line, 'tr')
    call clear_tags (line, 'td')
    call clear_tags (line, 'span')
    call clear_tags (line, 'span')
    call clear_tags (line, 'span')

    call string_replace (line, ',', '')
    call string_replace (line, ' )', '')
    call string_replace (line, '(', '')
    call string_replace (line, '円', '')
    call string_replace (line, '着', '')
    call string_replace (line, '特払い', '')
    call string_replace (line, '無投票', '100')
    !call string_replace (line, '=      ', '')
    !call string_replace (line, '－      ', '')
    !call string_replace (line, '＝', ';')
    !call string_replace (line, '－', ';')

    line = adjustl(line)

  end subroutine

  subroutine clear_tags (line, tag)

    character(*), intent(inout) :: line
    character(*), intent(in)    :: tag
    integer                     :: i_head_to, i_tail_fr
    integer                     :: n

    n = len_trim (tag)

    if ( index ( line, '<'//tag(1:n) ) > 0 ) then

      i_head_to = index( line, '<'//tag(1:n) ) + n
      i_tail_fr = index(line, '>')

      line = line(1:i_head_to)//line( i_tail_fr:len_trim(line) )

    end if

    call string_replace (line, '<'//tag(1:n)//'></'//tag(1:n)//'>', 'NA')
    call string_replace (line, '<'//tag(1:n)//'>',  '')
    call string_replace (line, '</'//tag(1:n)//'>', '')
    call string_replace (line, achar(9), '') ! Remove tab

  end subroutine

end submodule
