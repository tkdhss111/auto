! Last Updated: 2019-06-24 09:51:45
!===========================================================
! Submodule for cleaning HTML lines
!
! Created by : Hisashi Takeda, Ph.D., 2019-06-24
!===========================================================

submodule (dl_auto_mo) clean_lines_smo

  use string_helpers
  
  implicit none

contains

  module subroutine clean_line_race_conditions (line)

    character(*), intent(inout) :: line

    call string_replace (line, '<td>',  '')
    call string_replace (line, '</td>', '')
    call string_replace (line, 'ｍ',    '')
    call string_replace (line, '℃',     '')
    call string_replace (line, '％',    '')

    line = adjustl(line)

  end subroutine

  module subroutine clean_line_race_result (line)

    character(*), intent(inout) :: line
    integer                     :: i_a_fr, i_a_to

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
    call string_replace (line, '<tr>',                      '')
    call string_replace (line, '</tr>',                     '')
    call string_replace (line, '<td></td>',                '-')
    call string_replace (line, '<td>',                      '')
    call string_replace (line, '</td>',                     '')
    call string_replace (line, '　',                       ' ')
    call string_replace (line, achar(9),                    '')

    if (index(line, '<a') > 0) then

      i_a_fr = index(line, '<a')
      i_a_to = index(line, '>')

      line = line(1:i_a_fr + 1)//line( i_a_to:len_trim(line) )

    end if

    call string_replace (line, '<a></a>', 'NA')
    call string_replace (line, '<a>',  '')
    call string_replace (line, '</a>', '')

    line = adjustl(line)

  end subroutine

  module subroutine clean_line_rank_lap (line)

    character(*), intent(inout) :: line

    call string_replace (line, '<td class="td_orange_center">', '')
    call string_replace (line, '<td class="td_blue_center">',   '')
    call string_replace (line, '<td class="td_black_center">',  '')
    call string_replace (line, '<td class="td_yellow_center">', '')
    call string_replace (line, '<td class="td_green_center">',  '')
    call string_replace (line, '<td class="td_red_center">',    '')
    call string_replace (line, '<td class="td_white_center">',  '')
    call string_replace (line, '<td class="td_pink_center">',   '')
    call string_replace (line, '<tr class="td_white_center">',        '')
    call string_replace (line, '<td class="light txtArea">1周回</td>', '')
    call string_replace (line, '<td class="light txtArea">2周回</td>', '')
    call string_replace (line, '<td class="light txtArea">3周回</td>', '')
    call string_replace (line, '<td class="light txtArea">4周回</td>', '')
    call string_replace (line, '<td class="light txtArea">5周回</td>', '')
    call string_replace (line, '<td class="light txtArea">6周回</td>', '')
    call string_replace (line, '<td class="light txtArea">7周回</td>', '')
    call string_replace (line, '<td class="light txtArea">8周回</td>', '')
    call string_replace (line, '<td class="light txtArea">9周回</td>', '')
    call string_replace (line, '<td class="light txtArea">10周回</td>', '')
    call string_replace (line, '<td class=""></td>', 'NA')
    call string_replace (line, '</tr>', '')
    call string_replace (line, '<td>',  '')
    call string_replace (line, '</td>', '')

    line = adjustl(line)

  end subroutine

end submodule
