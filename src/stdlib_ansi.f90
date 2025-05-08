! SPDX-Identifier: MIT

!> Terminal color and style escape sequences
module stdlib_ansi
    use stdlib_kinds, only : i1 => int8
    use stdlib_string_type, only : string_type
    implicit none
    private

    public :: ansi_code
    public :: style_reset, style_bold, style_dim, style_italic, style_underline, &
        & style_blink, style_blink_fast, style_reverse, style_hidden, style_strikethrough
    public :: fg_color_black, fg_color_red, fg_color_green, fg_color_yellow, fg_color_blue, &
        & fg_color_magenta, fg_color_cyan, fg_color_white, fg_color_default
    public :: bg_color_black, bg_color_red, bg_color_green, bg_color_yellow, bg_color_blue, &
        & bg_color_magenta, bg_color_cyan, bg_color_white, bg_color_default

    public :: to_string, operator(+), operator(//)

    character, parameter :: esc = achar(27), chars(0:9) = &
        ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]


    !> Container for terminal escape code
    type :: ansi_code
        private
        !> Style descriptor
        integer(i1) :: style = -1_i1
        !> Background color descriptor
        integer(i1) :: bg = -1_i1
        !> Foreground color descriptor
        integer(i1) :: fg = -1_i1
    end type ansi_code


    !> Identifier for reset style
    type(ansi_code) :: style_reset = ansi_code(style=0)
    !> Identifier for bold style
    type(ansi_code) :: style_bold = ansi_code(style=1)
    !> Identifier for dim style
    type(ansi_code) :: style_dim = ansi_code(style=2)
    !> Identifier for italic style
    type(ansi_code) :: style_italic = ansi_code(style=3)
    !> Identifier for underline style
    type(ansi_code) :: style_underline = ansi_code(style=4)
    !> Identifier for blink style
    type(ansi_code) :: style_blink = ansi_code(style=5)
    !> Identifier for (fast) blink style
    type(ansi_code) :: style_blink_fast = ansi_code(style=6)
    !> Identifier for reverse style
    type(ansi_code) :: style_reverse = ansi_code(style=7)
    !> Identifier for hidden style
    type(ansi_code) :: style_hidden = ansi_code(style=8)
    !> Identifier for strikethrough style
    type(ansi_code) :: style_strikethrough = ansi_code(style=9)

    !> Identifier for black foreground color
    type(ansi_code) :: fg_color_black = ansi_code(fg=0)
    !> Identifier for red foreground color
    type(ansi_code) :: fg_color_red = ansi_code(fg=1)
    !> Identifier for green foreground color
    type(ansi_code) :: fg_color_green = ansi_code(fg=2)
    !> Identifier for yellow foreground color
    type(ansi_code) :: fg_color_yellow = ansi_code(fg=3)
    !> Identifier for blue foreground color
    type(ansi_code) :: fg_color_blue = ansi_code(fg=4)
    !> Identifier for magenta foreground color
    type(ansi_code) :: fg_color_magenta = ansi_code(fg=5)
    !> Identifier for cyan foreground color
    type(ansi_code) :: fg_color_cyan = ansi_code(fg=6)
    !> Identifier for white foreground color
    type(ansi_code) :: fg_color_white = ansi_code(fg=7)
    !> Identifier for the default foreground color
    type(ansi_code) :: fg_color_default = ansi_code(fg=9)

    !> Identifier for black background color
    type(ansi_code) :: bg_color_black = ansi_code(bg=0)
    !> Identifier for red background color
    type(ansi_code) :: bg_color_red = ansi_code(bg=1)
    !> Identifier for green background color
    type(ansi_code) :: bg_color_green = ansi_code(bg=2)
    !> Identifier for yellow background color
    type(ansi_code) :: bg_color_yellow = ansi_code(bg=3)
    !> Identifier for blue background color
    type(ansi_code) :: bg_color_blue = ansi_code(bg=4)
    !> Identifier for magenta background color
    type(ansi_code) :: bg_color_magenta = ansi_code(bg=5)
    !> Identifier for cyan background color
    type(ansi_code) :: bg_color_cyan = ansi_code(bg=6)
    !> Identifier for white background color
    type(ansi_code) :: bg_color_white = ansi_code(bg=7)
    !> Identifier for the default background color
    type(ansi_code) :: bg_color_default = ansi_code(bg=9)


    interface to_string
        !> Transform a color code into an actual ANSI escape sequence
        pure module function to_string_ansi_code(code) result(str)
            !> Color code to be used
            type(ansi_code), intent(in) :: code
            !> ANSI escape sequence representing the color code
            character(len=:), allocatable :: str
        end function to_string_ansi_code
    end interface to_string


    interface operator(+)
        !> Add two escape sequences, attributes in the right value override the left value ones.
        pure module function add(lval, rval) result(code)
            !> First escape code
            type(ansi_code), intent(in) :: lval
            !> Second escape code
            type(ansi_code), intent(in) :: rval
            !> Combined escape code
            type(ansi_code) :: code
        end function add
    end interface operator(+)

    interface operator(//)
        !> Concatenate an escape code with a string and turn it into an actual escape sequence
        pure module function concat_left(lval, code) result(str)
            !> String to add the escape code to
            character(len=*), intent(in) :: lval
            !> Escape sequence
            type(ansi_code), intent(in) :: code
            !> Concatenated string
            character(len=:), allocatable :: str
        end function concat_left

        !> Concatenate an escape code with a string and turn it into an actual escape sequence
        pure module function concat_right(code, rval) result(str)
            !> String to add the escape code to
            character(len=*), intent(in) :: rval
            !> Escape sequence
            type(ansi_code), intent(in) :: code
            !> Concatenated string
            character(len=:), allocatable :: str
        end function concat_right

        !> Concatenate an escape code with a string and turn it into an actual escape sequence
        pure module function concat_left_str(lval, code) result(str)
            !> String to add the escape code to
            type(string_type), intent(in) :: lval
            !> Escape sequence
            type(ansi_code), intent(in) :: code
            !> Concatenated string
            type(string_type) :: str
        end function concat_left_str

        !> Concatenate an escape code with a string and turn it into an actual escape sequence
        pure module function concat_right_str(code, rval) result(str)
            !> String to add the escape code to
            type(string_type), intent(in) :: rval
            !> Escape sequence
            type(ansi_code), intent(in) :: code
            !> Concatenated string
            type(string_type) :: str
        end function concat_right_str
    end interface operator(//)

contains

    !> Transform a color code into an actual ANSI escape sequence
    pure function to_string_ansi_code(code) result(str)
        !> Color code to be used
        type(ansi_code), intent(in) :: code
        !> ANSI escape sequence representing the color code
        character(len=:), allocatable :: str

        if (anycolor(code)) then
            str = esc // "[0"  ! Always reset the style
            if (code%style > 0 .and. code%style < 10) str = str // ";" // chars(code%style)
            if (code%fg >= 0 .and. code%fg < 10) str = str // ";3" // chars(code%fg)
            if (code%bg >= 0 .and. code%bg < 10) str = str // ";4" // chars(code%bg)
            str = str // "m"
        else
            str = ""
        end if
    end function to_string_ansi_code

    !> Check whether the code describes any color / style or is just a stub
    pure function anycolor(code)
        !> Escape sequence
        type(ansi_code), intent(in) :: code
        !> Any color / style is active
        logical :: anycolor

        anycolor = code%fg >= 0 .or. code%bg >= 0 .or. code%style >= 0
    end function anycolor

end module stdlib_ansi
