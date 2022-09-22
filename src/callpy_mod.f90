! module for calling python from C
module callpy_mod
  use, intrinsic :: iso_c_binding
  implicit none

  private

  interface
     function set_state_py(tag, dtype, t, nx, ny, nz) result(y) bind(c)
       use iso_c_binding
       character(kind=c_char,len=1), intent(in) :: tag(*) 
       character(kind=c_char,len=1), intent(in) :: dtype(*)               
       type(c_ptr), intent(in), value           :: t
       integer(kind=c_int) :: nx, ny, nz
       integer(c_int) :: y
     end function set_state_py

    function get_state_py(tag, dtype, t, n) result(y) bind(c)
      use iso_c_binding
       character(kind=c_char,len=1), intent(in) :: tag(*)  
       character(kind=c_char,len=1), intent(in) :: dtype(*)           
       type(c_ptr),  value           :: t
       integer(c_int) :: n, y
    end function get_state_py
  end interface

  interface set_state
     module procedure set_state_double_3d
     module procedure set_state_double_2d
     module procedure set_state_double_1d
     module procedure set_state_float_3d
     module procedure set_state_float_2d
     module procedure set_state_float_1d
     module procedure set_state_integer_1d
     module procedure set_state_integer_2d      
     module procedure set_state_char
  end interface

  interface get_state        
    module procedure get_state_float_3d
    module procedure get_state_float_2d
    module procedure get_state_float_1d
    module procedure get_state_double_3d
    module procedure get_state_double_2d
    module procedure get_state_double_1d
    module procedure get_state_integer_1d
    module procedure get_state_integer_2d      
    module procedure get_state_char
  end interface

  public :: set_state, call_function, get_state, set_state_scalar


contains

  subroutine call_function(module_name, function_name)
    interface
       function call_function_py(mod_name_c, fun_name_c) &
            result(y) bind(c, name='call_function')
         use iso_c_binding
         character(kind=c_char) mod_name_c, fun_name_c
         integer(c_int) :: y
       end function call_function_py
    end interface

    character(len=*) :: module_name, function_name
    character(kind=c_char, len=256) :: mod_name_c, fun_name_c

    mod_name_c = trim(module_name)//char(0)
    fun_name_c = trim(function_name)//char(0)

    call check(call_function_py(mod_name_c, fun_name_c))

  end subroutine call_function

  subroutine set_state_integer_1d(tag, t)
    character(kind=c_char, len=*) :: tag
    integer(4), dimension(:), intent(in), target :: t
    integer :: nx, ny, nz

    nx = size(t, 1)
    ny = -1
    nz = -1

    call set_state_integer_base(tag, t, nx, ny, nz)

  end subroutine set_state_integer_1d

  subroutine set_state_integer_2d(tag, t)
    character(kind=c_char, len=*) :: tag
    integer(4), dimension(:, :), intent(in), target :: t
    integer :: nx, ny, nz

    nx = size(t, 1)
    ny = size(t, 2)
    nz = -1

    call set_state_integer_base(tag, t, nx, ny, nz)

  end subroutine set_state_integer_2d

  subroutine set_state_double_1d(tag, t)
    character(kind=c_char, len=*) :: tag
    real(8), dimension(:), intent(in), target :: t
    integer :: nx, ny, nz

    nx = size(t, 1)
    ny = -1
    nz = -1

    call set_state_double_base(tag, t, nx, ny, nz)

  end subroutine set_state_double_1d

  subroutine set_state_float_1d(tag, t)
    character(kind=c_char, len=*) :: tag
    real(4), dimension(:), intent(in), target :: t
    integer :: nx, ny, nz

    nx = size(t, 1)
    ny = -1
    nz = -1

    call set_state_float_base(tag, t, nx, ny, nz)

  end subroutine set_state_float_1d

  subroutine set_state_double_2d(tag, t)
    character(kind=c_char, len=*) :: tag
    real(8), dimension(:, :), intent(in), target :: t
    integer :: nx, ny, nz

    nx = size(t, 1)
    ny = size(t, 2)
    nz = -1

    call set_state_double_base(tag, t, nx, ny, nz)

  end subroutine set_state_double_2d

  subroutine set_state_integer_base(tag, dat, nx, ny, nz)

    integer, intent(in) :: nx, ny, nz
    integer(4), dimension(*), intent(in), target :: dat
    character(kind=c_char,len=*) :: tag
    character(kind=c_char,len=*), parameter :: dtype = "int32_t"

    call check(set_state_py(tag//C_NULL_CHAR, dtype//C_NULL_CHAR, c_loc(dat), nx, ny, nz))

  end subroutine set_state_integer_base

  subroutine set_state_float_base(tag, dat, nx, ny, nz)

    integer, intent(in) :: nx, ny, nz
    real(4), dimension(*), intent(in), target :: dat
    character(kind=c_char,len=*) :: tag
    character(kind=c_char,len=*), parameter :: dtype = "float"

    call check(set_state_py(tag//C_NULL_CHAR, dtype//C_NULL_CHAR, c_loc(dat), nx, ny, nz))

  end subroutine set_state_float_base

  subroutine set_state_double_base(tag, dat, nx, ny, nz)

    integer, intent(in) :: nx, ny, nz
    real(8), dimension(*), intent(in), target :: dat
    character(kind=c_char,len=*) :: tag
    character(kind=c_char,len=*), parameter :: dtype = "double"

    call check(set_state_py(tag//C_NULL_CHAR, dtype//C_NULL_CHAR, c_loc(dat), nx, ny, nz))

  end subroutine set_state_double_base

  subroutine set_state_float_2d(tag, t)
    character(kind=c_char, len=*) :: tag
    real(4), dimension(:, :), intent(in), target :: t
    integer :: nx, ny, nz

    nx = size(t, 1)
    ny = size(t, 2)
    nz = -1

    call set_state_float_base(tag, t, nx, ny, nz)

  end subroutine set_state_float_2d

  subroutine set_state_double_3d(tag, t)
    character(kind=c_char, len=*) :: tag
    real(8), dimension(:, :, :), intent(in), target :: t
    integer :: nx, ny, nz

    nx = size(t, 1)
    ny = size(t, 2)
    nz = size(t, 3)

    call set_state_double_base(tag, t, nx, ny, nz)

  end subroutine set_state_double_3d

  subroutine set_state_float_3d(tag, t)
    character(kind=c_char, len=*) :: tag
    real(4), dimension(:, :, :), intent(in), target :: t
    integer :: nx, ny, nz

    nx = size(t, 1)
    ny = size(t, 2)
    nz = size(t, 3)

    call set_state_float_base(tag, t, nx, ny, nz)

  end subroutine set_state_float_3d

  subroutine set_state_scalar(tag, t)
    character(len=*) :: tag
    real :: t
    real(c_double) :: t_
    character(len=256) :: tag_c
    interface
       function set_state_scalar_py(tag, t) result(y)&
            bind(c, name='set_state_scalar')
         use iso_c_binding
         character(c_char) :: tag
         real(c_double) t
         integer(c_int) :: y
       end function set_state_scalar_py
    end interface

    t_ = t
    tag_c = trim(tag)//char(0)
    call check(set_state_scalar_py(tag_c, t_))
  end subroutine set_state_scalar

  subroutine get_state_double_base(tag, t, n)
    character(kind=c_char,len=*) :: tag
    character(kind=c_char,len=*), parameter :: dtype = "double"
    real(8), dimension(*), target :: t  
    integer, intent(in) :: n

    call check(get_state_py(tag//C_NULL_CHAR, dtype//C_NULL_CHAR, c_loc(t), n))
  end subroutine get_state_double_base

  subroutine get_state_float_base(tag, t, n)
    character(kind=c_char,len=*) :: tag
    character(kind=c_char,len=*), parameter :: dtype = "float"    
    real(4), dimension(*), target :: t  
    integer, intent(in) :: n

    call check(get_state_py(tag//C_NULL_CHAR, dtype//C_NULL_CHAR, c_loc(t), n))
  end subroutine get_state_float_base

  subroutine get_state_integer_base(tag, t, n)
    character(kind=c_char,len=*) :: tag
    character(kind=c_char,len=*), parameter :: dtype = "int32_t"    
    integer(4), dimension(*), target :: t  
    integer, intent(in) :: n

    call check(get_state_py(tag//C_NULL_CHAR, dtype//C_NULL_CHAR, c_loc(t), n))
  end subroutine get_state_integer_base

  subroutine get_state_double_3d(tag, t)
    character(kind=c_char, len=*) :: tag
    real(8), dimension(:, :, :),  target :: t
    integer :: n

    n  = size(t)
    call get_state_double_base(tag, t, n)
  end subroutine get_state_double_3d

  subroutine get_state_double_2d(tag, t)
    character(kind=c_char, len=*) :: tag
    real(8), dimension(:, :),  target :: t
    integer :: n

    n  = size(t)
    call get_state_double_base(tag, t, n)
  end subroutine get_state_double_2d

  subroutine get_state_double_1d(tag, t)
    character(kind=c_char, len=*) :: tag
    real(8), dimension(:),  target :: t
    integer :: n

    n  = size(t)
    call get_state_double_base(tag, t, n)
  end subroutine get_state_double_1d

  subroutine get_state_float_3d(tag, t)
    character(kind=c_char, len=*) :: tag
    real(4), dimension(:, :, :),  target :: t
    integer :: n

    n  = size(t)
    call get_state_float_base(tag, t, n)
  end subroutine get_state_float_3d

  subroutine get_state_float_2d(tag, t)
    character(kind=c_char, len=*) :: tag
    real(4), dimension(:, :),  target :: t
    integer :: n

    n  = size(t)
    call get_state_float_base(tag, t, n)
  end subroutine get_state_float_2d

  subroutine get_state_float_1d(tag, t)
    character(kind=c_char, len=*) :: tag
    real(4), dimension(:),  target :: t
    integer :: n

    n  = size(t)
    call get_state_float_base(tag, t, n)
  end subroutine get_state_float_1d

  subroutine get_state_integer_1d(tag, t)
    character(kind=c_char, len=*) :: tag
    integer(4), dimension(:),  target :: t
    integer :: n

    n  = size(t)
    call get_state_integer_base(tag, t, n)
  end subroutine get_state_integer_1d

  subroutine get_state_integer_2d(tag, t)
    character(kind=c_char, len=*) :: tag
    integer(4), dimension(:, :),  target :: t
    integer :: n

    n  = size(t)
    call get_state_integer_base(tag, t, n)
  end subroutine get_state_integer_2d

  subroutine set_state_char(tag, chr)
    interface
       function set_state_char_py(tag, chr) result(y)&
            bind(c, name='set_state_char')
         use iso_c_binding
         implicit none
         character(c_char) :: tag
         character(c_char) :: chr
         integer(c_int) :: y
       end function set_state_char_py
    end interface
    character(len=*) :: tag, chr
    character(len=256) :: tag_, chr_

    tag_ = trim(tag)//char(0)
    chr_ = trim(chr)//char(0)
    call check(set_state_char_py(tag_, chr_))
  end subroutine set_state_char

  subroutine get_state_char(tag, chr)
    interface
       function get_state_char_py(tag, chr, n) result(y)&
            bind(c, name='get_state_char')
         use iso_c_binding
         implicit none
         character(c_char) :: tag
         character(c_char) :: chr
         integer(c_int) :: y, n
       end function
    end interface
    character(len=*) :: tag, chr
    chr = ""
    call check(get_state_char_py(trim(tag) // char(0), chr, len(chr)))
  end subroutine get_state_char

  subroutine check(ret)
    integer :: ret
    if (ret /= 0) stop -1
  end subroutine check

end module callpy_mod
