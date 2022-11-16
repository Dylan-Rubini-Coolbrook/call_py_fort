program example
   use callpy_mod
   implicit none

   real(4), allocatable :: a(:, :)
   real(4), allocatable :: b(:, :)
   real(4), allocatable :: c(:, :), d(:, :), e(:, :)
   integer, parameter :: n = 5000
   character(len=*), parameter :: py_module = "multiply"

   logical(1) :: bo
   real :: start, finish

   allocate (a(n, n), b(n, n), c(n, n))
   allocate (d(n, n), e(n, n))

   a = 3.0
   b = 2.0
   c = 0.0
   d = 0.0
   e = 0.0

   call set_state("a", a)
   call set_state("b", b)
   call set_state("c", c)

   ! call set_state("d", d)
   ! call set_state("e", e)

   call call_function(py_module, "function")

   call get_state("c", c)
   call get_state("d", d)
   call get_state("e", e)

   print *, "check e = ", e(1, 6)

end program example
