program example
   use callpy_mod
   implicit none

   real(8), allocatable :: a(:, :)
   real(8), allocatable :: b(:, :)
   real(8), allocatable :: c(:, :)
   integer, parameter :: n = 10000
   character(len=*), parameter :: py_module = "multiply"

   logical(1) :: bo

   allocate (a(n, n), b(n, n), c(n, n))

   a = 3.0
   b = 2.0
   c = 0.0

   call set_state("a", a)
   call set_state("b", b)
   call set_state("c", c)

   bo = .True.

   call set_state("bo", bo)

   call call_function(py_module, "function")

   call get_state("bo", bo)
   call get_state("c", c)

   call call_function(py_module, "function")

   write (*, *) c(10000, 10000)
   write (*, *) bo

end program example
