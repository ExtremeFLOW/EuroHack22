program axbench
  use neko
  use ax_bk5
  implicit none

  character(len=NEKO_FNAME_LEN) :: fname, lxchar
  type(mesh_t) :: msh
  type(file_t) :: nmsh_file
  type(space_t) :: Xh
  type(coef_t) :: c_Xh
  type(dofmap_t) :: dm_Xh
  type(gs_t) :: gs_Xh
  type(field_t) :: w, u
  type(ax_bk5_t) :: bk5_kernel
  integer :: argc, lx, m, n, n_glb, niter, ierr
  character(len=80) :: suffix
  integer :: i

  argc = command_argument_count()

  if ((argc .lt. 2) .or. (argc .gt. 2)) then
     write(*,*) 'Usage: ./bk5 <neko mesh> <N>'
     stop
  end if

  call neko_init

  call job_info

  call get_command_argument(1, fname)
  call get_command_argument(2, lxchar)
  read(lxchar, *) lx

  nmsh_file = file_t(fname)
  call nmsh_file%read(msh)  

  call space_init(Xh, GLL, lx, lx, lx)

  dm_Xh = dofmap_t(msh, Xh)

  call gs_init(gs_Xh, dm_Xh)

  call coef_init(c_Xh, gs_Xh)

  call field_init(w, dm_Xh, "w")
  call field_init(u, dm_Xh, "u")

  niter = 100000
  n = Xh%lx * Xh%ly * Xh%lz

  call set_data(u%x, w%x, dm_Xh%n_dofs)

  call bk5_kernel%compute(w%x, u%x, c_Xh, msh, Xh)

  n_glb = n * msh%glb_nelv

  call neko_log%section('Begin BK5 Run')
  call set_timer_flop_cnt(0, msh%glb_nelv, Xh%lx, niter, n_glb)
  do i = 1, niter
     call bk5_kernel%compute(w%x, u%x, c_Xh, msh, Xh)
  end do
  call set_timer_flop_cnt(1, msh%glb_nelv, Xh%lx, niter, n_glb)
 

  call field_free(u)
  call field_free(w)
  call coef_free(c_Xh)
  call gs_free(gs_Xh)
  call space_free(Xh)   
  call mesh_free(msh)

  call neko_finalize

end program axbench

subroutine set_timer_flop_cnt(iset, nelt, nx1, niter, n)
  use comm
  use num_types
  use logger
  implicit none

  integer :: iset
  integer, intent(inout) :: nelt
  integer, intent(inout) :: nx1
  integer, intent(inout) :: niter
  integer, intent(inout) :: n
  real(kind=dp), save :: time0, time1, gflops, flop_a
  integer :: ierr  
  real(kind=dp) :: nxyz, nx
  character(len=LOG_SIZE) :: log_buf

  nx = dble(nx1)
  nxyz = dble(nx1 * nx1 * nx1)
  call MPI_Barrier(NEKO_COMM, ierr)  
  if (iset .eq. 0) then
     time0 = MPI_Wtime()
  else
     time1 = MPI_Wtime()
     time1 = time1-time0
     flop_a = (15d0 * nxyz + 12d0 * nx * nxyz) * dble(nelt) * dble(niter)
     if (time1 .gt. 0) gflops = (flop_a)/(1.d9*time1)
     call neko_log%end_section('Done!') ! Close BK5 run section
     call neko_log%section("BK5 Result")
     write(log_buf, '(A,1pE12.4)') 'GFlops  :', gflops
     call neko_log%message(log_buf)
     write(log_buf, '(A, I7)')   'Iters   : ', niter
     call neko_log%message(log_buf)
     if (nx1 .lt. 10) then        
        write(log_buf, '(A, I1)') 'lx      :   ', nx1
     else if (nx1 .ge. 10) then
        write(log_buf, '(A, I2)') 'lx      :  ', nx1
     else
        write(log_buf, '(A, I3)') 'lx      :  ', nx1
     end if
     call neko_log%message(log_buf)
     call neko_log%end_section()
  endif

end subroutine set_timer_flop_cnt

subroutine set_data(u, v, n)
  use num_types
  use device
  use, intrinsic :: iso_c_binding
  implicit none

  real(kind=dp), intent(inout), dimension(n) :: u
  real(kind=dp), intent(inout), dimension(n) :: v
  integer,  intent(inout) :: n
  real(kind=dp) :: arg
  integer :: i
  type(c_ptr) :: v_d, u_d

  v_d = device_get_ptr(v)
  u_d = device_get_ptr(u)

  do i = 1, n
     arg = (i * i)
     arg =  cos(arg)
     u(i) = sin(arg)
     v(i) = sin(arg)
  end do

  call device_memcpy(u, u_d, n, HOST_TO_DEVICE)
  call device_memcpy(v, v_d, n, HOST_TO_DEVICE)

end subroutine set_data


subroutine job_info()
  use neko_config
  use device
  use logger
  use comm
  implicit none
  
  character(len=LOG_SIZE) :: log_buf
  
  call neko_log%section("Job Information")
  write(log_buf, '(a)') 'Running on: '
  if (pe_size .lt. 1e1)  then
     write(log_buf(13:), '(i1,a)') pe_size, ' MPI '
     if (pe_size .eq. 1) then
        write(log_buf(19:), '(a)') 'rank'
     else
        write(log_buf(19:), '(a)') 'ranks'
     end if
  else if (pe_size .lt. 1e2) then
     write(log_buf(13:), '(i2,a)') pe_size, ' MPI ranks'
  else if (pe_size .lt. 1e3) then
     write(log_buf(13:), '(i3,a)') pe_size, ' MPI ranks'
  else if (pe_size .lt. 1e4) then
     write(log_buf(13:), '(i4,a)') pe_size, ' MPI ranks'
  else if (pe_size .lt. 1e5) then
     write(log_buf(13:), '(i5,a)') pe_size, ' MPI ranks'
  else
     write(log_buf(13:), '(i6,a)') pe_size, ' MPI ranks'
  end if
  call neko_log%message(log_buf)

  write(log_buf, '(a)') 'Bcknd type: '
  if (NEKO_BCKND_SX .eq. 1) then
     write(log_buf(13:), '(a)') 'SX-Aurora'
  else if (NEKO_BCKND_XSMM .eq. 1) then
     write(log_buf(13:), '(a)') 'CPU (libxsmm)'
  else if (NEKO_BCKND_CUDA .eq. 1) then
     write(log_buf(13:), '(a)') 'Accelerator (CUDA)'
  else if (NEKO_BCKND_HIP .eq. 1) then
     write(log_buf(13:), '(a)') 'Accelerator (HIP)'
  else if (NEKO_BCKND_OPENCL .eq. 1) then
     write(log_buf(13:), '(a)') 'Accelerator (OpenCL)'
  else
     write(log_buf(13:), '(a)') 'CPU'
  end if
  call neko_log%message(log_buf)

  if (NEKO_BCKND_HIP .eq. 1 .or. NEKO_BCKND_CUDA .eq. 1 .or. &
       NEKO_BCKND_OPENCL .eq. 1) then
     write(log_buf, '(a)') 'Dev. name : '
     call device_name(log_buf(13:))
     call neko_log%message(log_buf)
  end if

  write(log_buf, '(a)') 'Real type : '
  select case (rp)
  case (real32)
     write(log_buf(13:), '(a)') 'single precision'
  case (real64)
     write(log_buf(13:), '(a)') 'double precision'
  case (real128)
     write(log_buf(13:), '(a)') 'quad precision'
  end select
  call neko_log%message(log_buf)

  call neko_log%end()

end subroutine job_info
