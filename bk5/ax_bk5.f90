module ax_bk5
  use ax_product
  use, intrinsic :: iso_c_binding
  implicit none
  
  type, public, extends(ax_t) :: ax_bk5_t
   contains
     procedure, nopass :: compute => ax_bk5_compute
  end type ax_bk5_t

  interface
     subroutine ax_bk5_kernel(w, u, g1, g2, g3, g4, g5, g6, dx, dy, dz, nelv, lx) &
          bind(c, name='cuda_ax_bk5')
       use, intrinsic :: iso_c_binding
       implicit none
       type(c_ptr), value :: w, u, g1, g2, g3, g4, g5, g6, dx, dy, dz
       integer(c_int) :: nelv, lx
     end subroutine ax_bk5_kernel
  end interface

contains

  subroutine ax_bk5_compute(w, u, coef, msh, Xh)
    type(mesh_t), intent(inout) :: msh
    type(space_t), intent(inout) :: Xh
    type(coef_t), intent(inout) :: coef
    real(kind=rp), intent(inout) :: w(Xh%lx, Xh%ly, Xh%lz, msh%nelv)
    real(kind=rp), intent(inout) :: u(Xh%lx, Xh%ly, Xh%lz, msh%nelv)
    type(c_ptr) :: w_d, u_d

    w_d = device_get_ptr(w)
    u_d = device_get_ptr(u)

    call ax_bk5_kernel(w_d, u_d, &
         coef%g11_d, coef%g22_d, coef%g33_d, &
         coef%g12_d, coef%g13_d, coef%g23_d, &
         Xh%dx_d, Xh%dy_d, Xh%dz_d, msh%nelv, Xh%lx)

  end subroutine ax_bk5_compute
  
end module ax_bk5
