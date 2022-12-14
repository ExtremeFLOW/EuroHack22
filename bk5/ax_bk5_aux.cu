/*
 Copyright (c) 2021-2022, The Neko Authors
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:

   * Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     disclaimer in the documentation and/or other materials provided
     with the distribution.

   * Neither the name of the authors nor the names of its
     contributors may be used to endorse or promote products derived
     from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdio.h>
#include "ax_bk5_kernel.h"
#include <neko/device_config.h>

/**
 * Check a CUDA return code
 */
void cuda_check(const char *fname, const int line, const cudaError_t err)
{
  if (err != cudaSuccess) {
    fprintf(stderr, "%s in %s:%d \n", cudaGetErrorString(err), fname, line);
    exit(1);
  }						  
}

#define CUDA_CHECK(err) cuda_check(__FILE__, __LINE__, err)

extern "C" {

  /** 
   * Fortran wrapper for device CUDA Ax bk5
   */
  void cuda_ax_bk5(void *w, void *u, void *dx, void *dy, void *dz,
                   void *g11, void *g22, void *g33, void *g12,
                   void *g13, void *g23, int *nelv, int *lx) {

    const dim3 nthrds((*lx), (*lx), 1);
    const dim3 nblcks((*nelv), 1, 1);

#define CASE(LX)                                                                \
    case LX:                                                                    \
      ax_bk5_kernel<real, LX>                                                   \
      <<<nblcks, nthrds>>>((real *) w, (real *) u,                              \
                           (real *) dx, (real *) dy, (real *) dz,               \
                           (real *) g11, (real *) g22, (real *) g33,            \
                           (real *) g12, (real *) g13, (real *) g23);           \
      CUDA_CHECK(cudaGetLastError());                                           \
      break

    switch(*lx) {
      CASE(2);
      CASE(3);
      CASE(4);
      CASE(5);
      CASE(6);
      CASE(7);
      CASE(8);
      CASE(9);
      CASE(10);
      CASE(11);
      CASE(12);
      CASE(13);
      CASE(14);
      CASE(15);
      CASE(16);
    default:
      {
	fprintf(stderr, __FILE__ ": size not supported: %d\n", *lx);
	exit(1);
      }
    }
  }
}
