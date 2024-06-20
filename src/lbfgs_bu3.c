// commented out vecset and vecmul (DA)
//#include <lbfgs.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Lapack.h>
#include <R_ext/BLAS.h>
//
FILE *jp;
FILE *kp;
FILE *lp;

// Global pointers
int *nslice;
int *nburn;
int *nrowX;
int *ncolX;
int *NS;
int *N;
int *NDIM;
int *UNFOLD;
int *NMISSING;
double *X;
double *CONSTRAINTS;
//
/*
 *      C library of Limited memory BFGS (L-BFGS).
 *
 * Copyright (c) 1990, Jorge Nocedal
 * Copyright (c) 2007-2010 Naoaki Okazaki
 * All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

/* $Id$ */

#ifndef __LBFGS_H__
#define __LBFGS_H__

#ifdef  __cplusplus
extern "C" {
#endif/*__cplusplus*/

/*
 * The default precision of floating point values is 64bit (double).
 */
#ifndef LBFGS_FLOAT
#define LBFGS_FLOAT     64
#endif/*LBFGS_FLOAT*/

/*
 * Activate optimization routines for IEEE754 floating point values.
 */
#ifndef LBFGS_IEEE_FLOAT
#define LBFGS_IEEE_FLOAT    1
#endif/*LBFGS_IEEE_FLOAT*/

#if     LBFGS_FLOAT == 32
typedef float lbfgsfloatval_t;

#elif   LBFGS_FLOAT == 64
typedef double lbfgsfloatval_t;

#else
#error "libLBFGS supports single (float; LBFGS_FLOAT = 32) or double (double; LBFGS_FLOAT=64) precision only."

#endif


/** 
 * \addtogroup liblbfgs_api libLBFGS API
 * @{
 *
 *  The libLBFGS API.
 */

/**
 * Return values of lbfgs().
 * 
 *  Roughly speaking, a negative value indicates an error.
 */
enum {
    /** L-BFGS reaches convergence. */
    LBFGS_SUCCESS = 0,
    LBFGS_CONVERGENCE = 0,
    LBFGS_STOP,
    /** The initial variables already minimize the objective function. */
    LBFGS_ALREADY_MINIMIZED,

    /** Unknown error. */
    LBFGSERR_UNKNOWNERROR = -1024,
    /** Logic error. */
    LBFGSERR_LOGICERROR,
    /** Insufficient memory. */
    LBFGSERR_OUTOFMEMORY,
    /** The minimization process has been canceled. */
    LBFGSERR_CANCELED,
    /** Invalid number of variables specified. */
    LBFGSERR_INVALID_N,
    /** Invalid number of variables (for SSE) specified. */
    LBFGSERR_INVALID_N_SSE,
    /** The array x must be aligned to 16 (for SSE). */
    LBFGSERR_INVALID_X_SSE,
    /** Invalid parameter lbfgs_parameter_t::epsilon specified. */
    LBFGSERR_INVALID_EPSILON,
    /** Invalid parameter lbfgs_parameter_t::past specified. */
    LBFGSERR_INVALID_TESTPERIOD,
    /** Invalid parameter lbfgs_parameter_t::delta specified. */
    LBFGSERR_INVALID_DELTA,
    /** Invalid parameter lbfgs_parameter_t::linesearch specified. */
    LBFGSERR_INVALID_LINESEARCH,
    /** Invalid parameter lbfgs_parameter_t::max_step specified. */
    LBFGSERR_INVALID_MINSTEP,
    /** Invalid parameter lbfgs_parameter_t::max_step specified. */
    LBFGSERR_INVALID_MAXSTEP,
    /** Invalid parameter lbfgs_parameter_t::ftol specified. */
    LBFGSERR_INVALID_FTOL,
    /** Invalid parameter lbfgs_parameter_t::wolfe specified. */
    LBFGSERR_INVALID_WOLFE,
    /** Invalid parameter lbfgs_parameter_t::gtol specified. */
    LBFGSERR_INVALID_GTOL,
    /** Invalid parameter lbfgs_parameter_t::xtol specified. */
    LBFGSERR_INVALID_XTOL,
    /** Invalid parameter lbfgs_parameter_t::max_linesearch specified. */
    LBFGSERR_INVALID_MAXLINESEARCH,
    /** Invalid parameter lbfgs_parameter_t::orthantwise_c specified. */
    LBFGSERR_INVALID_ORTHANTWISE,
    /** Invalid parameter lbfgs_parameter_t::orthantwise_start specified. */
    LBFGSERR_INVALID_ORTHANTWISE_START,
    /** Invalid parameter lbfgs_parameter_t::orthantwise_end specified. */
    LBFGSERR_INVALID_ORTHANTWISE_END,
    /** The line-search step went out of the interval of uncertainty. */
    LBFGSERR_OUTOFINTERVAL,
    /** A logic error occurred; alternatively, the interval of uncertainty
        became too small. */
    LBFGSERR_INCORRECT_TMINMAX,
    /** A rounding error occurred; alternatively, no line-search step
        satisfies the sufficient decrease and curvature conditions. */
    LBFGSERR_ROUNDING_ERROR,
    /** The line-search step became smaller than lbfgs_parameter_t::min_step. */
    LBFGSERR_MINIMUMSTEP,
    /** The line-search step became larger than lbfgs_parameter_t::max_step. */
    LBFGSERR_MAXIMUMSTEP,
    /** The line-search routine reaches the maximum number of evaluations. */
    LBFGSERR_MAXIMUMLINESEARCH,
    /** The algorithm routine reaches the maximum number of iterations. */
    LBFGSERR_MAXIMUMITERATION,
    /** Relative width of the interval of uncertainty is at most
        lbfgs_parameter_t::xtol. */
    LBFGSERR_WIDTHTOOSMALL,
    /** A logic error (negative line-search step) occurred. */
    LBFGSERR_INVALIDPARAMETERS,
    /** The current search direction increases the objective function value. */
    LBFGSERR_INCREASEGRADIENT,
};

/**
 * Line search algorithms.
 */
enum {
    /** The default algorithm (MoreThuente method). */
    LBFGS_LINESEARCH_DEFAULT = 0,
    /** MoreThuente method proposd by More and Thuente. */
    LBFGS_LINESEARCH_MORETHUENTE = 0,
    /**
     * Backtracking method with the Armijo condition.
     *  The backtracking method finds the step length such that it satisfies
     *  the sufficient decrease (Armijo) condition,
     *    - f(x + a * d) <= f(x) + lbfgs_parameter_t::ftol * a * g(x)^T d,
     *
     *  where x is the current point, d is the current search direction, and
     *  a is the step length.
     */
    LBFGS_LINESEARCH_BACKTRACKING_ARMIJO = 1,
    /** The backtracking method with the defualt (regular Wolfe) condition. */
    LBFGS_LINESEARCH_BACKTRACKING = 2,
    /**
     * Backtracking method with regular Wolfe condition.
     *  The backtracking method finds the step length such that it satisfies
     *  both the Armijo condition (LBFGS_LINESEARCH_BACKTRACKING_ARMIJO)
     *  and the curvature condition,
     *    - g(x + a * d)^T d >= lbfgs_parameter_t::wolfe * g(x)^T d,
     *
     *  where x is the current point, d is the current search direction, and
     *  a is the step length.
     */
    LBFGS_LINESEARCH_BACKTRACKING_WOLFE = 2,
    /**
     * Backtracking method with strong Wolfe condition.
     *  The backtracking method finds the step length such that it satisfies
     *  both the Armijo condition (LBFGS_LINESEARCH_BACKTRACKING_ARMIJO)
     *  and the following condition,
     *    - |g(x + a * d)^T d| <= lbfgs_parameter_t::wolfe * |g(x)^T d|,
     *
     *  where x is the current point, d is the current search direction, and
     *  a is the step length.
     */
    LBFGS_LINESEARCH_BACKTRACKING_STRONG_WOLFE = 3,
};

/**
 * L-BFGS optimization parameters.
 *  Call lbfgs_parameter_init() function to initialize parameters to the
 *  default values.
 */
typedef struct {
    /**
     * The number of corrections to approximate the inverse hessian matrix.
     *  The L-BFGS routine stores the computation results of previous \ref m
     *  iterations to approximate the inverse hessian matrix of the current
     *  iteration. This parameter controls the size of the limited memories
     *  (corrections). The default value is \c 6. Values less than \c 3 are
     *  not recommended. Large values will result in excessive computing time.
     */
    int             m;

    /**
     * Epsilon for convergence test.
     *  This parameter determines the accuracy with which the solution is to
     *  be found. A minimization terminates when
     *      ||g|| < \ref epsilon * max(1, ||x||),
     *  where ||.|| denotes the Euclidean (L2) norm. The default value is
     *  \c 1e-5.
     */
    lbfgsfloatval_t epsilon;

    /**
     * Distance for delta-based convergence test.
     *  This parameter determines the distance, in iterations, to compute
     *  the rate of decrease of the objective function. If the value of this
     *  parameter is zero, the library does not perform the delta-based
     *  convergence test. The default value is \c 0.
     */
    int             past;

    /**
     * Delta for convergence test.
     *  This parameter determines the minimum rate of decrease of the
     *  objective function. The library stops iterations when the
     *  following condition is met:
     *      (f' - f) / f < \ref delta,
     *  where f' is the objective value of \ref past iterations ago, and f is
     *  the objective value of the current iteration.
     *  The default value is \c 0.
     */
    lbfgsfloatval_t delta;

    /**
     * The maximum number of iterations.
     *  The lbfgs() function terminates an optimization process with
     *  ::LBFGSERR_MAXIMUMITERATION status code when the iteration count
     *  exceedes this parameter. Setting this parameter to zero continues an
     *  optimization process until a convergence or error. The default value
     *  is \c 0.
     */
    int             max_iterations;

    /**
     * The line search algorithm.
     *  This parameter specifies a line search algorithm to be used by the
     *  L-BFGS routine.
     */
    int             linesearch;

    /**
     * The maximum number of trials for the line search.
     *  This parameter controls the number of function and gradients evaluations
     *  per iteration for the line search routine. The default value is \c 20.
     */
    int             max_linesearch;

    /**
     * The minimum step of the line search routine.
     *  The default value is \c 1e-20. This value need not be modified unless
     *  the exponents are too large for the machine being used, or unless the
     *  problem is extremely badly scaled (in which case the exponents should
     *  be increased).
     */
    lbfgsfloatval_t min_step;

    /**
     * The maximum step of the line search.
     *  The default value is \c 1e+20. This value need not be modified unless
     *  the exponents are too large for the machine being used, or unless the
     *  problem is extremely badly scaled (in which case the exponents should
     *  be increased).
     */
    lbfgsfloatval_t max_step;

    /**
     * A parameter to control the accuracy of the line search routine.
     *  The default value is \c 1e-4. This parameter should be greater
     *  than zero and smaller than \c 0.5.
     */
    lbfgsfloatval_t ftol;

    /**
     * A coefficient for the Wolfe condition.
     *  This parameter is valid only when the backtracking line-search
     *  algorithm is used with the Wolfe condition,
     *  ::LBFGS_LINESEARCH_BACKTRACKING_STRONG_WOLFE or
     *  ::LBFGS_LINESEARCH_BACKTRACKING_WOLFE .
     *  The default value is \c 0.9. This parameter should be greater
     *  the \ref ftol parameter and smaller than \c 1.0.
     */
    lbfgsfloatval_t wolfe;

    /**
     * A parameter to control the accuracy of the line search routine.
     *  The default value is \c 0.9. If the function and gradient
     *  evaluations are inexpensive with respect to the cost of the
     *  iteration (which is sometimes the case when solving very large
     *  problems) it may be advantageous to set this parameter to a small
     *  value. A typical small value is \c 0.1. This parameter shuold be
     *  greater than the \ref ftol parameter (\c 1e-4) and smaller than
     *  \c 1.0.
     */
    lbfgsfloatval_t gtol;

    /**
     * The machine precision for floating-point values.
     *  This parameter must be a positive value set by a client program to
     *  estimate the machine precision. The line search routine will terminate
     *  with the status code (::LBFGSERR_ROUNDING_ERROR) if the relative width
     *  of the interval of uncertainty is less than this parameter.
     */
    lbfgsfloatval_t xtol;

    /**
     * Coeefficient for the L1 norm of variables.
     *  This parameter should be set to zero for standard minimization
     *  problems. Setting this parameter to a positive value activates
     *  Orthant-Wise Limited-memory Quasi-Newton (OWL-QN) method, which
     *  minimizes the objective function F(x) combined with the L1 norm |x|
     *  of the variables, {F(x) + C |x|}. This parameter is the coeefficient
     *  for the |x|, i.e., C. As the L1 norm |x| is not differentiable at
     *  zero, the library modifies function and gradient evaluations from
     *  a client program suitably; a client program thus have only to return
     *  the function value F(x) and gradients G(x) as usual. The default value
     *  is zero.
     */
    lbfgsfloatval_t orthantwise_c;

    /**
     * Start index for computing L1 norm of the variables.
     *  This parameter is valid only for OWL-QN method
     *  (i.e., \ref orthantwise_c != 0). This parameter b (0 <= b < N)
     *  specifies the index number from which the library computes the
     *  L1 norm of the variables x,
     *      |x| := |x_{b}| + |x_{b+1}| + ... + |x_{N}| .
     *  In other words, variables x_1, ..., x_{b-1} are not used for
     *  computing the L1 norm. Setting b (0 < b < N), one can protect
     *  variables, x_1, ..., x_{b-1} (e.g., a bias term of logistic
     *  regression) from being regularized. The default value is zero.
     */
    int             orthantwise_start;

    /**
     * End index for computing L1 norm of the variables.
     *  This parameter is valid only for OWL-QN method
     *  (i.e., \ref orthantwise_c != 0). This parameter e (0 < e <= N)
     *  specifies the index number at which the library stops computing the
     *  L1 norm of the variables x,
     */
    int             orthantwise_end;
} lbfgs_parameter_t;


/**
 * Callback interface to provide objective function and gradient evaluations.
 *
 *  The lbfgs() function call this function to obtain the values of objective
 *  function and its gradients when needed. A client program must implement
 *  this function to evaluate the values of the objective function and its
 *  gradients, given current values of variables.
 *  
 *  @param  instance    The user data sent for lbfgs() function by the client.
 *  @param  x           The current values of variables.
 *  @param  g           The gradient vector. The callback function must compute
 *                      the gradient values for the current variables.
 *  @param  n           The number of variables.
 *  @param  step        The current step of the line search routine.
 *  @retval lbfgsfloatval_t The value of the objective function for the current
 *                          variables.
 */
typedef lbfgsfloatval_t (*lbfgs_evaluate_t)(
    void *instance,
    const lbfgsfloatval_t *x,
    lbfgsfloatval_t *g,
    const int n,
    const lbfgsfloatval_t step
    );

/**
 * Callback interface to receive the progress of the optimization process.
 *
 *  The lbfgs() function call this function for each iteration. Implementing
 *  this function, a client program can store or display the current progress
 *  of the optimization process.
 *
 *  @param  instance    The user data sent for lbfgs() function by the client.
 *  @param  x           The current values of variables.
 *  @param  g           The current gradient values of variables.
 *  @param  fx          The current value of the objective function.
 *  @param  xnorm       The Euclidean norm of the variables.
 *  @param  gnorm       The Euclidean norm of the gradients.
 *  @param  step        The line-search step used for this iteration.
 *  @param  n           The number of variables.
 *  @param  k           The iteration count.
 *  @param  ls          The number of evaluations called for this iteration.
 *  @retval int         Zero to continue the optimization process. Returning a
 *                      non-zero value will cancel the optimization process.
 */
typedef int (*lbfgs_progress_t)(
    void *instance,
    const lbfgsfloatval_t *x,
    const lbfgsfloatval_t *g,
    const lbfgsfloatval_t fx,
    const lbfgsfloatval_t xnorm,
    const lbfgsfloatval_t gnorm,
    const lbfgsfloatval_t step,
    int n,
    int k,
    int ls
    );

/*
A user must implement a function compatible with ::lbfgs_evaluate_t (evaluation
callback) and pass the pointer to the callback function to lbfgs() arguments.
Similarly, a user can implement a function compatible with ::lbfgs_progress_t
(progress callback) to obtain the current progress (e.g., variables, function
value, ||G||, etc) and to cancel the iteration process if necessary.
Implementation of a progress callback is optional: a user can pass \c NULL if
progress notification is not necessary.

In addition, a user must preserve two requirements:
    - The number of variables must be multiples of 16 (this is not 4).
    - The memory block of variable array ::x must be aligned to 16.

This algorithm terminates an optimization
when:

    ||G|| < \epsilon \cdot \max(1, ||x||) .

In this formula, ||.|| denotes the Euclidean norm.
*/

/**
 * Start a L-BFGS optimization.
 *
 *  @param  n           The number of variables.
 *  @param  x           The array of variables. A client program can set
 *                      default values for the optimization and receive the
 *                      optimization result through this array. This array
 *                      must be allocated by ::lbfgs_malloc function
 *                      for libLBFGS built with SSE/SSE2 optimization routine
 *                      enabled. The library built without SSE/SSE2
 *                      optimization does not have such a requirement.
 *  @param  ptr_fx      The pointer to the variable that receives the final
 *                      value of the objective function for the variables.
 *                      This argument can be set to \c NULL if the final
 *                      value of the objective function is unnecessary.
 *  @param  proc_evaluate   The callback function to provide function and
 *                          gradient evaluations given a current values of
 *                          variables. A client program must implement a
 *                          callback function compatible with \ref
 *                          lbfgs_evaluate_t and pass the pointer to the
 *                          callback function.
 *  @param  proc_progress   The callback function to receive the progress
 *                          (the number of iterations, the current value of
 *                          the objective function) of the minimization
 *                          process. This argument can be set to \c NULL if
 *                          a progress report is unnecessary.
 *  @param  instance    A user data for the client program. The callback
 *                      functions will receive the value of this argument.
 *  @param  param       The pointer to a structure representing parameters for
 *                      L-BFGS optimization. A client program can set this
 *                      parameter to \c NULL to use the default parameters.
 *                      Call lbfgs_parameter_init() function to fill a
 *                      structure with the default values.
 *  @retval int         The status code. This function returns zero if the
 *                      minimization process terminates without an error. A
 *                      non-zero value indicates an error.
 */
int lbfgs(
    int n,
    lbfgsfloatval_t *x,
    lbfgsfloatval_t *ptr_fx,
    lbfgs_evaluate_t proc_evaluate,
    lbfgs_progress_t proc_progress,
    void *instance,
    lbfgs_parameter_t *param
    );

/**
 * Initialize L-BFGS parameters to the default values.
 *
 *  Call this function to fill a parameter structure with the default values
 *  and overwrite parameter values if necessary.
 *
 *  @param  param       The pointer to the parameter structure.
 */
void lbfgs_parameter_init(lbfgs_parameter_t *param);

/**
 * Allocate an array for variables.
 *
 *  This function allocates an array of variables for the convenience of
 *  ::lbfgs function; the function has a requreiemt for a variable array
 *  when libLBFGS is built with SSE/SSE2 optimization routines. A user does
 *  not have to use this function for libLBFGS built without SSE/SSE2
 *  optimization.
 *  
 *  @param  n           The number of variables.
 */
lbfgsfloatval_t* lbfgs_malloc(int n);

/**
 * Free an array of variables.
 *  
 *  @param  x           The array of variables allocated by ::lbfgs_malloc
 *                      function.
 */
void lbfgs_free(lbfgsfloatval_t *x);

/** @} */

#ifdef  __cplusplus
}
#endif/*__cplusplus*/



/**
@mainpage libLBFGS: a library of Limited-memory Broyden-Fletcher-Goldfarb-Shanno (L-BFGS)

@section intro Introduction

This library is a C port of the implementation of Limited-memory
Broyden-Fletcher-Goldfarb-Shanno (L-BFGS) method written by Jorge Nocedal.
The original FORTRAN source code is available at:
http://www.ece.northwestern.edu/~nocedal/lbfgs.html

The L-BFGS method solves the unconstrainted minimization problem,

<pre>
    minimize F(x), x = (x1, x2, ..., xN),
</pre>

only if the objective function F(x) and its gradient G(x) are computable. The
well-known Newton's method requires computation of the inverse of the hessian
matrix of the objective function. However, the computational cost for the
inverse hessian matrix is expensive especially when the objective function
takes a large number of variables. The L-BFGS method iteratively finds a
minimizer by approximating the inverse hessian matrix by information from last
m iterations. This innovation saves the memory storage and computational time
drastically for large-scaled problems.

Among the various ports of L-BFGS, this library provides several features:
- <b>Optimization with L1-norm (Orthant-Wise Limited-memory Quasi-Newton
  (OWL-QN) method)</b>:
  In addition to standard minimization problems, the library can minimize
  a function F(x) combined with L1-norm |x| of the variables,
  {F(x) + C |x|}, where C is a constant scalar parameter. This feature is
  useful for estimating parameters of sparse log-linear models (e.g.,
  logistic regression and maximum entropy) with L1-regularization (or
  Laplacian prior).
- <b>Clean C code</b>:
  Unlike C codes generated automatically by f2c (Fortran 77 into C converter),
  this port includes changes based on my interpretations, improvements,
  optimizations, and clean-ups so that the ported code would be well-suited
  for a C code. In addition to comments inherited from the original code,
  a number of comments were added through my interpretations.
- <b>Callback interface</b>:
  The library receives function and gradient values via a callback interface.
  The library also notifies the progress of the optimization by invoking a
  callback function. In the original implementation, a user had to set
  function and gradient values every time the function returns for obtaining
  updated values.
- <b>Thread safe</b>:
  The library is thread-safe, which is the secondary gain from the callback
  interface.
- <b>Cross platform.</b> The source code can be compiled on Microsoft Visual
  Studio 2010, GNU C Compiler (gcc), etc.
- <b>Configurable precision</b>: A user can choose single-precision (float)
  or double-precision (double) accuracy by changing ::LBFGS_FLOAT macro.
- <b>SSE/SSE2 optimization</b>:
  This library includes SSE/SSE2 optimization (written in compiler intrinsics)
  for vector arithmetic operations on Intel/AMD processors. The library uses
  SSE for float values and SSE2 for double values. The SSE/SSE2 optimization
  routine is disabled by default.

This library is used by:
- <a href="http://www.chokkan.org/software/crfsuite/">CRFsuite: A fast implementation of Conditional Random Fields (CRFs)</a>
- <a href="http://www.chokkan.org/software/classias/">Classias: A collection of machine-learning algorithms for classification</a>
- <a href="http://www.public.iastate.edu/~gdancik/mlegp/">mlegp: an R package for maximum likelihood estimates for Gaussian processes</a>
- <a href="http://infmath.uibk.ac.at/~matthiasf/imaging2/">imaging2: the imaging2 class library</a>
- <a href="http://search.cpan.org/~laye/Algorithm-LBFGS-0.16/">Algorithm::LBFGS - Perl extension for L-BFGS</a>
- <a href="http://www.cs.kuleuven.be/~bernd/yap-lbfgs/">YAP-LBFGS (an interface to call libLBFGS from YAP Prolog)</a>

@section download Download

- <a href="https://github.com/downloads/chokkan/liblbfgs/liblbfgs-1.10.tar.gz">Source code</a>
- <a href="https://github.com/chokkan/liblbfgs">GitHub repository</a>

libLBFGS is distributed under the term of the
<a href="http://opensource.org/licenses/mit-license.php">MIT license</a>.

@section changelog History
- Version 1.10 (2010-12-22):
    - Fixed compiling errors on Mac OS X; this patch was kindly submitted by
      Nic Schraudolph.
    - Reduced compiling warnings on Mac OS X; this patch was kindly submitted
      by Tamas Nepusz.
    - Replaced memalign() with posix_memalign().
    - Updated solution and project files for Microsoft Visual Studio 2010.
- Version 1.9 (2010-01-29):
    - Fixed a mistake in checking the validity of the parameters "ftol" and
      "wolfe"; this was discovered by Kevin S. Van Horn.
- Version 1.8 (2009-07-13):
    - Accepted the patch submitted by Takashi Imamichi;
      the backtracking method now has three criteria for choosing the step
      length:
        - ::LBFGS_LINESEARCH_BACKTRACKING_ARMIJO: sufficient decrease (Armijo)
          condition only
        - ::LBFGS_LINESEARCH_BACKTRACKING_WOLFE: regular Wolfe condition
          (sufficient decrease condition + curvature condition)
        - ::LBFGS_LINESEARCH_BACKTRACKING_STRONG_WOLFE: strong Wolfe condition
    - Updated the documentation to explain the above three criteria.
- Version 1.7 (2009-02-28):
    - Improved OWL-QN routines for stability.
    - Removed the support of OWL-QN method in MoreThuente algorithm because
      it accidentally fails in early stages of iterations for some objectives.
      Because of this change, <b>the OW-LQN method must be used with the
      backtracking algorithm (::LBFGS_LINESEARCH_BACKTRACKING)</b>, or the
      library returns ::LBFGSERR_INVALID_LINESEARCH.
    - Renamed line search algorithms as follows:
        - ::LBFGS_LINESEARCH_BACKTRACKING: regular Wolfe condition.
        - ::LBFGS_LINESEARCH_BACKTRACKING_LOOSE: regular Wolfe condition.
        - ::LBFGS_LINESEARCH_BACKTRACKING_STRONG: strong Wolfe condition.
    - Source code clean-up.
- Version 1.6 (2008-11-02):
    - Improved line-search algorithm with strong Wolfe condition, which was
      contributed by Takashi Imamichi. This routine is now default for
      ::LBFGS_LINESEARCH_BACKTRACKING. The previous line search algorithm
      with regular Wolfe condition is still available as
      ::LBFGS_LINESEARCH_BACKTRACKING_LOOSE.
    - Configurable stop index for L1-norm computation. A member variable
      ::lbfgs_parameter_t::orthantwise_end was added to specify the index
      number at which the library stops computing the L1 norm of the
      variables. This is useful to prevent some variables from being
      regularized by the OW-LQN method.
    - A sample program written in C++ (sample/sample.cpp).
- Version 1.5 (2008-07-10):
    - Configurable starting index for L1-norm computation. A member variable
      ::lbfgs_parameter_t::orthantwise_start was added to specify the index
      number from which the library computes the L1 norm of the variables.
      This is useful to prevent some variables from being regularized by the
      OWL-QN method.
    - Fixed a zero-division error when the initial variables have already
      been a minimizer (reported by Takashi Imamichi). In this case, the
      library returns ::LBFGS_ALREADY_MINIMIZED status code.
    - Defined ::LBFGS_SUCCESS status code as zero; removed unused constants,
      LBFGSFALSE and LBFGSTRUE.
    - Fixed a compile error in an implicit down-cast.
- Version 1.4 (2008-04-25):
    - Configurable line search algorithms. A member variable
      ::lbfgs_parameter_t::linesearch was added to choose either MoreThuente
      method (::LBFGS_LINESEARCH_MORETHUENTE) or backtracking algorithm
      (::LBFGS_LINESEARCH_BACKTRACKING).
    - Fixed a bug: the previous version did not compute psuedo-gradients
      properly in the line search routines for OWL-QN. This bug might quit
      an iteration process too early when the OWL-QN routine was activated
      (0 < ::lbfgs_parameter_t::orthantwise_c).
    - Configure script for POSIX environments.
    - SSE/SSE2 optimizations with GCC.
    - New functions ::lbfgs_malloc and ::lbfgs_free to use SSE/SSE2 routines
      transparently. It is uncessary to use these functions for libLBFGS built
      without SSE/SSE2 routines; you can still use any memory allocators if
      SSE/SSE2 routines are disabled in libLBFGS.
- Version 1.3 (2007-12-16):
    - An API change. An argument was added to lbfgs() function to receive the
      final value of the objective function. This argument can be set to
      \c NULL if the final value is unnecessary.
    - Fixed a null-pointer bug in the sample code (reported by Takashi Imamichi).
    - Added build scripts for Microsoft Visual Studio 2005 and GCC.
    - Added README file.
- Version 1.2 (2007-12-13):
    - Fixed a serious bug in orthant-wise L-BFGS.
      An important variable was used without initialization.
- Version 1.1 (2007-12-01):
    - Implemented orthant-wise L-BFGS.
    - Implemented lbfgs_parameter_init() function.
    - Fixed several bugs.
    - API documentation.
- Version 1.0 (2007-09-20):
    - Initial release.

@section api Documentation

- @ref liblbfgs_api "libLBFGS API"

@section sample Sample code

@include sample.c

@section ack Acknowledgements

The L-BFGS algorithm is described in:
    - Jorge Nocedal.
      Updating Quasi-Newton Matrices with Limited Storage.
      <i>Mathematics of Computation</i>, Vol. 35, No. 151, pp. 773--782, 1980.
    - Dong C. Liu and Jorge Nocedal.
      On the limited memory BFGS method for large scale optimization.
      <i>Mathematical Programming</i> B, Vol. 45, No. 3, pp. 503-528, 1989.

The line search algorithms used in this implementation are described in:
    - John E. Dennis and Robert B. Schnabel.
      <i>Numerical Methods for Unconstrained Optimization and Nonlinear
      Equations</i>, Englewood Cliffs, 1983.
    - Jorge J. More and David J. Thuente.
      Line search algorithm with guaranteed sufficient decrease.
      <i>ACM Transactions on Mathematical Software (TOMS)</i>, Vol. 20, No. 3,
      pp. 286-307, 1994.

This library also implements Orthant-Wise Limited-memory Quasi-Newton (OWL-QN)
method presented in:
    - Galen Andrew and Jianfeng Gao.
      Scalable training of L1-regularized log-linear models.
      In <i>Proceedings of the 24th International Conference on Machine
      Learning (ICML 2007)</i>, pp. 33-40, 2007.

Special thanks go to:
    - Yoshimasa Tsuruoka and Daisuke Okanohara for technical information about
      OWL-QN
    - Takashi Imamichi for the useful enhancements of the backtracking method
    - Kevin S. Van Horn, Nic Schraudolph, and Tamas Nepusz for bug fixes

Finally I would like to thank the original author, Jorge Nocedal, who has been
distributing the effieicnt and explanatory implementation in an open source
licence.

@section reference Reference

- <a href="http://www.ece.northwestern.edu/~nocedal/lbfgs.html">L-BFGS</a> by Jorge Nocedal.
- <a href="http://research.microsoft.com/en-us/downloads/b1eb1016-1738-4bd5-83a9-370c9d498a03/default.aspx">Orthant-Wise Limited-memory Quasi-Newton Optimizer for L1-regularized Objectives</a> by Galen Andrew.
- <a href="http://chasen.org/~taku/software/misc/lbfgs/">C port (via f2c)</a> by Taku Kudo.
- <a href="http://www.alglib.net/optimization/lbfgs.php">C#/C++/Delphi/VisualBasic6 port</a> in ALGLIB.
- <a href="http://cctbx.sourceforge.net/">Computational Crystallography Toolbox</a> includes
  <a href="http://cctbx.sourceforge.net/current_cvs/c_plus_plus/namespacescitbx_1_1lbfgs.html">scitbx::lbfgs</a>.
*/

#endif/*__LBFGS_H__*/
#
double slice2(double theta[], double thetaLeft[], double thetaRight[], int param, double w, int p, double XCOORDS[], double SIGMAPRIOR);
double keithrules22(double theta[], double XCOORDS[], double SIGMAPRIOR, int param);
double keithrules222(double theta[], double XTEMP[], double ZCHAIN[], int param);
double keithrules333(double theta[], double XTEMP[], double XCHAIN[], int param);
double keithrules10(double theta[], double XCOORDS[], double SIGMAPRIOR);
double keithrules100(double XCHAIN[], double ZCHAIN[]);
double keithrules11(double theta[], double XCOORDS[], double SIGMAPRIOR);
double keithrules111(double XCHAIN[], double ZCHAIN[]);
void xsvdrotate(int kpnp, int kpnq, double XTRUE3[], double theta2[], double rmatrix[]); 
double sliceZ(double theta[], double thetaLeft[], double thetaRight[], int param, double w, int p, double XTEMP[], double XCHAIN[]);
double sliceX(double theta[], double thetaLeft[], double thetaRight[], int param, double w, int p, double XTEMP[], double ZCHAIN[]);
double runif(void);


double runif(void){
	return(rand() / ((double)RAND_MAX + 1));
}

void copyFromR(int *nz, int *ny, int *na, int *nb, int *nc, int *nd, int *ne, int *nf, int *ng, double *XX, double *XCONSTRAINTS, double *YCONSTRAINTS) {
	nslice = nz;
	nburn = ny;
	nrowX = na; //Assign pointers to data passed from R to our global pointers
	ncolX = nb;
	NS = nc;
	N = nd;
	NDIM = ne;
	UNFOLD = nf;
	NMISSING = ng;
	X = XX;
	CONSTRAINTS = XCONSTRAINTS;
} 
//
static lbfgsfloatval_t evaluate(
				void *instance,
				const lbfgsfloatval_t *x,
				lbfgsfloatval_t *g,
				const int n,
				const lbfgsfloatval_t step
			       )
{
	int i, j, jj, kk;
	lbfgsfloatval_t fx = 0.0;
	double sumsquared=0;
	double circledist, circledisthat;
//	double PI=3.141592653589793;
//	double sumsquareddstar=0;
	double *ZCOORDSD, *XCOORDSD, *GG;
	ZCOORDSD     = calloc( (((*nrowX)+(*ncolX))*(*NS)), sizeof(double));
	XCOORDSD     = calloc( (((*nrowX)+(*ncolX))*(*NS)), sizeof(double));
	GG     = calloc( (((*nrowX)+(*ncolX))*(*NS)), sizeof(double));
/*
 */
/*
   UNFOLDING *
*/
//
	if(*UNFOLD == 1){
	kk=0;
//
	for(j=0;j<((*NS)*(*ncolX));j++)
	{
		GG[j]=0.0;
		if(CONSTRAINTS[j]<1.0){
			ZCOORDSD[j]=0.0;
		}
		if(CONSTRAINTS[j]>0.0){
			ZCOORDSD[j]=x[kk];
			kk=kk+1;
		}
	}
	for(j=0;j<((*NS)*(*nrowX));j++)
	{
		GG[j+((*NS)*(*ncolX))]=0.0;
		if(CONSTRAINTS[j+((*NS)*(*ncolX))]<1.0){
			XCOORDSD[j]=0.0;
		}
		if(CONSTRAINTS[j+((*NS)*(*ncolX))]>0.0){
			XCOORDSD[j]=x[kk];
			kk=kk+1;
		}

	}
//
	for(i=0;i<(*nrowX);i++)
	{
		for(j=0;j<(*ncolX);j++)
		{
			circledist = X[i*(*ncolX)+j];
/*
CATCH MISSING DATA HERE
*/
			if(circledist > 0.0)
			{
					circledisthat=0.0;
					for(jj=0;jj<(*NS);jj++)
					{
						circledisthat = circledisthat+pow((XCOORDSD[(*NS)*i+jj]-ZCOORDSD[(*NS)*j+jj]),2.0);
					}
					circledisthat=sqrt(circledisthat);
				   //catch logs of zero
					if(circledisthat<.0001)circledisthat=.001;
					sumsquared=sumsquared+pow((log(circledist)-log(circledisthat)),2.0);
//				jp = fopen("heck.txt","a");
//				fprintf(jp,"LOSS FUNCTION L-BFGS %5d %5d %20.6f %20.6f %20.6f\n",i,j,circledist,circledisthat,sumsquared);
//				fclose(jp);
/* DERIVATIVES FOR RESPONDENTS
 * */
					for(jj=0;jj<(*NS);jj++)
					{
						GG[(*NS)*i+jj+(*NS)*(*ncolX)]=GG[(*NS)*i+jj+(*NS)*(*ncolX)]-(log(circledist)-log(circledisthat))*(1.0/(pow(circledisthat,2.0)))*(XCOORDSD[(*NS)*i+jj]-ZCOORDSD[(*NS)*j+jj]);
					}
/* DERIVATIVES FOR STIMULI
 * */
					for(jj=0;jj<(*NS);jj++)
					{
						GG[(*NS)*j+jj]=GG[(*NS)*j+jj]+(log(circledist)-log(circledisthat))*(1.0/(pow(circledisthat,2.0)))*(XCOORDSD[(*NS)*i+jj]-ZCOORDSD[(*NS)*j+jj]);
					}
			}
		}
	}
	fx=sumsquared;
//     jp = fopen("heck.txt","w");
//     fprintf(jp,"LOSS FUNCTION L-BFGS %5d %5d %5d %5d %5d %20.6f\n",*NDIM,*N,*nrowX,*ncolX,*UNFOLD,fx);
//     fclose(jp);
//    printf("LOSS FUNCTION L-BFGS %5d %5d %5d %5d %20.6f\n",n,N,nrowX,ncolX,fx);
//
//  TRANSFER INTO GRADIENT VECTOR
//
	kk=0;
	for(j=0;j<((*ncolX)*(*NS));j++)
	{
		if(CONSTRAINTS[j]>0.0){
			g[kk]=GG[j];
			kk=kk+1;
		}
	}
	for(j=0;j<((*nrowX)*(*NS));j++)
	{
		if(CONSTRAINTS[j+((*ncolX)*(*NS))]>0.0){
			g[kk]=GG[j+((*ncolX)*(*NS))];
			kk=kk+1;
		}
	}

}
//
//  SIMILARITIES PROBLEM
//
	if(*UNFOLD == 0){
	kk=0;
	for(j=0;j<((*NS)*(*nrowX));j++)
	{
		GG[j]=0.0;
		if(CONSTRAINTS[j]<1.0){
			XCOORDSD[j]=0.0;
		}
		if(CONSTRAINTS[j]>0.0){
			XCOORDSD[j]=x[kk];
			kk=kk+1;
		}
	}
//
	for(i=0;i<(*nrowX);i++)
	{
		for(j=0;j<(*ncolX);j++)
		{
			circledist = X[i+j*(*nrowX)];
/*
CATCH MISSING DATA HERE
*/
			if(circledist > 0.0)
			{
				circledisthat=0.0;
				for(jj=0;jj<(*NS);jj++)
				{
					circledisthat = circledisthat+pow((XCOORDSD[(*NS)*i+jj]-XCOORDSD[(*NS)*j+jj]),2.0);
				}
				circledisthat=sqrt(circledisthat);
				if(i != j)sumsquared=sumsquared+pow((log(circledist)-log(circledisthat)),2.0);
				for(jj=0;jj<(*NS);jj++)
				{
					if(i != j)GG[(*NS)*i+jj]=GG[(*NS)*i+jj]-(log(circledist)-log(circledisthat))*(1.0/(pow(circledisthat,2.0)))*(XCOORDSD[(*NS)*i+jj]-XCOORDSD[(*NS)*j+jj]);
				}
			}
		}
	}
	fx=sumsquared;
//    fprintf(jp,"LOSS FUNCTION L-BFGS %5d %5d %5d %5d %20.6f\n",n,N,nrowX,ncolX,fx);
//    printf("LOSS FUNCTION L-BFGS %5d %5d %5d %5d %20.6f\n",n,N,nrowX,ncolX,fx);
//
//  TRANSFER INTO GRADIENT VECTOR
//
	kk=0;
	for(j=0;j<((*nrowX)*(*NS));j++)
	{
		if(CONSTRAINTS[j]<1.0){
//		    XCOORDS[j]=0.0;
		}
		if(CONSTRAINTS[j]>0.0){
			g[kk]=GG[j];
			kk=kk+1;
		}
	}
	for (i = 0;i < (*N);i++) {
//
//	    fprintf(jp,"INITIAL VALUES GRADIENT %5d %12.6f\n",i,g[i]);
//	    printf("INITIAL VALUES GRADIENT %5d %12.6f\n",i,g[i]);
	}

    }
        free(ZCOORDSD);
        free(XCOORDSD);
	free(GG);
	return fx;
}

static int progress(
		    void *instance,
		    const lbfgsfloatval_t *x,
		    const lbfgsfloatval_t *g,
		    const lbfgsfloatval_t fx,
		    const lbfgsfloatval_t xnorm,
		    const lbfgsfloatval_t gnorm,
		    const lbfgsfloatval_t step,
		    int n,
		    int k,
		    int ls
		   )
{
	Rprintf("Iteration %d:\n", k);
	Rprintf("  fx = %f, x[0] = %f,  x[1] = %f,  x[2] = %f, x[3] = %f\n", fx, x[0], x[1], x[2], x[3]);
	Rprintf("  xnorm = %f, gnorm = %f, step = %f\n", xnorm, gnorm, step);
	Rprintf("\n");
//
//	fprintf(jp,"Iteration %d:\n", k);
//	fprintf(jp,"  fx = %f, x[0] = %f,  x[1] = %f,  x[2] = %f, x[3] = %f\n", fx, x[0], x[1], x[2], x[3]);
//	fprintf(jp,"  xnorm = %f, gnorm = %f, step = %f\n", xnorm, gnorm, step);
//	fprintf(jp,"\n");
	return 0;
}
/*
 *kpnp = number of rows of y
 *kpnq = number of columns of y
rmatrix holds the starting coordinates from the double-centering
yrotate returns the results from L-BFGS (Limited-Memory
       Broyden-Fletcher-Goldfarb-Shanno algorthm
y is the symmetric matrix of dissimilarities (distances)
*/

void mainlbfgs(int *ne, int *nf, double *yrotate, double *rmatrix)
{
    R_len_t  kpnp=*ne, kpnq=*nf;
    double *a, *b;
    int i, j, kk, ret = 0;
    lbfgsfloatval_t fx;
    lbfgsfloatval_t *x = lbfgs_malloc((*N));
    lbfgs_parameter_t param;
    a     = calloc( ((kpnp+kpnq)*(*NS)), sizeof(double));
    b     = calloc( ((kpnp+kpnq)*(*NS)), sizeof(double));
// Transfer coordinates
    if(*UNFOLD == 1){
    for(i=0;i<((kpnq+kpnp)*(*NS));i++)
    {
	    a[i]=rmatrix[i];
    }
}
    if(*UNFOLD == 0){
    for(i=0;i<(kpnp*(*NS));i++)
    {
	    a[i]=rmatrix[i];
    }
    for (i = 0;i < ((*NS)*(*nrowX));i++) {
//
//	    fprintf(jp,"%5d %12.6f  %12.6f  %12.6f  %12.6f\n",i,CONSTRAINTS[i],yrotate[i],rmatrix[i],a[i]);
//
    }
    }
    if (x == NULL) {
        Rprintf("ERROR: Failed to allocate a memory block for variables.\n");
//	return 1;
	return;
    }
    if(*UNFOLD == 1){
    kk=0;
    for(j=0;j<(kpnq*(*NS));j++)
    {
	    if(CONSTRAINTS[j]<1.0){
//		    XCOORDS[j]=0.0;
	    }
	    if(CONSTRAINTS[j]>0.0){
		    x[kk]=a[j];
		    kk=kk+1;
	    }
    }
    for(j=0;j<(kpnp*(*NS));j++)
    {
	    if(CONSTRAINTS[j+(kpnq*(*NS))]>0.0){
		    x[kk]=a[j+(kpnq*(*NS))];
		    kk=kk+1;
	    }
    }
}
//
if(*UNFOLD == 0){
    kk=0;
    for(j=0;j<(kpnp*(*NS));j++)
    {
	    if(CONSTRAINTS[j]<1.0){
//		    XCOORDS[j]=0.0;
	    }
	    if(CONSTRAINTS[j]>0.0){
		    x[kk]=a[j];
		    kk=kk+1;
	    }
    }
    }
    /* Initialize the variables. */
    for (i = 0;i < (*N);i++) {
//
//	    fprintf(jp,"INITIAL VALUES %5d %12.6f\n",i,x[i]);
//	    printf("INITIAL VALUES %5d %12.6f\n",i,x[i]);
    }

    /* Initialize the parameters for the L-BFGS optimization. */
    lbfgs_parameter_init(&param);
    /*param.linesearch = LBFGS_LINESEARCH_BACKTRACKING;*/

    /*
        Start the L-BFGS optimization; this will invoke the callback functions
        evaluate() and progress() when necessary.
     */
    ret = lbfgs(*N, x, &fx, evaluate, progress, NULL, &param);

    /* Report the result. */
    Rprintf("L-BFGS optimization terminated with status code = %d\n", ret);
//    fprintf(jp,"L-BFGS optimization terminated with status code = %d\n", ret);
//    printf("  fx = %f, x[0] = %f, x[1] = %f\n", fx, x[0], x[1]);
//    Rprintf("  fx = %f, x[0] = %f,  x[1] = %f,  x[2] = %f, x[3] = %f\n", fx, x[0], x[1], x[2], x[3]);
//    fprintf(jp,"  fx = %f, x[0] = %f,  x[1] = %f,  x[2] = %f, x[3] = %f\n", fx, x[0], x[1], x[2], x[3]);

// PASS BACK THE SOLUTION
//    
// PASS BACK THE SOLUTION
//    
    if(*UNFOLD == 1){
    kk=0;
    for(j=0;j<(kpnq*(*NS));j++)
    {
	    if(CONSTRAINTS[j]<1.0){
		    yrotate[j]=0.0;
	    }
	    if(CONSTRAINTS[j]>0.0){
		    yrotate[j]=x[kk];
		    kk=kk+1;
	    }
    }
    for(j=0;j<(kpnp*(*NS));j++)
    {
//	    yrotate[j+(kpnq*NS)]=a[j+(kpnq*NS)];
//	    if(a[j+(kpnq*NS)]> -99.0){
	    if(CONSTRAINTS[j+(kpnq*(*NS))]<1.0){
		    yrotate[j]=0.0;
	    }
		    if(CONSTRAINTS[j+(kpnq*(*NS))]>0.0){
		    yrotate[j+(kpnq*(*NS))]=x[kk];
		    kk=kk+1;
	    }
    }
}
//
if(*UNFOLD == 0){
    kk=0;
    for(j=0;j<(kpnp*(*NS));j++)
    {
	    if(CONSTRAINTS[j]<1.0){
		    yrotate[j]=0.0;
	    }
	    if(CONSTRAINTS[j]>0.0){
		    yrotate[j]=x[kk];
		    kk=kk+1;
	    }
    }
}
//
    lbfgs_free(x);
    free(a);
    free(b);
    return;
}
/*
 *      Limited memory BFGS (L-BFGS).
 *
 * Copyright (c) 1990, Jorge Nocedal
 * Copyright (c) 2007-2010 Naoaki Okazaki
 * All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

/* $Id$ */

/*
This library is a C port of the FORTRAN implementation of Limited-memory
Broyden-Fletcher-Goldfarb-Shanno (L-BFGS) method written by Jorge Nocedal.
The original FORTRAN source code is available at:
http://www.ece.northwestern.edu/~nocedal/lbfgs.html

The L-BFGS algorithm is described in:
    - Jorge Nocedal.
      Updating Quasi-Newton Matrices with Limited Storage.
      <i>Mathematics of Computation</i>, Vol. 35, No. 151, pp. 773--782, 1980.
    - Dong C. Liu and Jorge Nocedal.
      On the limited memory BFGS method for large scale optimization.
      <i>Mathematical Programming</i> B, Vol. 45, No. 3, pp. 503-528, 1989.

The line search algorithms used in this implementation are described in:
    - John E. Dennis and Robert B. Schnabel.
      <i>Numerical Methods for Unconstrained Optimization and Nonlinear
      Equations</i>, Englewood Cliffs, 1983.
    - Jorge J. More and David J. Thuente.
      Line search algorithm with guaranteed sufficient decrease.
      <i>ACM Transactions on Mathematical Software (TOMS)</i>, Vol. 20, No. 3,
      pp. 286-307, 1994.

This library also implements Orthant-Wise Limited-memory Quasi-Newton (OWL-QN)
method presented in:
    - Galen Andrew and Jianfeng Gao.
      Scalable training of L1-regularized log-linear models.
      In <i>Proceedings of the 24th International Conference on Machine
      Learning (ICML 2007)</i>, pp. 33-40, 2007.

I would like to thank the original author, Jorge Nocedal, who has been
distributing the effieicnt and explanatory implementation in an open source
licence.
*/

#ifdef  HAVE_CONFIG_H
#include <config.h>
#endif/*HAVE_CONFIG_H*/

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

//#include <lbfgs.h>

#ifdef  _MSC_VER
#define inline  __inline
#endif/*_MSC_VER*/

#if     defined(USE_SSE) && defined(__SSE2__) && LBFGS_FLOAT == 64
/* Use SSE2 optimization for 64bit double precision. */
#include "arithmetic_sse_double.h"

#elif   defined(USE_SSE) && defined(__SSE__) && LBFGS_FLOAT == 32
/* Use SSE optimization for 32bit float precision. */
#include "arithmetic_sse_float.h"

#else
/* No CPU specific optimization. */
//#include "arithmetic_ansi.h"
/*
 *      ANSI C implementation of vector operations.
 *
 * Copyright (c) 2007-2010 Naoaki Okazaki
 * All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

/* $Id$ */

#include <stdlib.h>
#include <memory.h>

#if     LBFGS_FLOAT == 32 && LBFGS_IEEE_FLOAT
#define fsigndiff(x, y) (((*(uint32_t*)(x)) ^ (*(uint32_t*)(y))) & 0x80000000U)
#else
#define fsigndiff(x, y) (*(x) * (*(y) / fabs(*(y))) < 0.)
#endif/*LBFGS_IEEE_FLOAT*/

inline static void* vecalloc(size_t size)
{
	void *memblock = malloc(size);
	if (memblock) {
		memset(memblock, 0, size);
	}
	return memblock;
}

inline static void vecfree(void *memblock)
{
	free(memblock);
}

// inline static void vecset(lbfgsfloatval_t *x, const lbfgsfloatval_t c, const int n)
// {
// 	int i;

// 	for (i = 0;i < n;++i) {
// 		x[i] = c;
// 	}
// }

inline static void veccpy(lbfgsfloatval_t *y, const lbfgsfloatval_t *x, const int n)
{
	int i;

	for (i = 0;i < n;++i) {
		y[i] = x[i];
	}
}

inline static void vecncpy(lbfgsfloatval_t *y, const lbfgsfloatval_t *x, const int n)
{
	int i;

	for (i = 0;i < n;++i) {
		y[i] = -x[i];
	}
}

inline static void vecadd(lbfgsfloatval_t *y, const lbfgsfloatval_t *x, const lbfgsfloatval_t c, const int n)
{
	int i;

	for (i = 0;i < n;++i) {
		y[i] += c * x[i];
	}
}

inline static void vecdiff(lbfgsfloatval_t *z, const lbfgsfloatval_t *x, const lbfgsfloatval_t *y, const int n)
{
	int i;

	for (i = 0;i < n;++i) {
		z[i] = x[i] - y[i];
	}
}

inline static void vecscale(lbfgsfloatval_t *y, const lbfgsfloatval_t c, const int n)
{
	int i;

	for (i = 0;i < n;++i) {
		y[i] *= c;
	}
}

// inline static void vecmul(lbfgsfloatval_t *y, const lbfgsfloatval_t *x, const int n)
// {
// 	int i;

// 	for (i = 0;i < n;++i) {
// 		y[i] *= x[i];
// 	}
// }

inline static void vecdot(lbfgsfloatval_t* s, const lbfgsfloatval_t *x, const lbfgsfloatval_t *y, const int n)
{
	int i;
	*s = 0.;
	for (i = 0;i < n;++i) {
		*s += x[i] * y[i];
	}
}

inline static void vec2norm(lbfgsfloatval_t* s, const lbfgsfloatval_t *x, const int n)
{
	vecdot(s, x, x, n);
	*s = (lbfgsfloatval_t)sqrt(*s);
}

inline static void vec2norminv(lbfgsfloatval_t* s, const lbfgsfloatval_t *x, const int n)
{
	vec2norm(s, x, n);
	*s = (lbfgsfloatval_t)(1.0 / *s);
}

#endif

#define min2(a, b)      ((a) <= (b) ? (a) : (b))
#define max2(a, b)      ((a) >= (b) ? (a) : (b))
#define max3(a, b, c)   max2(max2((a), (b)), (c));

struct tag_callback_data {
    int n;
    void *instance;
    lbfgs_evaluate_t proc_evaluate;
    lbfgs_progress_t proc_progress;
};
typedef struct tag_callback_data callback_data_t;

struct tag_iteration_data {
    lbfgsfloatval_t alpha;
    lbfgsfloatval_t *s;     /* [n] */
    lbfgsfloatval_t *y;     /* [n] */
    lbfgsfloatval_t ys;     /* vecdot(y, s) */
};
typedef struct tag_iteration_data iteration_data_t;
// STOPPING CRITERIA -- EPSILON IS THE SECOND NUMBER --
// GNORM/XNORM < epsilon IS
// THE STOPPING CRITERIA
static const lbfgs_parameter_t _defparam = {
    6, 1e-2, 0, 1e-5,
    0, LBFGS_LINESEARCH_DEFAULT, 40,
    1e-20, 1e20, 1e-4, 0.9, 0.9, 1.0e-16,
    0.0, 0, -1,
};

/* Forward function declarations. */

typedef int (*line_search_proc)(
    int n,
    lbfgsfloatval_t *x,
    lbfgsfloatval_t *f,
    lbfgsfloatval_t *g,
    lbfgsfloatval_t *s,
    lbfgsfloatval_t *stp,
    const lbfgsfloatval_t* xp,
    const lbfgsfloatval_t* gp,
    lbfgsfloatval_t *wa,
    callback_data_t *cd,
    const lbfgs_parameter_t *param
    );
    
static int line_search_backtracking(
    int n,
    lbfgsfloatval_t *x,
    lbfgsfloatval_t *f,
    lbfgsfloatval_t *g,
    lbfgsfloatval_t *s,
    lbfgsfloatval_t *stp,
    const lbfgsfloatval_t* xp,
    const lbfgsfloatval_t* gp,
    lbfgsfloatval_t *wa,
    callback_data_t *cd,
    const lbfgs_parameter_t *param
    );

static int line_search_backtracking_owlqn(
    int n,
    lbfgsfloatval_t *x,
    lbfgsfloatval_t *f,
    lbfgsfloatval_t *g,
    lbfgsfloatval_t *s,
    lbfgsfloatval_t *stp,
    const lbfgsfloatval_t* xp,
    const lbfgsfloatval_t* gp,
    lbfgsfloatval_t *wp,
    callback_data_t *cd,
    const lbfgs_parameter_t *param
    );

static int line_search_morethuente(
    int n,
    lbfgsfloatval_t *x,
    lbfgsfloatval_t *f,
    lbfgsfloatval_t *g,
    lbfgsfloatval_t *s,
    lbfgsfloatval_t *stp,
    const lbfgsfloatval_t* xp,
    const lbfgsfloatval_t* gp,
    lbfgsfloatval_t *wa,
    callback_data_t *cd,
    const lbfgs_parameter_t *param
    );

static int update_trial_interval(
    lbfgsfloatval_t *x,
    lbfgsfloatval_t *fx,
    lbfgsfloatval_t *dx,
    lbfgsfloatval_t *y,
    lbfgsfloatval_t *fy,
    lbfgsfloatval_t *dy,
    lbfgsfloatval_t *t,
    lbfgsfloatval_t *ft,
    lbfgsfloatval_t *dt,
    const lbfgsfloatval_t tmin,
    const lbfgsfloatval_t tmax,
    int *brackt
    );

static lbfgsfloatval_t owlqn_x1norm(
    const lbfgsfloatval_t* x,
    const int start,
    const int n
    );

static void owlqn_pseudo_gradient(
    lbfgsfloatval_t* pg,
    const lbfgsfloatval_t* x,
    const lbfgsfloatval_t* g,
    const int n,
    const lbfgsfloatval_t c,
    const int start,
    const int end
    );

static void owlqn_project(
    lbfgsfloatval_t* d,
    const lbfgsfloatval_t* sign,
    const int start,
    const int end
    );


#if     defined(USE_SSE) && (defined(__SSE__) || defined(__SSE2__))
static int round_out_variables(int n)
{
    n += 7;
    n /= 8;
    n *= 8;
    return n;
}
#endif/*defined(USE_SSE)*/

lbfgsfloatval_t* lbfgs_malloc(int n)
{
#if     defined(USE_SSE) && (defined(__SSE__) || defined(__SSE2__))
    n = round_out_variables(n);
#endif/*defined(USE_SSE)*/
    return (lbfgsfloatval_t*)vecalloc(sizeof(lbfgsfloatval_t) * n);
}

void lbfgs_free(lbfgsfloatval_t *x)
{
    vecfree(x);
}

void lbfgs_parameter_init(lbfgs_parameter_t *param)
{
    memcpy(param, &_defparam, sizeof(*param));
}

int lbfgs(
    int n,
    lbfgsfloatval_t *x,
    lbfgsfloatval_t *ptr_fx,
    lbfgs_evaluate_t proc_evaluate,
    lbfgs_progress_t proc_progress,
    void *instance,
    lbfgs_parameter_t *_param
    )
{
    int ret;
    int i, j, k, ls, end, bound;
    lbfgsfloatval_t step;

    /* Constant parameters and their default values. */
    lbfgs_parameter_t param = (_param != NULL) ? (*_param) : _defparam;
    const int m = param.m;

    lbfgsfloatval_t *xp = NULL;
    lbfgsfloatval_t *g = NULL, *gp = NULL, *pg = NULL;
    lbfgsfloatval_t *d = NULL, *w = NULL, *pf = NULL;
    iteration_data_t *lm = NULL, *it = NULL;
    lbfgsfloatval_t ys, yy;
    lbfgsfloatval_t xnorm, gnorm, beta;
    lbfgsfloatval_t fx = 0.;
    lbfgsfloatval_t rate = 0.;
    line_search_proc linesearch = line_search_morethuente;

    /* Construct a callback data. */
    callback_data_t cd;
    cd.n = n;
    cd.instance = instance;
    cd.proc_evaluate = proc_evaluate;
    cd.proc_progress = proc_progress;

#if     defined(USE_SSE) && (defined(__SSE__) || defined(__SSE2__))
    /* Round out the number of variables. */
    n = round_out_variables(n);
#endif/*defined(USE_SSE)*/

    /* Check the input parameters for errors. */
    if (n <= 0) {
        return LBFGSERR_INVALID_N;
    }
#if     defined(USE_SSE) && (defined(__SSE__) || defined(__SSE2__))
    if (n % 8 != 0) {
        return LBFGSERR_INVALID_N_SSE;
    }
    if ((uintptr_t)(const void*)x % 16 != 0) {
        return LBFGSERR_INVALID_X_SSE;
    }
#endif/*defined(USE_SSE)*/
    if (param.epsilon < 0.) {
        return LBFGSERR_INVALID_EPSILON;
    }
    if (param.past < 0) {
        return LBFGSERR_INVALID_TESTPERIOD;
    }
    if (param.delta < 0.) {
        return LBFGSERR_INVALID_DELTA;
    }
    if (param.min_step < 0.) {
        return LBFGSERR_INVALID_MINSTEP;
    }
    if (param.max_step < param.min_step) {
        return LBFGSERR_INVALID_MAXSTEP;
    }
    if (param.ftol < 0.) {
        return LBFGSERR_INVALID_FTOL;
    }
    if (param.linesearch == LBFGS_LINESEARCH_BACKTRACKING_WOLFE ||
        param.linesearch == LBFGS_LINESEARCH_BACKTRACKING_STRONG_WOLFE) {
        if (param.wolfe <= param.ftol || 1. <= param.wolfe) {
            return LBFGSERR_INVALID_WOLFE;
        }
    }
    if (param.gtol < 0.) {
        return LBFGSERR_INVALID_GTOL;
    }
    if (param.xtol < 0.) {
        return LBFGSERR_INVALID_XTOL;
    }
    if (param.max_linesearch <= 0) {
        return LBFGSERR_INVALID_MAXLINESEARCH;
    }
    if (param.orthantwise_c < 0.) {
        return LBFGSERR_INVALID_ORTHANTWISE;
    }
    if (param.orthantwise_start < 0 || n < param.orthantwise_start) {
        return LBFGSERR_INVALID_ORTHANTWISE_START;
    }
    if (param.orthantwise_end < 0) {
        param.orthantwise_end = n;
    }
    if (n < param.orthantwise_end) {
        return LBFGSERR_INVALID_ORTHANTWISE_END;
    }
    if (param.orthantwise_c != 0.) {
        switch (param.linesearch) {
        case LBFGS_LINESEARCH_BACKTRACKING:
            linesearch = line_search_backtracking_owlqn;
            break;
        default:
            /* Only the backtracking method is available. */
            return LBFGSERR_INVALID_LINESEARCH;
        }
    } else {
        switch (param.linesearch) {
        case LBFGS_LINESEARCH_MORETHUENTE:
            linesearch = line_search_morethuente;
            break;
        case LBFGS_LINESEARCH_BACKTRACKING_ARMIJO:
        case LBFGS_LINESEARCH_BACKTRACKING_WOLFE:
        case LBFGS_LINESEARCH_BACKTRACKING_STRONG_WOLFE:
            linesearch = line_search_backtracking;
            break;
        default:
            return LBFGSERR_INVALID_LINESEARCH;
        }
    }

    /* Allocate working space. */
    xp = (lbfgsfloatval_t*)vecalloc(n * sizeof(lbfgsfloatval_t));
    g = (lbfgsfloatval_t*)vecalloc(n * sizeof(lbfgsfloatval_t));
    gp = (lbfgsfloatval_t*)vecalloc(n * sizeof(lbfgsfloatval_t));
    d = (lbfgsfloatval_t*)vecalloc(n * sizeof(lbfgsfloatval_t));
    w = (lbfgsfloatval_t*)vecalloc(n * sizeof(lbfgsfloatval_t));
    if (xp == NULL || g == NULL || gp == NULL || d == NULL || w == NULL) {
        ret = LBFGSERR_OUTOFMEMORY;
        goto lbfgs_exit;
    }

    if (param.orthantwise_c != 0.) {
        /* Allocate working space for OW-LQN. */
        pg = (lbfgsfloatval_t*)vecalloc(n * sizeof(lbfgsfloatval_t));
        if (pg == NULL) {
            ret = LBFGSERR_OUTOFMEMORY;
            goto lbfgs_exit;
        }
    }

    /* Allocate limited memory storage. */
    lm = (iteration_data_t*)vecalloc(m * sizeof(iteration_data_t));
    if (lm == NULL) {
        ret = LBFGSERR_OUTOFMEMORY;
        goto lbfgs_exit;
    }

    /* Initialize the limited memory. */
    for (i = 0;i < m;++i) {
        it = &lm[i];
        it->alpha = 0;
        it->ys = 0;
        it->s = (lbfgsfloatval_t*)vecalloc(n * sizeof(lbfgsfloatval_t));
        it->y = (lbfgsfloatval_t*)vecalloc(n * sizeof(lbfgsfloatval_t));
        if (it->s == NULL || it->y == NULL) {
            ret = LBFGSERR_OUTOFMEMORY;
            goto lbfgs_exit;
        }
    }

    /* Allocate an array for storing previous values of the objective function. */
    if (0 < param.past) {
        pf = (lbfgsfloatval_t*)vecalloc(param.past * sizeof(lbfgsfloatval_t));
    }

    /* Evaluate the function value and its gradient. */
    fx = cd.proc_evaluate(cd.instance, x, g, cd.n, 0);
    if (0. != param.orthantwise_c) {
        /* Compute the L1 norm of the variable and add it to the object value. */
        xnorm = owlqn_x1norm(x, param.orthantwise_start, param.orthantwise_end);
        fx += xnorm * param.orthantwise_c;
        owlqn_pseudo_gradient(
            pg, x, g, n,
            param.orthantwise_c, param.orthantwise_start, param.orthantwise_end
            );
    }

    /* Store the initial value of the objective function. */
    if (pf != NULL) {
        pf[0] = fx;
    }

    /*
        Compute the direction;
        we assume the initial hessian matrix H_0 as the identity matrix.
     */
    if (param.orthantwise_c == 0.) {
        vecncpy(d, g, n);
    } else {
        vecncpy(d, pg, n);
    }

    /*
       Make sure that the initial variables are not a minimizer.
     */
    vec2norm(&xnorm, x, n);
    if (param.orthantwise_c == 0.) {
        vec2norm(&gnorm, g, n);
    } else {
        vec2norm(&gnorm, pg, n);
    }
    if (xnorm < 1.0) xnorm = 1.0;
    if (gnorm / xnorm <= param.epsilon) {
        ret = LBFGS_ALREADY_MINIMIZED;
        goto lbfgs_exit;
    }

    /* Compute the initial step:
        step = 1.0 / sqrt(vecdot(d, d, n))
     */
    vec2norminv(&step, d, n);

    k = 1;
    end = 0;
    for (;;) {
        /* Store the current position and gradient vectors. */
        veccpy(xp, x, n);
        veccpy(gp, g, n);

        /* Search for an optimal step. */
        if (param.orthantwise_c == 0.) {
            ls = linesearch(n, x, &fx, g, d, &step, xp, gp, w, &cd, &param);
        } else {
            ls = linesearch(n, x, &fx, g, d, &step, xp, pg, w, &cd, &param);
            owlqn_pseudo_gradient(
                pg, x, g, n,
                param.orthantwise_c, param.orthantwise_start, param.orthantwise_end
                );
        }
        if (ls < 0) {
            /* Revert to the previous point. */
            veccpy(x, xp, n);
            veccpy(g, gp, n);
            ret = ls;
            goto lbfgs_exit;
        }

        /* Compute x and g norms. */
        vec2norm(&xnorm, x, n);
        if (param.orthantwise_c == 0.) {
            vec2norm(&gnorm, g, n);
        } else {
            vec2norm(&gnorm, pg, n);
        }

        /* Report the progress. */
        if (cd.proc_progress) {
            if ((ret = cd.proc_progress(cd.instance, x, g, fx, xnorm, gnorm, step, cd.n, k, ls))) {
                goto lbfgs_exit;
            }
        }

        /*
            Convergence test.
            The criterion is given by the following formula:
                |g(x)| / \max(1, |x|) < \epsilon
         */
        if (xnorm < 1.0) xnorm = 1.0;
        if (gnorm / xnorm <= param.epsilon) {
            /* Convergence. */
            ret = LBFGS_SUCCESS;
            break;
        }

        /*
            Test for stopping criterion.
            The criterion is given by the following formula:
                (f(past_x) - f(x)) / f(x) < \delta
         */
        if (pf != NULL) {
            /* We don't test the stopping criterion while k < past. */
            if (param.past <= k) {
                /* Compute the relative improvement from the past. */
                rate = (pf[k % param.past] - fx) / fx;

                /* The stopping criterion. */
                if (rate < param.delta) {
                    ret = LBFGS_STOP;
                    break;
                }
            }

            /* Store the current value of the objective function. */
            pf[k % param.past] = fx;
        }

        if (param.max_iterations != 0 && param.max_iterations < k+1) {
            /* Maximum number of iterations. */
            ret = LBFGSERR_MAXIMUMITERATION;
            break;
        }

        /*
            Update vectors s and y:
                s_{k+1} = x_{k+1} - x_{k} = \step * d_{k}.
                y_{k+1} = g_{k+1} - g_{k}.
         */
        it = &lm[end];
        vecdiff(it->s, x, xp, n);
        vecdiff(it->y, g, gp, n);

        /*
            Compute scalars ys and yy:
                ys = y^t \cdot s = 1 / \rho.
                yy = y^t \cdot y.
            Notice that yy is used for scaling the hessian matrix H_0 (Cholesky factor).
         */
        vecdot(&ys, it->y, it->s, n);
        vecdot(&yy, it->y, it->y, n);
        it->ys = ys;

        /*
            Recursive formula to compute dir = -(H \cdot g).
                This is described in page 779 of:
                Jorge Nocedal.
                Updating Quasi-Newton Matrices with Limited Storage.
                Mathematics of Computation, Vol. 35, No. 151,
                pp. 773--782, 1980.
         */
        bound = (m <= k) ? m : k;
        ++k;
        end = (end + 1) % m;

        /* Compute the steepest direction. */
        if (param.orthantwise_c == 0.) {
            /* Compute the negative of gradients. */
            vecncpy(d, g, n);
        } else {
            vecncpy(d, pg, n);
        }

        j = end;
        for (i = 0;i < bound;++i) {
            j = (j + m - 1) % m;    /* if (--j == -1) j = m-1; */
            it = &lm[j];
            /* \alpha_{j} = \rho_{j} s^{t}_{j} \cdot q_{k+1}. */
            vecdot(&it->alpha, it->s, d, n);
            it->alpha /= it->ys;
            /* q_{i} = q_{i+1} - \alpha_{i} y_{i}. */
            vecadd(d, it->y, -it->alpha, n);
        }

        vecscale(d, ys / yy, n);

        for (i = 0;i < bound;++i) {
            it = &lm[j];
            /* \beta_{j} = \rho_{j} y^t_{j} \cdot \gamma_{i}. */
            vecdot(&beta, it->y, d, n);
            beta /= it->ys;
            /* \gamma_{i+1} = \gamma_{i} + (\alpha_{j} - \beta_{j}) s_{j}. */
            vecadd(d, it->s, it->alpha - beta, n);
            j = (j + 1) % m;        /* if (++j == m) j = 0; */
        }

        /*
            Constrain the search direction for orthant-wise updates.
         */
        if (param.orthantwise_c != 0.) {
            for (i = param.orthantwise_start;i < param.orthantwise_end;++i) {
                if (d[i] * pg[i] >= 0) {
                    d[i] = 0;
                }
            }
        }

        /*
            Now the search direction d is ready. We try step = 1 first.
         */
        step = 1.0;
    }

lbfgs_exit:
    /* Return the final value of the objective function. */
    if (ptr_fx != NULL) {
        *ptr_fx = fx;
    }

    vecfree(pf);

    /* Free memory blocks used by this function. */
    if (lm != NULL) {
        for (i = 0;i < m;++i) {
            vecfree(lm[i].s);
            vecfree(lm[i].y);
        }
        vecfree(lm);
    }
    vecfree(pg);
    vecfree(w);
    vecfree(d);
    vecfree(gp);
    vecfree(g);
    vecfree(xp);

    return ret;
}



static int line_search_backtracking(
    int n,
    lbfgsfloatval_t *x,
    lbfgsfloatval_t *f,
    lbfgsfloatval_t *g,
    lbfgsfloatval_t *s,
    lbfgsfloatval_t *stp,
    const lbfgsfloatval_t* xp,
    const lbfgsfloatval_t* gp,
    lbfgsfloatval_t *wp,
    callback_data_t *cd,
    const lbfgs_parameter_t *param
    )
{
    int count = 0;
    lbfgsfloatval_t width, dg;
    lbfgsfloatval_t finit, dginit = 0., dgtest;
    const lbfgsfloatval_t dec = 0.5, inc = 2.1;

    /* Check the input parameters for errors. */
    if (*stp <= 0.) {
        return LBFGSERR_INVALIDPARAMETERS;
    }

    /* Compute the initial gradient in the search direction. */
    vecdot(&dginit, g, s, n);

    /* Make sure that s points to a descent direction. */
    if (0 < dginit) {
        return LBFGSERR_INCREASEGRADIENT;
    }

    /* The initial value of the objective function. */
    finit = *f;
    dgtest = param->ftol * dginit;

    for (;;) {
        veccpy(x, xp, n);
        vecadd(x, s, *stp, n);

        /* Evaluate the function and gradient values. */
        *f = cd->proc_evaluate(cd->instance, x, g, cd->n, *stp);

        ++count;

        if (*f > finit + *stp * dgtest) {
            width = dec;
        } else {
            /* The sufficient decrease condition (Armijo condition). */
            if (param->linesearch == LBFGS_LINESEARCH_BACKTRACKING_ARMIJO) {
                /* Exit with the Armijo condition. */
                return count;
	        }

	        /* Check the Wolfe condition. */
	        vecdot(&dg, g, s, n);
	        if (dg < param->wolfe * dginit) {
    		    width = inc;
	        } else {
		        if(param->linesearch == LBFGS_LINESEARCH_BACKTRACKING_WOLFE) {
		            /* Exit with the regular Wolfe condition. */
		            return count;
		        }

		        /* Check the strong Wolfe condition. */
		        if(dg > -param->wolfe * dginit) {
		            width = dec;
		        } else {
		            /* Exit with the strong Wolfe condition. */
		            return count;
		        }
            }
        }

        if (*stp < param->min_step) {
            /* The step is the minimum value. */
            return LBFGSERR_MINIMUMSTEP;
        }
        if (*stp > param->max_step) {
            /* The step is the maximum value. */
            return LBFGSERR_MAXIMUMSTEP;
        }
        if (param->max_linesearch <= count) {
            /* Maximum number of iteration. */
            return LBFGSERR_MAXIMUMLINESEARCH;
        }

        (*stp) *= width;
    }
}



static int line_search_backtracking_owlqn(
    int n,
    lbfgsfloatval_t *x,
    lbfgsfloatval_t *f,
    lbfgsfloatval_t *g,
    lbfgsfloatval_t *s,
    lbfgsfloatval_t *stp,
    const lbfgsfloatval_t* xp,
    const lbfgsfloatval_t* gp,
    lbfgsfloatval_t *wp,
    callback_data_t *cd,
    const lbfgs_parameter_t *param
    )
{
    int i, count = 0;
    lbfgsfloatval_t width = 0.5, norm = 0.;
    lbfgsfloatval_t finit = *f, dgtest;

    /* Check the input parameters for errors. */
    if (*stp <= 0.) {
        return LBFGSERR_INVALIDPARAMETERS;
    }

    /* Choose the orthant for the new point. */
    for (i = 0;i < n;++i) {
        wp[i] = (xp[i] == 0.) ? -gp[i] : xp[i];
    }

    for (;;) {
        /* Update the current point. */
        veccpy(x, xp, n);
        vecadd(x, s, *stp, n);

        /* The current point is projected onto the orthant. */
        owlqn_project(x, wp, param->orthantwise_start, param->orthantwise_end);

        /* Evaluate the function and gradient values. */
        *f = cd->proc_evaluate(cd->instance, x, g, cd->n, *stp);

        /* Compute the L1 norm of the variables and add it to the object value. */
        norm = owlqn_x1norm(x, param->orthantwise_start, param->orthantwise_end);
        *f += norm * param->orthantwise_c;

        ++count;

        dgtest = 0.;
        for (i = 0;i < n;++i) {
            dgtest += (x[i] - xp[i]) * gp[i];
        }

        if (*f <= finit + param->ftol * dgtest) {
            /* The sufficient decrease condition. */
            return count;
        }

        if (*stp < param->min_step) {
            /* The step is the minimum value. */
            return LBFGSERR_MINIMUMSTEP;
        }
        if (*stp > param->max_step) {
            /* The step is the maximum value. */
            return LBFGSERR_MAXIMUMSTEP;
        }
        if (param->max_linesearch <= count) {
            /* Maximum number of iteration. */
            return LBFGSERR_MAXIMUMLINESEARCH;
        }

        (*stp) *= width;
    }
}



static int line_search_morethuente(
    int n,
    lbfgsfloatval_t *x,
    lbfgsfloatval_t *f,
    lbfgsfloatval_t *g,
    lbfgsfloatval_t *s,
    lbfgsfloatval_t *stp,
    const lbfgsfloatval_t* xp,
    const lbfgsfloatval_t* gp,
    lbfgsfloatval_t *wa,
    callback_data_t *cd,
    const lbfgs_parameter_t *param
    )
{
    int count = 0;
    int brackt, stage1, uinfo = 0;
    lbfgsfloatval_t dg;
    lbfgsfloatval_t stx, fx, dgx;
    lbfgsfloatval_t sty, fy, dgy;
    lbfgsfloatval_t fxm, dgxm, fym, dgym, fm, dgm;
    lbfgsfloatval_t finit, ftest1, dginit, dgtest;
    lbfgsfloatval_t width, prev_width;
    lbfgsfloatval_t stmin, stmax;

    /* Check the input parameters for errors. */
    if (*stp <= 0.) {
        return LBFGSERR_INVALIDPARAMETERS;
    }

    /* Compute the initial gradient in the search direction. */
    vecdot(&dginit, g, s, n);

    /* Make sure that s points to a descent direction. */
    if (0 < dginit) {
        return LBFGSERR_INCREASEGRADIENT;
    }

    /* Initialize local variables. */
    brackt = 0;
    stage1 = 1;
    finit = *f;
    dgtest = param->ftol * dginit;
    width = param->max_step - param->min_step;
    prev_width = 2.0 * width;

    /*
        The variables stx, fx, dgx contain the values of the step,
        function, and directional derivative at the best step.
        The variables sty, fy, dgy contain the value of the step,
        function, and derivative at the other endpoint of
        the interval of uncertainty.
        The variables stp, f, dg contain the values of the step,
        function, and derivative at the current step.
    */
    stx = sty = 0.;
    fx = fy = finit;
    dgx = dgy = dginit;

    for (;;) {
        /*
            Set the minimum and maximum steps to correspond to the
            present interval of uncertainty.
         */
        if (brackt) {
            stmin = min2(stx, sty);
            stmax = max2(stx, sty);
        } else {
            stmin = stx;
            stmax = *stp + 4.0 * (*stp - stx);
        }

        /* Clip the step in the range of [stpmin, stpmax]. */
        if (*stp < param->min_step) *stp = param->min_step;
        if (param->max_step < *stp) *stp = param->max_step;

        /*
            If an unusual termination is to occur then let
            stp be the lowest point obtained so far.
         */
        if ((brackt && ((*stp <= stmin || stmax <= *stp) || param->max_linesearch <= count + 1 || uinfo != 0)) || (brackt && (stmax - stmin <= param->xtol * stmax))) {
            *stp = stx;
        }

        /*
            Compute the current value of x:
                x <- x + (*stp) * s.
         */
        veccpy(x, xp, n);
        vecadd(x, s, *stp, n);

        /* Evaluate the function and gradient values. */
        *f = cd->proc_evaluate(cd->instance, x, g, cd->n, *stp);
        vecdot(&dg, g, s, n);

        ftest1 = finit + *stp * dgtest;
        ++count;

        /* Test for errors and convergence. */
        if (brackt && ((*stp <= stmin || stmax <= *stp) || uinfo != 0)) {
            /* Rounding errors prevent further progress. */
            return LBFGSERR_ROUNDING_ERROR;
        }
        if (*stp == param->max_step && *f <= ftest1 && dg <= dgtest) {
            /* The step is the maximum value. */
            return LBFGSERR_MAXIMUMSTEP;
        }
        if (*stp == param->min_step && (ftest1 < *f || dgtest <= dg)) {
            /* The step is the minimum value. */
            return LBFGSERR_MINIMUMSTEP;
        }
        if (brackt && (stmax - stmin) <= param->xtol * stmax) {
            /* Relative width of the interval of uncertainty is at most xtol. */
            return LBFGSERR_WIDTHTOOSMALL;
        }
        if (param->max_linesearch <= count) {
            /* Maximum number of iteration. */
            return LBFGSERR_MAXIMUMLINESEARCH;
        }
        if (*f <= ftest1 && fabs(dg) <= param->gtol * (-dginit)) {
            /* The sufficient decrease condition and the directional derivative condition hold. */
            return count;
        }

        /*
            In the first stage we seek a step for which the modified
            function has a nonpositive value and nonnegative derivative.
         */
        if (stage1 && *f <= ftest1 && min2(param->ftol, param->gtol) * dginit <= dg) {
            stage1 = 0;
        }

        /*
            A modified function is used to predict the step only if
            we have not obtained a step for which the modified
            function has a nonpositive function value and nonnegative
            derivative, and if a lower function value has been
            obtained but the decrease is not sufficient.
         */
        if (stage1 && ftest1 < *f && *f <= fx) {
            /* Define the modified function and derivative values. */
            fm = *f - *stp * dgtest;
            fxm = fx - stx * dgtest;
            fym = fy - sty * dgtest;
            dgm = dg - dgtest;
            dgxm = dgx - dgtest;
            dgym = dgy - dgtest;

            /*
                Call update_trial_interval() to update the interval of
                uncertainty and to compute the new step.
             */
            uinfo = update_trial_interval(
                &stx, &fxm, &dgxm,
                &sty, &fym, &dgym,
                stp, &fm, &dgm,
                stmin, stmax, &brackt
                );

            /* Reset the function and gradient values for f. */
            fx = fxm + stx * dgtest;
            fy = fym + sty * dgtest;
            dgx = dgxm + dgtest;
            dgy = dgym + dgtest;
        } else {
            /*
                Call update_trial_interval() to update the interval of
                uncertainty and to compute the new step.
             */
            uinfo = update_trial_interval(
                &stx, &fx, &dgx,
                &sty, &fy, &dgy,
                stp, f, &dg,
                stmin, stmax, &brackt
                );
        }

        /*
            Force a sufficient decrease in the interval of uncertainty.
         */
        if (brackt) {
            if (0.66 * prev_width <= fabs(sty - stx)) {
                *stp = stx + 0.5 * (sty - stx);
            }
            prev_width = width;
            width = fabs(sty - stx);
        }
    }

    return LBFGSERR_LOGICERROR;
}



/**
 * Define the local variables for computing minimizers.
 */
#define USES_MINIMIZER \
    lbfgsfloatval_t a, d, gamma, theta, p, q, r, s;

/**
 * Find a minimizer of an interpolated cubic function.
 *  @param  cm      The minimizer of the interpolated cubic.
 *  @param  u       The value of one point, u.
 *  @param  fu      The value of f(u).
 *  @param  du      The value of f'(u).
 *  @param  v       The value of another point, v.
 *  @param  fv      The value of f(v).
 *  @param  du      The value of f'(v).
 */
#define CUBIC_MINIMIZER(cm, u, fu, du, v, fv, dv) \
    d = (v) - (u); \
    theta = ((fu) - (fv)) * 3 / d + (du) + (dv); \
    p = fabs(theta); \
    q = fabs(du); \
    r = fabs(dv); \
    s = max3(p, q, r); \
    /* gamma = s*sqrt((theta/s)**2 - (du/s) * (dv/s)) */ \
    a = theta / s; \
    gamma = s * sqrt(a * a - ((du) / s) * ((dv) / s)); \
    if ((v) < (u)) gamma = -gamma; \
    p = gamma - (du) + theta; \
    q = gamma - (du) + gamma + (dv); \
    r = p / q; \
    (cm) = (u) + r * d;

/**
 * Find a minimizer of an interpolated cubic function.
 *  @param  cm      The minimizer of the interpolated cubic.
 *  @param  u       The value of one point, u.
 *  @param  fu      The value of f(u).
 *  @param  du      The value of f'(u).
 *  @param  v       The value of another point, v.
 *  @param  fv      The value of f(v).
 *  @param  du      The value of f'(v).
 *  @param  xmin    The maximum value.
 *  @param  xmin    The minimum value.
 */
#define CUBIC_MINIMIZER2(cm, u, fu, du, v, fv, dv, xmin, xmax) \
    d = (v) - (u); \
    theta = ((fu) - (fv)) * 3 / d + (du) + (dv); \
    p = fabs(theta); \
    q = fabs(du); \
    r = fabs(dv); \
    s = max3(p, q, r); \
    /* gamma = s*sqrt((theta/s)**2 - (du/s) * (dv/s)) */ \
    a = theta / s; \
    gamma = s * sqrt(max2(0, a * a - ((du) / s) * ((dv) / s))); \
    if ((u) < (v)) gamma = -gamma; \
    p = gamma - (dv) + theta; \
    q = gamma - (dv) + gamma + (du); \
    r = p / q; \
    if (r < 0. && gamma != 0.) { \
        (cm) = (v) - r * d; \
    } else if (a < 0) { \
        (cm) = (xmax); \
    } else { \
        (cm) = (xmin); \
    }

/**
 * Find a minimizer of an interpolated quadratic function.
 *  @param  qm      The minimizer of the interpolated quadratic.
 *  @param  u       The value of one point, u.
 *  @param  fu      The value of f(u).
 *  @param  du      The value of f'(u).
 *  @param  v       The value of another point, v.
 *  @param  fv      The value of f(v).
 */
#define QUARD_MINIMIZER(qm, u, fu, du, v, fv) \
    a = (v) - (u); \
    (qm) = (u) + (du) / (((fu) - (fv)) / a + (du)) / 2 * a;

/**
 * Find a minimizer of an interpolated quadratic function.
 *  @param  qm      The minimizer of the interpolated quadratic.
 *  @param  u       The value of one point, u.
 *  @param  du      The value of f'(u).
 *  @param  v       The value of another point, v.
 *  @param  dv      The value of f'(v).
 */
#define QUARD_MINIMIZER2(qm, u, du, v, dv) \
    a = (u) - (v); \
    (qm) = (v) + (dv) / ((dv) - (du)) * a;

/**
 * Update a safeguarded trial value and interval for line search.
 *
 *  The parameter x represents the step with the least function value.
 *  The parameter t represents the current step. This function assumes
 *  that the derivative at the point of x in the direction of the step.
 *  If the bracket is set to true, the minimizer has been bracketed in
 *  an interval of uncertainty with endpoints between x and y.
 *
 *  @param  x       The pointer to the value of one endpoint.
 *  @param  fx      The pointer to the value of f(x).
 *  @param  dx      The pointer to the value of f'(x).
 *  @param  y       The pointer to the value of another endpoint.
 *  @param  fy      The pointer to the value of f(y).
 *  @param  dy      The pointer to the value of f'(y).
 *  @param  t       The pointer to the value of the trial value, t.
 *  @param  ft      The pointer to the value of f(t).
 *  @param  dt      The pointer to the value of f'(t).
 *  @param  tmin    The minimum value for the trial value, t.
 *  @param  tmax    The maximum value for the trial value, t.
 *  @param  brackt  The pointer to the predicate if the trial value is
 *                  bracketed.
 *  @retval int     Status value. Zero indicates a normal termination.
 *  
 *  @see
 *      Jorge J. More and David J. Thuente. Line search algorithm with
 *      guaranteed sufficient decrease. ACM Transactions on Mathematical
 *      Software (TOMS), Vol 20, No 3, pp. 286-307, 1994.
 */
static int update_trial_interval(
    lbfgsfloatval_t *x,
    lbfgsfloatval_t *fx,
    lbfgsfloatval_t *dx,
    lbfgsfloatval_t *y,
    lbfgsfloatval_t *fy,
    lbfgsfloatval_t *dy,
    lbfgsfloatval_t *t,
    lbfgsfloatval_t *ft,
    lbfgsfloatval_t *dt,
    const lbfgsfloatval_t tmin,
    const lbfgsfloatval_t tmax,
    int *brackt
    )
{
    int bound;
    int dsign = fsigndiff(dt, dx);
    lbfgsfloatval_t mc; /* minimizer of an interpolated cubic. */
    lbfgsfloatval_t mq; /* minimizer of an interpolated quadratic. */
    lbfgsfloatval_t newt;   /* new trial value. */
    USES_MINIMIZER;     /* for CUBIC_MINIMIZER and QUARD_MINIMIZER. */

    /* Check the input parameters for errors. */
    if (*brackt) {
        if (*t <= min2(*x, *y) || max2(*x, *y) <= *t) {
            /* The trival value t is out of the interval. */
            return LBFGSERR_OUTOFINTERVAL;
        }
        if (0. <= *dx * (*t - *x)) {
            /* The function must decrease from x. */
            return LBFGSERR_INCREASEGRADIENT;
        }
        if (tmax < tmin) {
            /* Incorrect tmin and tmax specified. */
            return LBFGSERR_INCORRECT_TMINMAX;
        }
    }

    /*
        Trial value selection.
     */
    if (*fx < *ft) {
        /*
            Case 1: a higher function value.
            The minimum is brackt. If the cubic minimizer is closer
            to x than the quadratic one, the cubic one is taken, else
            the average of the minimizers is taken.
         */
        *brackt = 1;
        bound = 1;
        CUBIC_MINIMIZER(mc, *x, *fx, *dx, *t, *ft, *dt);
        QUARD_MINIMIZER(mq, *x, *fx, *dx, *t, *ft);
        if (fabs(mc - *x) < fabs(mq - *x)) {
            newt = mc;
        } else {
            newt = mc + 0.5 * (mq - mc);
        }
    } else if (dsign) {
        /*
            Case 2: a lower function value and derivatives of
            opposite sign. The minimum is brackt. If the cubic
            minimizer is closer to x than the quadratic (secant) one,
            the cubic one is taken, else the quadratic one is taken.
         */
        *brackt = 1;
        bound = 0;
        CUBIC_MINIMIZER(mc, *x, *fx, *dx, *t, *ft, *dt);
        QUARD_MINIMIZER2(mq, *x, *dx, *t, *dt);
        if (fabs(mc - *t) > fabs(mq - *t)) {
            newt = mc;
        } else {
            newt = mq;
        }
    } else if (fabs(*dt) < fabs(*dx)) {
        /*
            Case 3: a lower function value, derivatives of the
            same sign, and the magnitude of the derivative decreases.
            The cubic minimizer is only used if the cubic tends to
            infinity in the direction of the minimizer or if the minimum
            of the cubic is beyond t. Otherwise the cubic minimizer is
            defined to be either tmin or tmax. The quadratic (secant)
            minimizer is also computed and if the minimum is brackt
            then the the minimizer closest to x is taken, else the one
            farthest away is taken.
         */
        bound = 1;
        CUBIC_MINIMIZER2(mc, *x, *fx, *dx, *t, *ft, *dt, tmin, tmax);
        QUARD_MINIMIZER2(mq, *x, *dx, *t, *dt);
        if (*brackt) {
            if (fabs(*t - mc) < fabs(*t - mq)) {
                newt = mc;
            } else {
                newt = mq;
            }
        } else {
            if (fabs(*t - mc) > fabs(*t - mq)) {
                newt = mc;
            } else {
                newt = mq;
            }
        }
    } else {
        /*
            Case 4: a lower function value, derivatives of the
            same sign, and the magnitude of the derivative does
            not decrease. If the minimum is not brackt, the step
            is either tmin or tmax, else the cubic minimizer is taken.
         */
        bound = 0;
        if (*brackt) {
            CUBIC_MINIMIZER(newt, *t, *ft, *dt, *y, *fy, *dy);
        } else if (*x < *t) {
            newt = tmax;
        } else {
            newt = tmin;
        }
    }

    /*
        Update the interval of uncertainty. This update does not
        depend on the new step or the case analysis above.

        - Case a: if f(x) < f(t),
            x <- x, y <- t.
        - Case b: if f(t) <= f(x) && f'(t)*f'(x) > 0,
            x <- t, y <- y.
        - Case c: if f(t) <= f(x) && f'(t)*f'(x) < 0, 
            x <- t, y <- x.
     */
    if (*fx < *ft) {
        /* Case a */
        *y = *t;
        *fy = *ft;
        *dy = *dt;
    } else {
        /* Case c */
        if (dsign) {
            *y = *x;
            *fy = *fx;
            *dy = *dx;
        }
        /* Cases b and c */
        *x = *t;
        *fx = *ft;
        *dx = *dt;
    }

    /* Clip the new trial value in [tmin, tmax]. */
    if (tmax < newt) newt = tmax;
    if (newt < tmin) newt = tmin;

    /*
        Redefine the new trial value if it is close to the upper bound
        of the interval.
     */
    if (*brackt && bound) {
        mq = *x + 0.66 * (*y - *x);
        if (*x < *y) {
            if (mq < newt) newt = mq;
        } else {
            if (newt < mq) newt = mq;
        }
    }

    /* Return the new trial value. */
    *t = newt;
    return 0;
}





static lbfgsfloatval_t owlqn_x1norm(
    const lbfgsfloatval_t* x,
    const int start,
    const int n
    )
{
    int i;
    lbfgsfloatval_t norm = 0.;

    for (i = start;i < n;++i) {
        norm += fabs(x[i]);
    }

    return norm;
}

static void owlqn_pseudo_gradient(
    lbfgsfloatval_t* pg,
    const lbfgsfloatval_t* x,
    const lbfgsfloatval_t* g,
    const int n,
    const lbfgsfloatval_t c,
    const int start,
    const int end
    )
{
    int i;

    /* Compute the negative of gradients. */
    for (i = 0;i < start;++i) {
        pg[i] = g[i];
    }

    /* Compute the psuedo-gradients. */
    for (i = start;i < end;++i) {
        if (x[i] < 0.) {
            /* Differentiable. */
            pg[i] = g[i] - c;
        } else if (0. < x[i]) {
            /* Differentiable. */
            pg[i] = g[i] + c;
        } else {
            if (g[i] < -c) {
                /* Take the right partial derivative. */
                pg[i] = g[i] + c;
            } else if (c < g[i]) {
                /* Take the left partial derivative. */
                pg[i] = g[i] - c;
            } else {
                pg[i] = 0.;
            }
        }
    }

    for (i = end;i < n;++i) {
        pg[i] = g[i];
    }
}

static void owlqn_project(
    lbfgsfloatval_t* d,
    const lbfgsfloatval_t* sign,
    const int start,
    const int end
    )
{
    int i;

    for (i = start;i < end;++i) {
        if (d[i] * sign[i] <= 0) {
            d[i] = 0;
        }
    }
}
//
//  Calculates log-posterior over all the parameters
//
void keithrules(double *theta, double *XCOORDS, double *sumsquared, double *SIGMAPRIOR)
{
	int i, j, jj, kk;
//	double sumsquared=0;
	double circledist, circledisthat, XPI;
//	double PI=3.141592653589793;
	double sumsquareddstar=0;
/*
 */
/*
 *NOTE THAT THIS IS NDIM-1 IF SIGMA**2 IS BEING ESTIMATED!!!!
*/
	XPI=3.141592653589793;
//
	kk=0;
	for(j=0;j<(*NS)*(*nrowX);j++)
	{
		if(CONSTRAINTS[j]<1.0){
			XCOORDS[j]=0.0;
		}
		if(CONSTRAINTS[j]>0.0){
			XCOORDS[j]=theta[kk];
			kk=kk+1;
		}
	}
	for(i=0;i<(*nrowX);i++)
	{
		for(j=0;j<(*ncolX);j++)
		{
			circledist = X[i+j*(*nrowX)];
/*
CATCH MISSING DATA HERE
*/
			if(circledist > 0.0)
			{
				circledisthat=0.0;
				for(jj=0;jj<(*NS);jj++)
				{
					circledisthat = circledisthat+pow((XCOORDS[(*NS)*i+jj]-XCOORDS[(*NS)*j+jj]),2.0);
				}
				circledisthat=sqrt(circledisthat);
				if(i != j)*sumsquared=*sumsquared+pow((log(circledist)-log(circledisthat)),2.0);
				if(i != j)sumsquareddstar=sumsquareddstar+log(1.0/circledist);
			}
		}
	}
//	fprintf(kp,"%20.6f\n",sumsquared);
//	printf("keithrules %20.6f %12.6f\n",sumsquared,theta[NDIM-1]);
/*  ADD IN THE LOG OF THE PRIORS ON THE COORDINATES -- SIMPLE NORMAL
 *  DISTRIBUTIONS  */
/*  Set PRIORS ON COORDINATES TO N(0,sigma**2)
 *  IN THIS CASE USE THE NEGATIVE OF THE SEE SO THAT THE OBJECTIVE
 *  FUNCTION IS NEGATIVE -- -SSE - LOG(PRIORS) */
	*sumsquared=*sumsquared/2.0;
	*sumsquared=(1/(2.0*theta[(*NDIM)-1]))*(*sumsquared) + sumsquareddstar/2.0;
	*sumsquared=*sumsquared+((((*ncolX)*((*ncolX)-1))/2)/2)*log(2.0*XPI*theta[(*NDIM)-1]);
	for(j=0;j<(*NDIM)-1;j++)
	{
		*sumsquared=*sumsquared+((theta[j]*theta[j])/(2.0*(*SIGMAPRIOR)*(*SIGMAPRIOR)));
	}
//
//	return -sumsquared;
}
void slicesimilarities(double *theta, double *theta2, double *theta1000, double *ssenow, double *XTRUE, double *thetaLeft, double *thetaRight, double *WW, int *PP, double *XCOORDS, double *SIGMAPRIOR)
{
	int i, j, kk, kki;
	double *XTRUE2, *rmatrix;
	double sum1,sum2;
//	double sum1,sum2,sum3,sum4;
	double SLICE_W = *WW;
	int SLICE_P = *PP;
	int kpnp=(*nrowX)-1;
	int kpnq=(*NS);
	XTRUE2 = calloc( (kpnp)*kpnq, sizeof(double));
	rmatrix = calloc( (kpnq)*kpnq, sizeof(double));

	for(j=0;j<(*NS)*(*nrowX);j++)
	{
//		fprintf(kp,"Zzsim %5d %7.2f\n",j,CONSTRAINTS[j]);
		
	}
//	fclose(kp);
//
  kki=0;
  for(i=0;i<((*nburn)+(*nslice));i++)
  {
     for(j=0;j<(*NDIM);j++)
     { 
	if(j < (*NDIM)-1){
		theta2[j] = slice2(theta2,thetaLeft,thetaRight,j,SLICE_W,SLICE_P, XCOORDS, *SIGMAPRIOR);
	}
	theta[j]=theta2[j];
//	theta1000[(i*(*NDIM))+j]=theta2[j];
     }
     theta[(*NDIM)-1]=keithrules11(theta2,XCOORDS,*SIGMAPRIOR);
     theta2[(*NDIM)-1]=theta[(*NDIM)-1];
     ssenow[i]=keithrules10(theta2,XCOORDS,*SIGMAPRIOR);
     ssenow[i+((*nslice)+(*nburn))]=keithrules11(theta2,XCOORDS,*SIGMAPRIOR);
/*
  Assemble the Matrices for the Orthogonal Rotation
*/
     kk=0;
     for(j=0;j<(*NS)*(*nrowX);j++)
     {
	     if(CONSTRAINTS[j]<1.0){
		     XCOORDS[j]=0.0;
	     }
	     if(CONSTRAINTS[j]>0.0){
		     XCOORDS[j]=theta2[kk];
		     XTRUE2[kk]=XTRUE[j];
		     kk=kk+1;
	     }
     }
//
// CALL ORTHOGONAL PROCRUSTES ROTATION HERE -- XTRUE (XTRUE2) ARE THE TARGETS,
// XCOORDS (theta2) IS THE MATRIX TO BE ROTATED, RMATRIX IS THE ROTATION MATRIX
//
//  Calculate SSE Before Rotation
//
     sum1=0.0;
     for (j=0;j<((*nrowX)*(*NS));j++)
     {
	     sum1=sum1+pow((XTRUE[j]-XCOORDS[j]),2.0);
     }
/*
   Note that the origin is dropped from XTRUE so kpnp = nrowX-1.
   theta2 is length NDIM=(nrowX-1)*NS + 1
*/
     xsvdrotate(kpnp,kpnq,XTRUE2,theta2,rmatrix);
//
     kk=0;
     for(j=0;j<(*NS)*(*nrowX);j++)
     {
	     if(CONSTRAINTS[j]<1.0){
		     XCOORDS[j]=0.0;
	     }
	     if(CONSTRAINTS[j]>0.0){
		     XCOORDS[j]=theta2[kk];
// PUT ROTATED CONFIGURATION INTO theta
		     theta[kk]=theta2[kk];
		     kk=kk+1;
	     }
     }
//
//  Calculate SSE After Rotation
//
     sum2=0.0;
     for (j=0;j<((*nrowX)*(*NS));j++)
     {
	     sum2=sum2+pow((XTRUE[j]-XCOORDS[j]),2.0);
     }
//     sum3=keithrules10(theta2,XCOORDS,*SIGMAPRIOR);
//     sum4=keithrules11(theta2,XCOORDS,*SIGMAPRIOR);
//     jp = fopen("heck.txt","a");
//     fprintf(jp,"SSE Before and After Rotation %10.5f %10.5f %10.5f %10.5f %10.5f %10.5f %10.5f\n",sum1,sum2,ssenow[i],ssenow[i+((*nslice)+(*nburn))],sum3,sum4,theta2[(*NDIM)-1]);
//     fclose(jp);
//
     if(i > (*nburn))
     {
        for(j=0;j<(*NDIM);j++)
        { 
	     theta1000[(kki*(*NDIM))+j]=theta2[j];
        }
     kki=kki+1;
//
     }
  }
  free(rmatrix);
  free(XTRUE2);
}

double slice2(double theta[], double thetaLeft[], double thetaRight[], int param, double w, int p, double XCOORDS[], double SIGMAPRIOR){

	double x,y,L,R,leftloglike,rightloglike;
//	double xx,x,y,L,R,leftloglike,rightloglike;
	int K;
	short int flag;
	double *temp;
	double rexp = -1*log(runif());
	temp = (double *) malloc((*NDIM)*sizeof(double));
//
//	xx=theta[param];
//
	for(K=0;K<(*NDIM);K++){
		temp[K]=theta[K];
	}
// Insert here
// w fixed at 1.0, K is fixed at 3
	y = keithrules22(theta, XCOORDS, SIGMAPRIOR, param) - rexp;
	L = theta[param] - w*runif();
	if(L<thetaLeft[param])L=thetaLeft[param]+0.000001;
	R = L + w;
	if(R>thetaRight[param]){
		R=thetaRight[param]-0.000001;
		if(L>R)L=thetaLeft[param]+0.000001;
	}
	K = p;

	temp[param] = L;
	leftloglike = keithrules22(temp, XCOORDS, SIGMAPRIOR, param);
	temp[param] = R;
	rightloglike = keithrules22(temp, XCOORDS, SIGMAPRIOR, param);

	flag=0;
	if(y < leftloglike) flag=1;
	else if(y < rightloglike) flag=-1;

	while(K>0 && flag!=0) {
		if(runif()<0.5) {
			L = 2*L - R;
			if(L<thetaLeft[param])L=thetaLeft[param]+0.000001;
			temp[param] = L;
			if(y >=keithrules22(temp, XCOORDS, SIGMAPRIOR, param) && flag==1) flag=0; 
		}
		else {
			R = 2*R -L;
			if(R>thetaRight[param])R=thetaRight[param]-0.000001;
			temp[param] = R;
			if(y >=keithrules22(temp, XCOORDS, SIGMAPRIOR, param) && flag==-1) flag=0; 
		}
		K--;   
	}

	while(1){
		x = L + runif()*(R-L);
		if(x>thetaRight[param])x=thetaRight[param]-0.000001;
		if(x<thetaLeft[param])x=thetaLeft[param]+0.000001;
		temp[param] = x;
		if(keithrules22(temp, XCOORDS, SIGMAPRIOR, param)>y) break;
		if(theta[param] > x) L = x;
		else R = x;
	} 
//
	free(temp);
	return(x);

}
// End of Slice2

double keithrules22(double theta[], double XCOORDS[], double SIGMAPRIOR, int param)
{
	int i, j, jj, kk, k1, marker;
	double sumsquared=0;
	double circledist, circledisthat;
	marker=0;
/*
 *
 */
/*
 *NOTE THAT THIS IS NDIM-1 IF SIGMA**2 IS BEING ESTIMATED!!!!
*/
	kk=0;
	for(j=0;j<(*NS)*(*nrowX);j++)
	{
		if(CONSTRAINTS[j]<1.0){
			XCOORDS[j]=0.0;
		}
		if(CONSTRAINTS[j]>0.0){
			XCOORDS[j]=theta[kk];
			if(kk == param)marker=j;
			kk=kk+1;
		}
	}
	k1=marker/(*NS);
//	k2=marker % NS;
// First Dimension = 0, 2nd Dimension = 1, 3rd Dimension = 2, etc
//	jj=k2;
	i=k1;
	for(j=0;j<(*ncolX);j++)
	{
		circledist = X[i+j*(*nrowX)];
/*
CATCH MISSING DATA HERE
*/
		if(circledist > 0.0)
		{
			circledisthat=0.0;
			for(jj=0;jj<(*NS);jj++)
			{
				circledisthat = circledisthat+pow((XCOORDS[(*NS)*i+jj]-XCOORDS[(*NS)*j+jj]),2.0);
			}
			circledisthat=sqrt(circledisthat);

			if(i != j)sumsquared=sumsquared+pow((log(circledist)-log(circledisthat)),2.0);
		}
	}
/*  ADD IN THE LOG OF THE PRIORS ON THE COORDINATES -- SIMPLE NORMAL
 *  DISTRIBUTIONS  */
/*  Set PRIORS ON COORDINATES TO N(0,sigma**2)
 *  IN THIS CASE USE THE NEGATIVE OF THE SEE SO THAT THE OBJECTIVE
 *  FUNCTION IS NEGATIVE -- -SSE - LOG(PRIORS) */

//		printf("keithrules22 %8d %8d %15.6f %15.6f %15.6f\n",param,k1,theta[param],sumsquared,theta[NDIM-1]);
	sumsquared=sumsquared/2.0;
	sumsquared=(1/(2.0*theta[(*NDIM)-1]))*sumsquared;
	sumsquared=sumsquared+((((*ncolX)*((*ncolX)-1))/2)/2)*log(theta[(*NDIM)-1]);
	for(j=0;j<(*NDIM)-1;j++)
	{
		sumsquared=sumsquared+((theta[j]*theta[j])/(2.0*(SIGMAPRIOR)*(SIGMAPRIOR)));
	}

//
//	printf("keithrules23 %8d %8d %15.6f %15.6f\n",param,k1,theta[param],sumsquared);
	return -sumsquared;
}
//
// This Routine calculates the full Log-Posterior for the Similarities
// Model
//
//
//
double keithrules10(double theta[], double XCOORDS[], double SIGMAPRIOR)
{
	int i, j, jj, kk;
	double sumsquared=0;
	double circledist, circledisthat;
	double XPI=3.141592653589793;
	double sumsquareddstar=0;
/*
 */
/*
 *NOTE THAT THIS IS NDIM-1 IF SIGMA**2 IS BEING ESTIMATED!!!!
*/
	kk=0;
	for(j=0;j<(*NS)*(*nrowX);j++)
	{
		if(CONSTRAINTS[j]<1.0){
			XCOORDS[j]=0.0;
		}
		if(CONSTRAINTS[j]>0.0){
			XCOORDS[j]=theta[kk];
			kk=kk+1;
		}
	}
	for(i=0;i<(*nrowX);i++)
	{
		for(j=0;j<(*ncolX);j++)
		{
			circledist = X[i+j*(*nrowX)];
/*
CATCH MISSING DATA HERE
*/
			if(circledist > 0.0)
			{
				circledisthat=0.0;
				for(jj=0;jj<(*NS);jj++)
				{
					circledisthat = circledisthat+pow((XCOORDS[(*NS)*i+jj]-XCOORDS[(*NS)*j+jj]),2.0);
				}
				circledisthat=sqrt(circledisthat);
				if(i != j)sumsquared=sumsquared+pow((log(circledist)-log(circledisthat)),2.0);
				if(i != j)sumsquareddstar=sumsquareddstar+log(1.0/circledist);
			}
		}
	}
/*  ADD IN THE LOG OF THE PRIORS ON THE COORDINATES -- SIMPLE NORMAL
 *  DISTRIBUTIONS  */
/*  Set PRIORS ON COORDINATES TO N(0,sigma**2)
 *  IN THIS CASE USE THE NEGATIVE OF THE SEE SO THAT THE OBJECTIVE
 *  FUNCTION IS NEGATIVE -- -SSE - LOG(PRIORS) */
	sumsquared=sumsquared/2.0;
	sumsquared=(1/(2.0*theta[(*NDIM)-1]))*sumsquared + sumsquareddstar/2.0;
	sumsquared=sumsquared+((((*ncolX)*((*ncolX)-1))/2)/2)*log(2.0*XPI*theta[(*NDIM)-1]);
	for(j=0;j<(*NDIM)-1;j++)
	{
		sumsquared=sumsquared+((theta[j]*theta[j])/(2.0*SIGMAPRIOR*SIGMAPRIOR));
	}
//
	return -sumsquared;
}
//
//  This Routine Calculates sigma-hat-squared for the Similarities
//  Problem
//
double keithrules11(double theta[], double XCOORDS[], double SIGMAPRIOR)
{
	int i, j, jj, kk;
	double sumsquared=0;
	double circledist, circledisthat;
//	double XPI=3.141592653589793;
	double sumsquareddstar=0;
/*
 */
/*
 *NOTE THAT THIS IS NDIM-1 IF SIGMA**2 IS BEING ESTIMATED!!!!
*/
	kk=0;
	for(j=0;j<(*NS)*(*nrowX);j++)
	{
		if(CONSTRAINTS[j]<1.0){
			XCOORDS[j]=0.0;
		}
		if(CONSTRAINTS[j]>0.0){
			XCOORDS[j]=theta[kk];
			kk=kk+1;
		}
	}
	for(i=0;i<(*nrowX);i++)
	{
		for(j=0;j<(*ncolX);j++)
		{
			circledist = X[i+j*(*nrowX)];
/*
CATCH MISSING DATA HERE
*/
			if(circledist > 0.0)
			{
				circledisthat=0.0;
				for(jj=0;jj<(*NS);jj++)
				{
					circledisthat = circledisthat+pow((XCOORDS[(*NS)*i+jj]-XCOORDS[(*NS)*j+jj]),2.0);
				}
				circledisthat=sqrt(circledisthat);
				if(i != j)sumsquared=sumsquared+pow((log(circledist)-log(circledisthat)),2.0);
				if(i != j)sumsquareddstar=sumsquareddstar+log(1.0/circledist);
			}
		}
	}
/*  COMPUTE VARIANCE TERM  */
//
	sumsquared=sumsquared/((*ncolX)*((*ncolX)-1));
//
	return sumsquared;
}
//  ORTHOGONAL PROCRUSTES ROTATION SUBROUTINE
//
//  NOTE THAT y IS STACKED BY ROW IN C BUT IT MUST BE STACKED BY COLUMN
//     IN THE FORTRAN CALL
//L(T) = tr(A - BT)(A - BT)'
//
//   The solution is:

//   T = VU' where

//   A'B = ULV'
//
void xsvdrotate(int kpnp, int kpnq, double XTRUE3[], double theta2[], double rmatrix[]) {
/*   
*/

	double *a, *b, *work, *XprimeX, *XXinv;
	double *u, *lambda, *vt, *ydummy;
//        double *work;
	double sum, sum2, svd_error_sum, svd_error_sum_2, sumulv;
	int i, j, jj;
	int  info = 12;
	int  lwork= 10*(kpnp+kpnq);
	int  lda,ldu,ldvt,nrow,ncol;

	a     = (double *) malloc( kpnp*kpnq*sizeof(double));
	b     = (double *) malloc( kpnp*kpnq*sizeof(double));
	work  = (double *) malloc( lwork*sizeof(double));
	XprimeX  = (double *) malloc( kpnq*kpnq*sizeof(double));
	ydummy  = (double *) malloc( kpnq*kpnq*sizeof(double));
	XXinv = (double *) malloc( kpnq*kpnq*sizeof(double));
	u      = (double *) malloc (kpnq*kpnq*sizeof(double));
	lambda = (double *) malloc (kpnq*kpnq*sizeof(double));
	vt     = (double *) malloc (kpnq*kpnq*sizeof(double));
//
	lda  = kpnq;
	nrow = kpnp;
	ncol = kpnq;
	ldu  = kpnq;
	ldvt = kpnq;

	for (i=0;i<lwork;i++){
		work[i] = 0;
	}

//	jp = fopen("heck2.txt","a");
//	fprintf(jp,"lwork=%5d %5d %5d %5d\n",lwork,kpnp,kpnq,lda);
//	fclose(jp);
	for (i=0;i<(nrow);i++) {
		for (j=0;j<kpnq;j++) {
			a[(j*(nrow))+i] = XTRUE3[(i*kpnq)+j];
			b[(j*(nrow))+i] = theta2[(i*kpnq)+j];
		}
	}
	sum2=0.0;
	for (i=0;i<(nrow*ncol);i++)
	{
		sum2=sum2+pow((XTRUE3[i]-theta2[i]),2.0);
	}
//	jp = fopen("heck2.txt","a");
//	fprintf(jp," SSE Before Rotation in SVD %7d %20.8f\n",nrow*ncol, sum2);
//	fclose(jp);
// Calculate A'B -- store in XprimeX FORTRAN column stacked style
	for(j=0;j<(ncol);j++)
	{
		for(jj=0;jj<(ncol);jj++)
		{
			sum=0.0;
			for(i=0;i<nrow;i++)
			{
				sum=sum+a[j*(nrow)+i]*b[jj*(nrow)+i];
			}
			XprimeX[jj*ncol+j]=sum;
		}
	}


//  Pass it the Matrix

//	jp = fopen("heck2.txt","a");

	for(j=0;j<(ncol);j++)
	{
//		fprintf(jp,"%5d ",j);

		for(jj=0;jj<ncol;jj++)

		{
//			fprintf(jp,"%15.6f",XprimeX[jj*(ncol)+j]);

		}
//		fprintf(jp,"\n");
	}
//	fclose(jp);
	for (i=0;i<lwork;i++){
		work[i] = 0;
	}
	for (i=0;i<(ncol*ncol);i++){
		ydummy[i]=XprimeX[i];
	}
//
double* rwork;
int* iwork;

dgesvd_("A","A", &kpnq, &kpnq, XprimeX, &lda, lambda,
          u, &ldu, vt, &ldvt, work, &lwork, rwork, iwork, &info);
          
//	jp = fopen("heck2.txt","a");
//	fprintf(jp,"Info = %i\n",info);
//	fprintf(jp,"Singular Values\n");
	for(jj=0;jj<kpnq;jj++)
	{
//		fprintf(jp,"%5d %16.6f\n",jj,lambda[jj]);
	}
/*
  Do simple check of SVD

*/
	svd_error_sum=0.0;
	svd_error_sum_2=0.0;
	for (i=0;i<kpnq;i++)
	{
		for (jj=0;jj<kpnq;jj++)
		{
			sumulv=0.0;
			for (j=0;j<kpnq;j++)
			{
				sumulv+=u[(j*kpnq)+i]*lambda[j]*vt[j+(jj*kpnq)];
			}
			svd_error_sum+=(ydummy[i+(jj*kpnq)]-sumulv)*(ydummy[i+(jj*kpnq)]-sumulv);
			svd_error_sum_2+=fabs(ydummy[i+(jj*kpnq)]-sumulv);
		}
	}
//	fprintf(jp,"SVD Error Check = %12.7g %12.7g\n",svd_error_sum,svd_error_sum_2);
//	fclose(jp);
//
//  Write out U and vt as checks -- U and V' are stored by column,
//  FORTRAN style
//	printf("\nU Matrix\n");
//	fprintf(jp,"\nU Matrix\n");

	for (i=0;i<kpnq;i++)
	{
		for (jj=0;jj<kpnq;jj++)
		{
//			printf("%10.5f",u[jj*kpnq+i]);
//			fprintf(jp,"%10.5f",u[jj*kpnq+i]);
		}
//		printf("\n");
//		fprintf(jp,"\n");
	}
//	printf("\nV Matrix\n");
//	fprintf(jp,"\nV Matrix\n");

	for (i=0;i<kpnq;i++)
	{
		for (jj=0;jj<kpnq;jj++)
		{
//			printf("%10.5f",vt[(jj*kpnq)+i]);
//			fprintf(jp,"%10.5f",vt[(jj*kpnq)+i]);
		}
//		printf("\n");
//		fprintf(jp,"\n");
	}
//	printf("\nV' Matrix\n");
	for (i=0;i<(kpnq*kpnq);i++)
	{
//		printf("%10.5f",vt[i]);
	}
//	printf("\n");
//	printf("\nU Matrix\n");
	for (i=0;i<(kpnq*kpnq);i++)
	{
//		printf("%10.5f",u[i]);
	}
//	printf("\n");
//   The solution is:

//   T = VU' where T=rmatrix
//     Put into rmatrix[.] in C style stacked by rows
//
	for (i=0;i<kpnq;i++)
	{
		for (j=0;j<kpnq;j++)
		{
			sum=0.0;
			for (jj=0;jj<kpnq;jj++)
			{
				sum=sum+vt[jj+(j*kpnq)]*u[i+(jj*kpnq)];
			}
//			printf("%5d %5d %10.5f \n",i,j,sum);
			rmatrix[i+(j*kpnq)]=sum;
		}
	}
//
// Rotate B (ZCOORDS2 == yrotate)
//
	for (i=0;i<(nrow);i++) {
		for (j=0;j<kpnq;j++)
		{
			sum=0.0;
			for (jj=0;jj<kpnq;jj++) {
				sum=sum+theta2[(i*kpnq)+jj]*rmatrix[(jj*kpnq)+j];
			}
			b[(i*kpnq)+j]=sum;
		}
//		printf("%5d %10.5f %10.5f %10.5f %10.5f \n",i,yrotate[(i*kpnq)+0],yrotate[(i*kpnq)+1],b[(i*kpnq)+0],b[(i*kpnq)+1]);
//		fprintf(jp,"%5d %10.5f %10.5f %10.5f %10.5f \n",i,yrotate[(i*kpnq)+0],yrotate[(i*kpnq)+1],b[(i*kpnq)+0],b[(i*kpnq)+1]);
	}
//
// Transfer into yrotate for passing back
//
	for (i=0;i<(nrow*kpnq);i++)
	{
		theta2[i]=b[i];
	}
	sum=0.0;
	for (i=0;i<(nrow*ncol);i++)
	{
		sum=sum+pow((XTRUE3[i]-theta2[i]),2.0);
//		sum=sum+pow((y[i]-b[i]),2.0);
	}
//	jp = fopen("heck2.txt","a");
//	fprintf(jp,"\nSSE Before and After Rotation %10.5f %10.5f\n",sum2,sum);
//	fclose(jp);
//
	free(a);
	free(b);
	free(work);
	free(XprimeX);
	free(ydummy);
	free(XXinv);
	free(u);
	free(lambda);
	free(vt);
//
//	i2012=0;
//	idebug=0;
//	if(i2012==0)idebug=99998;
//	if(idebug==99998)exit(EXIT_FAILURE);
// Storage by Column FORTRAN Style -- y is storage by row
//	return svd_error_sum_2;
}
//
// SLICE SAMPLER FOR UNFOLDING PROBLEM
//
void sliceunfolding(double *theta, double *theta2, double *theta1000, double *ssenow, double *XTRUE, double *thetaLeft, double *thetaRight, double *WW, int *PP, double *XCOORDS, double *SIGMAPRIOR)
{
	int i2011, i, j, jj, kk, kki, kkk, kkx;
	double *XTRUE3, *rmatrix, *XTEMP, *XCHAIN, *ZCHAIN;
	double sum,sum1,sum2,sumLogLX,sumLogLZ;
//	double sum4;
	double SLICE_W = *WW;
	int SLICE_P = *PP;
	int kpnp=(*nrowX)+(*ncolX)-1;
	int kpnq=(*NS);
	XTRUE3     = (double *) malloc( (((*nrowX)+(*ncolX))*(*NS))*sizeof(double));
	rmatrix     = (double *) malloc( (((*nrowX)+(*ncolX))*(*NS))*sizeof(double));
	XTEMP     = (double *) malloc( (((*nrowX)+(*ncolX))*(*NS))*sizeof(double));
	XCHAIN     = (double *) malloc( (((*nrowX)+(*ncolX))*(*NS))*sizeof(double));
	ZCHAIN     = (double *) malloc( (((*nrowX)+(*ncolX))*(*NS))*sizeof(double));

//
//	INITIALIZE XCHAIN AND ZCHAIN AND SAVE XCOORDS AND ZCOORDS
//	NDIM <- (nrowX+ncolX)*NS - NS + 1 -- Last Individual Set at
//	Origin.  The "+ 1" is the variance term.
//
	for(i=0;i<((*NS)*(*ncolX));i++)
	{
		ZCHAIN[i]=theta[i];
	}
//
	for(i=0;i<((*NS)*((*nrowX)-1));i++)
	{
		XCHAIN[i]=theta[i+((*NS)*(*ncolX))];
	}
//
//  SIMPLE METHOD OF STOPPING EXECUTION
//
	sum=keithrules111(XCHAIN,ZCHAIN);
//
//	sum4=keithrules100(XCHAIN,ZCHAIN);
//		jp = fopen("heck2.txt","a");
//		fprintf(jp,"BEGINNING OF SLICEUNFOLDING %5d %5d %5d %5d %20.8f %20.8f\n",kpnp,kpnq,*nburn,*nslice,sum,sum4);
//		fprintf(jp,"Z %10.8f %10.8f %10.8f %10.8f %10.8f %10.8f\n",ZCHAIN[0],ZCHAIN[1],ZCHAIN[2],ZCHAIN[3],ZCHAIN[4],ZCHAIN[5]);
//		fprintf(jp,"Z %10.8f %10.8f %10.8f %10.8f %10.8f %10.8f\n",ZCHAIN[6],ZCHAIN[7],ZCHAIN[8],ZCHAIN[9],ZCHAIN[10],ZCHAIN[11]);
//		fprintf(jp,"X %10.8f %10.8f %10.8f %10.8f %10.8f %10.8f\n",XCHAIN[0],XCHAIN[1],XCHAIN[2],XCHAIN[3],XCHAIN[4],XCHAIN[5]);
//		fprintf(jp,"X %10.8f %10.8f %10.8f %10.8f %10.8f %10.8f\n",XCHAIN[2776],XCHAIN[2777],XCHAIN[2778],XCHAIN[2779],XCHAIN[2780],XCHAIN[2781]);
//		fclose(jp);
	kki=0;
	for(i2011=0;i2011<((*nburn)+(*nslice));i2011++)
	{
		sumLogLX=0.0;
/*
          INDIVIDUALS FIRST
          NOTE THAT THE LAST INDIVIDUAL IS SET AT THE ORIGIN
*/
		kk=0;
		for(i=0;i<((*nrowX)-1);i++)
		{
// TRANSFER DISTANCES INTO TEMP VECTOR AND CHECK FOR MISSING
// DATA
			kkk=0;
			for(jj=0;jj<(*ncolX);jj++)
			{
				XTEMP[jj]=X[i*(*ncolX)+jj];
				if(XTEMP[jj] < 0.0)kkk=kkk+1;
			}
//  INCREMENT kk HERE BECAUSE OF MISSING DATA LOOP
//
			for(jj=0;jj<(*NS);jj++)
			{
				kk=kk+1;
				theta2[jj]=0.0;
				theta[jj]=0.0;
			}
			if(kkk <= *NMISSING)
			{
				for(jj=0;jj<(*NS);jj++)
				{
					theta[jj]=XCHAIN[i*(*NS)+jj];
					theta2[jj]=theta[jj];
				}
				for(jj=0;jj<(*NS);jj++)
				{
					theta2[jj] = sliceX(theta2,thetaLeft,thetaRight,jj,SLICE_W,SLICE_P, XTEMP, ZCHAIN);
					theta[jj]=theta2[jj];
					XCHAIN[i*(*NS)+jj]=theta2[jj];
				}
				sum=keithrules222(theta2,XTEMP,ZCHAIN,i);
				sumLogLX=sumLogLX+sum;
			}

		}
  /*
END OF SLICE SAMPLER LOOP ON INDIVIDUALS
*/
		sumLogLZ=0.0;
  /*
  NOW RUN SLICE ON THE STIMULI
*/
		kk=0;
		for(i=0;i<(*ncolX);i++)
		{
// TRANSFER DISTANCES INTO TEMP VECTOR AND CHECK FOR MISSING
// DATA
				kkk=0;
				for(jj=0;jj<(*nrowX);jj++)
				{
					XTEMP[jj]=X[jj*(*ncolX)+i];
					if(XTEMP[jj] < 0.0)kkk=kkk+1;
				}
//
// MISSING DATA LOOP -- AT LEAST 200 RESPONDENTS TO SCALE THE STIMULUS
//
//  INCREMENT kk HERE BECAUSE OF MISSING DATA LOOP
//
			for(jj=0;jj<(*NS);jj++)
			{
				kk=kk+1;
				theta2[jj]=0.0;
				theta[jj]=0.0;
			}
//
//  At Least 1/4 of the data in a column must be present for the
//  scaling
//
			if(kkk <= ((3*(*nrowX))/4))
			{
					for(jj=0;jj<(*NS);jj++)
					{
						theta[jj]=ZCHAIN[i*(*NS)+jj];
						theta2[jj]=theta[jj];
					}
					for(jj=0;jj<(*NS);jj++)
					{
						theta2[jj] = sliceZ(theta2,thetaLeft,thetaRight,jj,SLICE_W,SLICE_P,XTEMP,XCHAIN);
						theta[jj]=theta2[jj];
						ZCHAIN[i*(*NS)+jj]=theta2[jj];
					}
			}
			sum=0.0;
			sum=keithrules333(theta2,XTEMP,XCHAIN,i);
//			fprintf(jp,"%20.8f\n",sum);
			sumLogLZ=sumLogLZ+sum;

		}
//		jp = fopen("heck2.txt","a");
//		fprintf(jp," Sum of Log-likelihood X Phase and Z phase %7d %20.8f %20.8f\n",i2011, sumLogLX, sumLogLZ);
//  fclose(jp);
  Rprintf(" Sum of Log-likelihood X Phase and Z phase %7d %20.8f %20.8f\n",i2011, sumLogLX, sumLogLZ);
//
// CALL ORTHOGONAL PROCRUSTES ROTATION HERE -- ZCOORDS/XCOORDS (XTRUE) ARE THE TARGETS,
// ZCHAIN/XCHAIN (XCOORDS) IS THE MATRIX TO BE ROTATED, RMATRIX IS THE ROTATION MATRIX
//
//
		theta[(*NDIM)-1]=keithrules111(XCHAIN,ZCHAIN);
		theta2[(*NDIM)-1]=theta[(*NDIM)-1];
//	jp = fopen("heck2.txt","a");
//	fprintf(jp," Sigma-hat-Squared %7d %20.8f\n",i2011, theta[(*NDIM)-1]);
//	fclose(jp);
		ssenow[i2011]=keithrules100(XCHAIN,ZCHAIN);
//	jp = fopen("heck2.txt","a");
//	fprintf(jp," Total Loss %7d %20.8f\n",i2011, ssenow[i2011]);
//	fclose(jp);
	ssenow[i2011+((*nslice)+(*nburn))]=keithrules111(XCHAIN,ZCHAIN);
//
/*
  Assemble the Matrices for the Orthogonal Rotation
  FIRST STACK THE STIMULI
*/
//
		kk=0;
		kkx=0;
		for(j=0;j<(*NS)*(*ncolX);j++)
		{
				XCOORDS[j]=0.0;
				XCOORDS[j]=ZCHAIN[kk];
				XTRUE3[kk]=XTRUE[j];
				theta2[kk]=ZCHAIN[kk];
				kk=kk+1;
		}
/*
 NOW STACK THE INDIVIDUALS
*/
		kkx=0;
		for (j=0;j<((*nrowX)*(*NS));j++)
		{
			XCOORDS[j+((*NS)*(*ncolX))]=0.0;
			if(kk<(*NDIM)){
				XCOORDS[j+((*NS)*(*ncolX))]=XCHAIN[kkx];
				XTRUE3[kk]=XTRUE[j+((*NS)*(*ncolX))];
				theta2[kk]=XCHAIN[kkx];
			}
				kk=kk+1;
				kkx=kkx+1;
		}
		for(j=0;j<((*nrowX)+(*ncolX));j++)
		{
		}
//
//
// CALL ORTHOGONAL PROCRUSTES ROTATION HERE -- XTRUE (XTRUE2) ARE THE TARGETS,
// XCOORDS (theta2) IS THE MATRIX TO BE ROTATED, RMATRIX IS THE ROTATION MATRIX
//
//  Calculate SSE Before Rotation
//
		sum1=0.0;
		for (j=0;j<((*NS)*((*nrowX)+(*ncolX)));j++)
		{
			sum1=sum1+pow((XTRUE[j]-XCOORDS[j]),2.0);
		}
//		jp = fopen("heck2.txt","a");
//		fprintf(jp," SSE Before Rotation %7d %7d %7d %20.8f\n",i2011, kpnp, kpnq, sum1);
//		fclose(jp);

/*
   Note that the origin is dropped from XTRUE so kpnp = ncolX+nrowX-1.
   theta2 is length NDIM=(nrowX-1)*NS + 1
*/
		xsvdrotate(kpnp,kpnq,XTRUE3,theta2,rmatrix);
//
		kk=0;
		for(j=0;j<((*NS)*((*nrowX)+(*ncolX)));j++)
		{
			XCOORDS[j]=0.0;
			if(kk<(*NDIM)){
				XCOORDS[j]=theta2[kk];
// PUT ROTATED CONFIGURATION INTO theta
				theta[kk]=theta2[kk];
			}
				kk=kk+1;
		}
//
//  Calculate SSE After Rotation
//
		sum2=0.0;
		for (j=0;j<((*NS)*((*nrowX)+(*ncolX)));j++)
		{
			sum2=sum2+pow((XTRUE[j]-XCOORDS[j]),2.0);
		}
//		jp = fopen("heck2.txt","a");
//		fprintf(jp," SSE After Rotation %7d %20.8f\n",i2011, sum2);
//		fclose(jp);
//		i2012=0;
//		idebug=0;
//		if(i2012==0)idebug=99998;
//		if(idebug==99998)exit(EXIT_FAILURE);
//
		if(i2011 > (*nburn))
		{
			for(j=0;j<(*NDIM);j++)
			{ 
				theta1000[(kki*(*NDIM))+j]=theta2[j];
			}
			kki=kki+1;
//
		}
	}
	free(rmatrix);
	free(XTRUE3);
	free(XTEMP);
	free(XCHAIN);
	free(ZCHAIN);
}


/*

   SLICE SAMPLER FOR INDIVIDUAL COORDINATES
*/

double sliceX(double theta[], double thetaLeft[], double thetaRight[], int param, double w, int p, double XTEMP[], double ZCHAIN[]){

	double x,y,L,R,leftloglike,rightloglike;
	int K;
	short int flag;
	double *temp;
	double rexp = -1*log(runif());
	temp = (double *) malloc((*NDIM)*sizeof(double));

	for(K=0;K<(*NS);K++){
		temp[K]=theta[K];
	}
// w fixed at 1.0, K is fixed at 3
	y = keithrules222(theta,XTEMP,ZCHAIN,param) - rexp;
	L = theta[param] - w*runif();
	if(L<thetaLeft[param])L=thetaLeft[param]+0.000001;
	R = L + w;
	if(R>thetaRight[param]){
		R=thetaRight[param]-0.000001;
		if(L>R)L=thetaLeft[param]+0.000001;
	}
	K = p;

	temp[param] = L;
	leftloglike = keithrules222(temp,XTEMP,ZCHAIN,param);
	temp[param] = R;
	rightloglike = keithrules222(temp,XTEMP,ZCHAIN,param);

	flag=0;
	if(y < leftloglike) flag=1;
	else if(y < rightloglike) flag=-1;

	while(K>0 && flag!=0) {
		if(runif()<0.5) {
			L = 2*L - R;
			if(L<thetaLeft[param])L=thetaLeft[param]+0.000001;
			temp[param] = L;
			if(y >=keithrules222(temp,XTEMP,ZCHAIN,param) && flag==1) flag=0; 
		}
		else {
			R = 2*R -L;
			if(R>thetaRight[param])R=thetaRight[param]-0.000001;
			temp[param] = R;
			if(y >=keithrules222(temp,XTEMP,ZCHAIN,param) && flag==-1) flag=0; 
		}
		K--;   
	}

	while(1){
		x = L + runif()*(R-L);
		if(x>thetaRight[param])x=thetaRight[param]-0.000001;
		if(x<thetaLeft[param])x=thetaLeft[param]+0.000001;
		temp[param] = x;
		if(keithrules222(temp,XTEMP,ZCHAIN,param)>y) break;
		if(theta[param] > x) L = x;
		else R = x;
	} 

	free(temp);
	return(x);

}
// End of SliceX

/*
keithrules22 is called by sliceX -- used in slice sampler for the
individuals (X's)

*/
double keithrules222(double theta[], double XTEMP[], double ZCHAIN[], int param)
{
	int j, jj;
	double sumsquared=0;
	double circledist, circledisthat;
/*
 */
/*
 *NOTE THAT THIS IS NDIM-1 IF SIGMA**2 IS BEING ESTIMATED!!!!
*/
//	i=param/NS;
//	
	for(j=0;j<(*ncolX);j++)
	{
		circledist = XTEMP[j];
/*
CATCH MISSING DATA HERE
*/
		if(circledist > 0.0)
		{
			circledisthat=0.0;
			for(jj=0;jj<(*NS);jj++)
			{
				circledisthat = circledisthat+pow((theta[jj]-ZCHAIN[(*NS)*j+jj]),2.0);
			}
			circledisthat=sqrt(circledisthat);
				   //catch logs of zero
			if(circledisthat<.0001)circledisthat=.001;
			sumsquared=sumsquared+pow((log(circledist)-log(circledisthat)),2.0);
		}
	}
//	
//  SIMPLE SUM OF SQUARED LOG DIFFERENCES
//
//
	return -sumsquared;
}

/*

   SLICE SAMPLER FOR STIMULI COORDINATES
*/
double sliceZ(double theta[], double thetaLeft[], double thetaRight[], int param, double w, int p, double XTEMP[], double XCHAIN[]){

	double x,y,L,R,leftloglike,rightloglike;
	int K;
	short int flag;
	double *temp;
	double rexp = -1*log(runif());
	temp = (double *) malloc((*NDIM)*sizeof(double));

	for(K=0;K<(*NS);K++){
		temp[K]=theta[K];
	}
// w fixed at 1.0, K is fixed at 3
	y = keithrules333(theta,XTEMP,XCHAIN,param) - rexp;
	L = theta[param] - w*runif();
	if(L<thetaLeft[param])L=thetaLeft[param]+0.000001;
	R = L + w;
	if(R>thetaRight[param]){
		R=thetaRight[param]-0.000001;
		if(L>R)L=thetaLeft[param]+0.000001;
	}
	K = p;

	temp[param] = L;
	leftloglike = keithrules333(temp,XTEMP,XCHAIN,param);
	temp[param] = R;
	rightloglike = keithrules333(temp,XTEMP,XCHAIN,param);

	flag=0;
	if(y < leftloglike) flag=1;
	else if(y < rightloglike) flag=-1;

	while(K>0 && flag!=0) {
		if(runif()<0.5) {
			L = 2*L - R;
			if(L<thetaLeft[param])L=thetaLeft[param]+0.000001;
			temp[param] = L;
			if(y >=keithrules333(temp,XTEMP,XCHAIN,param) && flag==1) flag=0; 
		}
		else {
			R = 2*R -L;
			if(R>thetaRight[param])R=thetaRight[param]-0.000001;
			temp[param] = R;
			if(y >=keithrules333(temp,XTEMP,XCHAIN,param) && flag==-1) flag=0; 
		}
		K--;   
	}

	while(1){
		x = L + runif()*(R-L);
		if(x>thetaRight[param])x=thetaRight[param]-0.000001;
		if(x<thetaLeft[param])x=thetaLeft[param]+0.000001;
		temp[param] = x;
		if(keithrules333(temp,XTEMP,XCHAIN,param)>y) break;
		if(theta[param] > x) L = x;
		else R = x;
	} 

	free(temp);
	return(x);

}
//End of Slice

/*

  keithrules333 is called by sliceZ which does the slice sampler for the
  Stimuli

*/
double keithrules333(double theta[], double XTEMP[], double XCHAIN[], int param)
{
	int j, jj;
	double sumsquared=0;
	double circledist, circledisthat;
/*
 */
	for(j=0;j<(*nrowX);j++)
	{
		circledist = XTEMP[j];
/*
CATCH MISSING DATA HERE
*/
		if(circledist > 0.0)
		{
			circledisthat=0.0;
				for(jj=0;jj<(*NS);jj++)
				{
					circledisthat = circledisthat+pow((theta[jj]-XCHAIN[(*NS)*j+jj]),2.0);
				}
				circledisthat=sqrt(circledisthat);
				   //catch logs of zero
				if(circledisthat<.0001)circledisthat=.001;
				sumsquared=sumsquared+pow((log(circledist)-log(circledisthat)),2.0);
		}
	}
//	
//  SIMPLE SUM OF SQUARED ERROR
//
//
	return -sumsquared;
}
//
// Calculates the sigma-hat-squared for the Unfolding Model
//
//
double keithrules111(double XCHAIN[], double ZCHAIN[])
{
	int ii, j, jj;
	double sumsquared=0;
	double circledist, xkk;
/*
 */
	xkk=0.0;
	for(ii=0;ii<(*nrowX);ii++)
	{
		for(j=0;j<(*ncolX);j++)
		{
			if(X[ii*(*ncolX)+j]> 0.0)
			{
				circledist=0.0;
				for(jj=0;jj<(*NS);jj++)
				{
					circledist=circledist+pow((ZCHAIN[(*NS)*j+jj]-XCHAIN[(*NS)*ii+jj]),2.0);
				}
				circledist=sqrt(circledist);
				   //catch logs of zero
				if(circledist<.0001)circledist=.001;
				sumsquared=sumsquared+pow((log(circledist)-log(X[ii*(*ncolX)+j])),2.0);
				xkk=xkk+1;
			}
		}
	}
//
	return sumsquared/xkk;
}
//
// Calculates the full Log-Posterior for the Unfolding Model
//
//
double keithrules100(double XCHAIN[], double ZCHAIN[])
{
	int i, j, jj, kk;
	double sumsquared=0;
	double circledist, circledisthat;
	double *ZCOORDSD, *XCOORDSD;
	ZCOORDSD     = calloc( (((*nrowX)+(*ncolX))*(*NS)), sizeof(double));
	XCOORDSD     = calloc( (((*nrowX)+(*ncolX))*(*NS)), sizeof(double));
/*
 */
//
//	kp = fopen("heck4.txt","a");
	kk=0;
//
	for(j=0;j<((*NS)*(*ncolX));j++)
	{
			ZCOORDSD[j]=0.0;
			ZCOORDSD[j]=ZCHAIN[kk];
			kk=kk+1;
	}
	kk=0;
	for(j=0;j<((*NS)*(*nrowX));j++)
	{
			XCOORDSD[j]=0.0;
		if(kk < (*NDIM)){
			XCOORDSD[j]=XCHAIN[kk];
		}
			kk=kk+1;
	}
	for(i=0;i<(*nrowX);i++)
	{
		for(j=0;j<(*ncolX);j++)
		{
			circledist = X[i*(*ncolX)+j];
/*
CATCH MISSING DATA HERE
*/
			if(circledist > 0.0)
			{
				circledisthat=0.0;
				for(jj=0;jj<(*NS);jj++)
				{
					circledisthat = circledisthat+pow((XCOORDSD[(*NS)*i+jj]-ZCOORDSD[(*NS)*j+jj]),2.0);
				}
				circledisthat=sqrt(circledisthat);
				   //catch logs of zero
				if(circledisthat<.0001)circledisthat=.001;
				sumsquared=sumsquared+pow((log(circledist)-log(circledisthat)),2.0);
			}
		}
	}
	free(ZCOORDSD);
	free(XCOORDSD);
	return -sumsquared;
}
