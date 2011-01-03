#include <math.h>
#include <stdio.h>
#include <sundials/sundials_nvector.h>
#include <nvector/nvector_serial.h>
#include <ida/ida.h>
#include <ida/ida_spgmr.h>
#include <ida/ida_dense.h>
#include <lm.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

static int modelResiduals(double t, double* y, double* derivy, double* resids);
static void boundaryAssignment(double t, double* v, double* dv);
static int boundaryResiduals(double t, double* params, double* res);
static int modelRoots(double t, N_Vector y, N_Vector derivy, realtype *gout, void *user_data);
static void translateParams(N_Vector y, N_Vector derivy, double* params);
static void reverseTranslateParams(N_Vector y, N_Vector derivy, double* params);
static int gNumVars, gNumInterventions, gNumEquations, gNumBoundaryEquations, gNumParams;
static void setupIdVector(N_Vector id);
static int checkConditions(double t, N_Vector y, N_Vector derivy);

static double max(double x, double y) { if (x > y) return x; else return y; }
static double min(double x, double y) { if (x < y) return x; else return y; }

static int imax(int x, int y) { if (x > y) return x; else return y; }
static int modelJacobian(double, double*, double*, double, double**);

struct Overrides {
  int howMany, howManyAlloc;
  unsigned int * whichVariable, * whichVarParam, * whichDerivParam;
  double* whatValue;
};

static int
modelResidualsForIDA(double t, N_Vector y, N_Vector derivy, N_Vector resids, void* user_data)
{
  int i, j;
  struct Overrides* overrides;
  double* res = N_VGetArrayPointer(resids),
        * v = N_VGetArrayPointer(y),
        * dv = N_VGetArrayPointer(derivy);
  overrides = user_data;

  //printf("Warning (0, \"\", \"\", \"Computing model residuals at:\"),\n%g,", t);
  //for (i = 0; i < gNumVars; i++)
  //{
  //  printf("%g,%g,", v[i], dv[i]);
  //}
  //printf("\n");

  modelResiduals(t, v, dv, res);

  i = gNumEquations;
  for (j = 0; j < overrides->howMany; j++, i++)
    res[i] = dv[overrides->whichVariable[j]];

  //printf("Warning (0, \"\", \"\", \"Computed model residuals.\"),\n");
  for (i = 0; i < gNumVars; i++)
  {
    //printf("%g,", res[i]);
    if (!isfinite(res[i]))
    {
      printf("Warning (0, \"\", \"\", \"Residual %d not finite at t=%g\"),\n",
             i, t);
      return 1;
    }
    if (fabs(res[i]) > 1E100)
    {
      printf("Warning (0, \"\", \"\", \"Residual %d has large magnitude at t=%g\"),\n",
             i, t);
    }
  }
  //printf("\n");

  return 0;
}

static int
modelJacobianForIDA(int Neq, double t, realtype cj, N_Vector yy, N_Vector yp,
                    N_Vector rr, DlsMat Jac, void *user_data, N_Vector tmp1,
                    N_Vector tmp2, N_Vector tmp3)
{
  int i, j, k;
  struct Overrides* overrides;
  double* v = N_VGetArrayPointer(yy),
    * dv = N_VGetArrayPointer(yp);
  overrides = user_data;
  //printf("Warning (0, \"\", \"\", \"Computed model Jacobian.\"),\n");
  modelJacobian(t, v, dv, cj, Jac->cols);

  i = gNumEquations;
  for (j = 0; j < overrides->howMany; j++, i++)
  {
    for (k = 0; k < Neq; k++)
      Jac->cols[k][i] = 0;
    Jac->cols[overrides->whichVariable[j]][i] = cj;
  }

  for (i = 0; i < Neq; i++)
  {
    for (j = 0; j < Neq; j++)
    {
      //printf("%g,", Jac->cols[j][i]);
      if (!finite(Jac->cols[i][j]) || fabs(Jac->cols[i][j]) > 1E30)
      {
        printf("Warning (0, \"\", \"\", \"Jac_(%d,%d) is not finite!\"),\n", i, j);
        Jac->cols[i][j] = 0;
#if 0
        switch (isinf(Jac->cols[i][j]))
        {
        case 1:
          Jac->cols[i][j] = 1E100;
          break;
        case -1:
          Jac->cols[i][j] = -1E100;
          break;
        default:
          // NaN... we don't know what way to go so do a random perturbation...
          Jac->cols[i][j] = 1E100; // 2E30 * ((double)random()) / ((double)RAND_MAX) - 1E30;
          break;
        }
#endif
      }
#if 0
      else if (Jac->cols[i][j] > 1E30)
        Jac->cols[i][j] = 1E30;
      else if (Jac->cols[i][j] < -1E30)
        Jac->cols[i][j] = -1E30;
#endif
    }
    //printf("\n");
  }

  return 0;
}

static char* quote(const char* msg)
{
  const char* p;
  int l;
  for (p = msg; *p; p++)
    if (*p == '\\' || *p == '\n' || *p == '"')
      l += 2;
    else
      l++;

  char* news = malloc(l + 1);
  char* newp = news;
  for (p = msg; *p; p++)
    if (*p == '\\' || *p == '\n' || *p == '"')
    {
      *newp++ = '\\';
      if (*p == '\n')
        *newp++ = 'n';
      else
        *newp++ = *p;
    }
    else
      *newp++ = *p;
  *newp++ = 0;
  return news;
}
static void checkedConditionFail(const char* msg)
{
  printf("CheckedConditionFail \"%s\"\n]\n", quote(msg));
  exit(3);
}

static void
handle_error(int err_code, const char* module, const char* function, char* msg, void* user_data)
{
  if (err_code >= 0)
  {
    printf("Warning (%u, \"%s\", \"%s\", \"%s\"),\n", err_code, module, function, msg);
    return;
  }

  printf("FatalError (%u, \"%s\", \"%s\", \"%s\")\n]\n", err_code, module, function, msg);
  exit(1);
}

static void
show_results(double t, N_Vector y, N_Vector derivy)
{
  double* v = N_VGetArrayPointer(y),
        * dv = N_VGetArrayPointer(derivy);
  int i;
  printf("Result (%g, [", t);
  for (i = 0; i < gNumVars; i++)
  {
    if (i > 0)
      printf(",");
    printf("%g", v[i]);
  }
  printf("], [");
  for (i = 0; i < gNumVars; i++)
  {
    if (i > 0)
      printf(",");
    printf("%.16g", dv[i]);
  }
  printf("]),\n");
}

double gtStart;

static void
iv_sys_fn(double *p, double *hx, int m, int n, void* dat)
{
  int i, j;
  struct Overrides* overrides = dat;

  boundaryResiduals(gtStart, p, hx);

  i = gNumEquations + gNumBoundaryEquations;
  for (j = 0; j < overrides->howMany; j++)
  {
    hx[i++] = p[overrides->whichVarParam[j]] - overrides->whatValue[j];
    if (overrides->whichDerivParam[j] == -1)
      hx[i++] = 0;
    else
      hx[i++] = p[overrides->whichDerivParam[j]];
  }

  // Saturate numbers to help numerical solver with NaNs etc...
  for (i = 0; i < m; i++)
    if (!isfinite(hx[i]) || hx[i] > 1E30)
      hx[i] = 1E30;
    else if (hx[i] < -1E30)
      hx[i] = -1E30;
}

static void
setup_parameters
(
 double t,
 N_Vector y, N_Vector yp,
 double* params,
 struct Overrides* overrides
)
{
  int ret, i;
  double* v = N_VGetArrayPointer(y),
        * dv = N_VGetArrayPointer(yp);
  
  // sleep(20);

  // Apply overrides...
  for (i = 0; i < overrides->howMany; i++)
  {
    v[overrides->whichVariable[i]] = overrides->whatValue[i];
    dv[overrides->whichVariable[i]] = 0;
  }
  boundaryAssignment(t, v, dv);

#ifdef DEBUG_PARAMFINDER
  /* Check that the boundary residual solver found a solution to the residuals... */
  {
    int i;
    double err;
    char buf[40];
    double* tresids = malloc(sizeof(double) * gNumParams);
    modelResiduals(t, v, dv, tresids);
    for (i = 0; i < gNumParams; i++)
    {
      snprintf(buf, 40, "Pre-solve residual error %d: %g", i, tresids[i]);
      handle_error(0, "debug_paramfinder", "setup_parameters", buf, NULL);
      err += tresids[i];
    }
    snprintf(buf, 40, "Pre-solve total residual error: %g", err);
    handle_error(0, "debug_paramfinder", "setup_parameters", buf, NULL);
    free(tresids);
  }
#endif

  reverseTranslateParams(y, yp, params);

  if (gNumParams > gNumEquations + gNumBoundaryEquations + overrides->howMany * 2)
  {
    printf("FatalError (0, \"Initial value solver\", \"model check\", \"More parameters at initial value than constraints available; unable to solve model: %d parameters, %d equations, %d boundaryEquations, %d overrides\")]\n",
           gNumParams, gNumEquations, gNumBoundaryEquations, overrides->howMany);
    exit(0);
  }

  double opts[5] = { 1E-03, 1E-17, 1E-240, 1E-17, 1E-17};
  while (1)
  {
    double info[10];
    ret = dlevmar_dif(iv_sys_fn, params, NULL, gNumParams,
                      gNumEquations + gNumBoundaryEquations + overrides->howMany * 2,
                      1000000, opts, info, NULL, NULL, overrides);
    if (info[6] != 4 && info[6] != 5)
      break;

    opts[0] *= 2;
  }
  if (ret < 0)
    handle_error(0, "Initial value solver", "Solve failed", "Couldn't find initial conditions.", NULL);

  translateParams(y, yp, params);

#ifdef DEBUG_PARAMFINDER
  /* Check that the boundary residual solver found a solution to the residuals... */
  {
    int i;
    double err;
    char buf[40];
    double* tresids = malloc(sizeof(double) * gNumParams);
    modelResiduals(t, v, dv, tresids);
    for (i = 0; i < gNumParams; i++)
      err += tresids[i];
    snprintf(buf, 40, "Total residual error: %g", err);
    handle_error(0, "debug_paramfinder", "setup_parameters", buf, NULL);
    free(tresids);
  }
#endif
}

static void
do_ida_solve(double tStart, double tMaxSolverStep, double tMaxReportStep, double tEnd, int everyStep,
             double reltol, double abstol, struct Overrides* overrides)
{
  int i;
  void *ida_mem = IDACreate();
  N_Vector y = N_VNew_Serial(gNumVars);
  N_Vector yp = N_VNew_Serial(gNumVars);
  N_VConst(0.1, y);
  N_VConst(0, yp);

  double * v = N_VGetArrayPointer(y), * dv = N_VGetArrayPointer(yp);

  gtStart = tStart;

  double * params = malloc(gNumVars * 2 * sizeof(double));
  setup_parameters(tStart, y, yp, params, overrides);
  
  IDAInit(ida_mem, modelResidualsForIDA, tStart, y, yp);
  IDASStolerances(ida_mem, reltol, abstol);
  // IDASpgmr(ida_mem, 0);
  IDADense(ida_mem, imax(gNumVars, gNumEquations));
  IDADlsSetDenseJacFn(ida_mem, modelJacobianForIDA);
  IDASetErrHandlerFn(ida_mem, handle_error, NULL);
  IDASetNoInactiveRootWarn(ida_mem);
  IDASetMaxStep(ida_mem, tMaxSolverStep);
  IDASetStopTime(ida_mem, tEnd);
  IDASetMaxNumSteps(ida_mem, -1);
  IDASetUserData(ida_mem, overrides);

  N_Vector idv = N_VNew_Serial(imax(gNumVars, gNumEquations));
  setupIdVector(idv);
  {
    int i;
    double *r = N_VGetArrayPointer(idv);
    for (i = 0; i < overrides->howMany; i++)
      r[overrides->whichVariable[i]] = 1.0;
  }
  IDASetId(ida_mem, idv);
  N_VDestroy(idv);

  // IDACalcIC(ida_mem, IDA_YA_YDP_INIT, tStart + tMaxReportStep);
  IDARootInit(ida_mem, gNumInterventions, modelRoots);

  show_results(tStart, y, yp);

  double tnext = tStart;
  while (1)
  {
    tnext += tMaxReportStep;
    int ret = IDASolve(ida_mem, tnext, &tnext, y, yp, everyStep ? IDA_ONE_STEP : IDA_NORMAL);
    show_results(tnext, y, yp);
    checkConditions(tnext, y, yp);
    if (ret == IDA_TSTOP_RETURN)
      break;
    if (ret == IDA_ROOT_RETURN)
    {
      setup_parameters(tnext, y, yp, params, overrides);
    }
  }

  IDAFree(&ida_mem);
  free(params);

  printf("Success");
}

static double smax(double x, double y)
{
  // return (x /* + y * y */);
  return ((x >= 0.0) ? 1.0 : -1.0) * ((y >= 0.0) ? 1.0 : -1.0) * max(fabs(x), fabs(y));
}

void
skipwhitespace()
{
  int c;
  while (1)
  {
    c = getc(stdin);
    if (c == ' ' || c == '\t' || c == '\r' || c == '\n')
      continue;
    ungetc(c, stdin);
    break;
  }
}

void
skiptowhitespace()
{
  int c;
  while (1)
  {
    c = getc(stdin);
    if (c == -1)
      return;

    if (c == ' ' || c == '\t' || c == '\r' || c == '\n')
    {
      ungetc(c, stdin);
      break;
    }
  }
}


/* Note: We don't deallocate this, since it survives until program exit. */
void
allocOverrides(struct Overrides* aIV)
{
  aIV->howMany = 0;
  aIV->howManyAlloc = 4;
  aIV->whichVariable = malloc(4 * sizeof(unsigned int));
  aIV->whichVarParam = malloc(4 * sizeof(unsigned int));
  aIV->whichDerivParam = malloc(4 * sizeof(unsigned int));
  aIV->whatValue = malloc(4 * sizeof(double));
}

void
addOverride(struct Overrides* aIV, uint32_t whichVariable, uint32_t whichVarParam, uint32_t whichDerivParam,
            double what)
{
  if (aIV->howManyAlloc <= aIV->howMany)
  {
    aIV->howManyAlloc *= 2;
    aIV->whichVariable = realloc(aIV->whichVariable, aIV->howManyAlloc * sizeof(unsigned int));
    aIV->whichVarParam = realloc(aIV->whichVarParam, aIV->howManyAlloc * sizeof(unsigned int));
    aIV->whichDerivParam = realloc(aIV->whichDerivParam, aIV->howManyAlloc * sizeof(unsigned int));
    aIV->whatValue = realloc(aIV->whatValue, aIV->howManyAlloc * sizeof(double));
  }
  aIV->whichVariable[aIV->howMany] = whichVariable;
  aIV->whichVarParam[aIV->howMany] = whichVarParam;
  aIV->whichDerivParam[aIV->howMany] = whichDerivParam;
  aIV->whatValue[aIV->howMany++] = what;
}

/*
 * Now the main solver...
 */
int
main(int argc, char** argv)
{
  int c, needcomma = 0;
  struct Overrides overrides;

  allocOverrides(&overrides);

  if (getc(stdin) != '[')
  {
    printf("Input should be: [(tStart, maxSolverStep, maxReportStep, tEnd, showEveryStep, reltol, abstol, [(overrideVariableNo, overrideValue)...])...]");
    return 2;
  }

  printf("[\n");
  while (1)
  {
    double tStart, maxSolverStep, maxReportStep, tEnd;
    double showEveryStep;
    double reltol, abstol;

    overrides.howMany = 0;

    skipwhitespace();
    c = getc(stdin);
    if (c == ',')
      continue;
    if (c == ']')
      break;

    if (c != 'S')
    {
      printf("Expected SolverParameters at start of parameter list");
      return 2;
    }
    skiptowhitespace();
    skipwhitespace();

    if ((c = fscanf(stdin, "{ tStart = %lg , maxSolverStep = %lg , maxReportStep = %lg , tEnd = %lg , showEveryStep = %lg , reltol = %lg , abstol = %lg , variableOverrides = [ ", &tStart, &maxSolverStep, &maxReportStep, &tEnd,
                    &showEveryStep, &reltol, &abstol)) != 7)
    {
      fprintf(stderr, "SolverParameters malformed - %d read\n", c);
      return 2;
    }
    while (1)
    {
      unsigned int whichVariable, whichVarParam, whichDerivParam;
      double what;
      skipwhitespace();
      c = getc(stdin);
      if (c == ',')
        continue;
      if (c == ']')
        break;
      
      fscanf(stdin, "( %u, %u, %u ) , %lg )", &whichVariable, &whichVarParam, &whichDerivParam, &what);
      addOverride(&overrides, whichVariable, whichVarParam, whichDerivParam, what);
    }
    skipwhitespace();
    if (getc(stdin) != '}')
    {
      printf("Expected }\n");
      return 2;
    }
    skipwhitespace();

    if (needcomma == 0)
      needcomma = 1;
    else
      printf(",\n");

    do_ida_solve(tStart, maxSolverStep, maxReportStep, tEnd, (int)showEveryStep,
                 reltol, abstol, &overrides);
  }
  printf("\n]\n");
  return 0;
}
