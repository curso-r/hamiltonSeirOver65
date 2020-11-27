#include "twoagesR.h"
#include <R_ext/Rdynload.h>

void R_init_hamiltonSeirOver65(DllInfo *info) {
  R_RegisterCCallable("hamiltonSeirOver65", "twoages",  (DL_FUNC) &twoages_);
}