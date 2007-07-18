#include "HsSVN.h"
#include <stdlib.h>

/* initialization ************************************************************ */
int HsSVN_initialize() {
    apr_status_t st;

    st = apr_initialize();

    if (st) {
        return -1;
    }

    if (atexit(apr_terminate)) {
        return -1;
    }

    return 0;
}

/* pools ********************************************************************* */
apr_pool_t* HsSVN_svn_pool_create(apr_pool_t* parent) {
    return svn_pool_create(parent);
}

void HsSVN_svn_pool_destroy(apr_pool_t* pool) {
    svn_pool_destroy(pool);
}
