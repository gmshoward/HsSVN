#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.68])
AC_INIT([HsSVN], [], [pho@cielonegro.org])
AC_CONFIG_SRCDIR([HsSVN.cabal])

# Checks for programs.
AC_PROG_CC

# Checks for header files.
AC_CHECK_HEADERS([stdlib.h])

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_CHECK_FUNCS([atexit])

#
# Checks for Subversion...
#
# NOTE: we can get rid of these ugly checks when we got svn-config or
# something alike.
#
ac_old_CPPFLAGS="$CPPFLAGS"
ac_old_LDFLAGS="$LDFLAGS"

AC_ARG_VAR([EXTRA_CPPFLAGS], [extra CPPFLAGS to find subversion])
AC_ARG_VAR([EXTRA_LDFLAGS], [extra LDFLAGS to find subversion])
CPPFLAGS="$EXTRA_CPPFLAGS $CPPFLAGS"
LDFLAGS="$EXTRA_LDFLAGS $LDFLAGS"

SVN_CFLAGS="$EXTRA_CPPFLAGS"
SVN_LIBS="$EXTRA_LIBS -lsvn_subr-1 -lsvn_repos-1 -lsvn_fs-1"
AC_SUBST([SVN_CFLAGS])
AC_SUBST([SVN_LIBS])

# apr
AC_ARG_VAR([APR_CONFIG], [apr-config to use])
AC_CHECK_PROGS(
        [APR_CONFIG],
        [apr-config apr-1-config],
        [AC_MSG_ERROR([apr-config is required. Hint: APR_CONFIG])])
SVN_CFLAGS=`$APR_CONFIG --includes --cppflags`" $SVN_CFLAGS"
SVN_LIBS=`$APR_CONFIG --link-ld --libs`" $SVN_LIBS"

CPPFLAGS=`$APR_CONFIG --includes --cppflags`" $CPPFLAGS"
LDFLAGS=`$APR_CONFIG --link-ld --libs`" $LDFLAGS"

# apu
AC_ARG_VAR([APU_CONFIG], [apu-config to use])
AC_CHECK_PROGS(
        [APU_CONFIG],
        [apu-config apu-1-config],
        [AC_MSG_ERROR([apu-config is required. Hint: APU_CONFIG])])
SVN_CFLAGS=`$APU_CONFIG --includes`" $SVN_CFLAGS"
SVN_LIBS=`$APU_CONFIG --link-ld --libs`" $SVN_LIBS"

CPPFLAGS=`$APU_CONFIG --includes`" $CPPFLAGS"
LDFLAGS=`$APU_CONFIG --link-ld --libs`" $LDFLAGS"

# svn
AC_CHECK_HEADERS(
        [svn_error.h],
        [],
        [AC_MSG_ERROR([SVN headers are required. Hint: EXTRA_CPPFLAGS])])

AC_CHECK_LIB(
        [svn_subr-1],
        [svn_error_clear],
        [],
        [AC_MSG_ERROR([SVN libraries are required. Hint: EXTRA_LDFLAGS])])
#
# Done the ugly checks
#

AC_CONFIG_FILES([
    HsSVN.buildinfo
])
AC_OUTPUT