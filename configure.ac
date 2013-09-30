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

# apr
AC_ARG_VAR([APR_CONFIG], [apr-config to use])
AC_CHECK_PROGS(
        [APR_CONFIG],
        [apr-config apr-1-config],
        [AC_MSG_ERROR([apr-config is required. Hint: APR_CONFIG])])
SVN_CFLAGS="$SVN_CFLAGS "`$APR_CONFIG --includes --cppflags`
SVN_LIBS="$SVN_LIBS "`$APR_CONFIG --link-ld --libs`

# apu
AC_ARG_VAR([APU_CONFIG], [apu-config to use])
AC_CHECK_PROGS(
        [APU_CONFIG],
        [apu-config apu-1-config],
        [AC_MSG_ERROR([apu-config is required. Hint: APU_CONFIG])])
SVN_CFLAGS="$SVN_CFLAGS "`$APU_CONFIG --includes`
SVN_LIBS="$SVN_LIBS "`$APU_CONFIG --link-ld --libs`

# svn
AC_ARG_WITH(
    [subversion-prefix],
    [AS_HELP_STRING(
         [--with-subversion-prefix=PREFIX],
         [path to subversion headers and libraries @<:@PREFIX@:>@])])
AS_CASE(
    ["$with_subversion_prefix"],
    ["yes"|"no"|""], [with_subversion_prefix="$prefix"])
SVN_CFLAGS="$SVN_CFLAGS -I$with_subversion_prefix/include/subversion-1"
SVN_LIBS=" \
 $SVN_LIBS \
 -L$with_subversion_prefix/lib \
 -Wl,-rpath -Wl,$with_subversion_prefix/lib \
 -lsvn_subr-1 -lsvn_repos-1 -lsvn_fs-1"
AC_SUBST([SVN_CFLAGS])
AC_SUBST([SVN_LIBS])

AC_ARG_VAR([EXTRA_CPPFLAGS], [(deprecated)])
AC_ARG_VAR([EXTRA_LDFLAGS],  [(deprecated)])
SVN_CFLAGS="$SVN_CFLAGS $EXTRA_CPPFLAGS"
SVN_LIBS="$SVN_LIBS $EXTRA_LDFLAGS"

ac_old_CPPFLAGS="$CPPFLAGS"
ac_old_LDFLAGS="$LDFLAGS"
CPPFLAGS="$CPPFLAGS $SVN_CFLAGS"
LDFLAGS="$LDFLAGS $SVN_LDFLAGS"

AC_CHECK_HEADERS(
        [svn_version.h],
        [],
        [AC_MSG_ERROR([SVN headers are required. Hint: --with-subversion-prefix=PREFIX])])

AC_CHECK_LIB(
        [svn_subr-1],
        [svn_subr_version],
        [],
        [AC_MSG_ERROR([SVN libraries are required. Hint: --with-subversion-prefix=PREFIX])])

CPPFLAGS="$ac_old_CPPFLAGS"
LDFLAGS="$ac_old_LDFLAGS"
#
# Done the ugly checks
#

AC_CONFIG_FILES([
    HsSVN.buildinfo
])
AC_OUTPUT